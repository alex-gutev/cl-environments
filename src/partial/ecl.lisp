;;;; ecl.lisp
;;;;
;;;; Copyright 2021 Alexander Gutev
;;;;
;;;; Permission is hereby granted, free of charge, to any person
;;;; obtaining a copy of this software and associated documentation
;;;; files (the "Software"), to deal in the Software without
;;;; restriction, including without limitation the rights to use,
;;;; copy, modify, merge, publish, distribute, sublicense, and/or sell
;;;; copies of the Software, and to permit persons to whom the
;;;; Software is furnished to do so, subject to the following
;;;; conditions:
;;;;
;;;; The above copyright notice and this permission notice shall be
;;;; included in all copies or substantial portions of the Software.
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;;; OTHER DEALINGS IN THE SOFTWARE.

;;;; ECL specific implementation of the CLTL2 API

(in-package :cl-environments.cltl2)


;;; Variable Information

(defun variable-information (variable &optional env)
  "Return information about the variable VARIABLE in the environment ENV."

  (let ((extra (variable-binding variable (get-environment env)))
	(env (or env c::*cmp-env-root*)))

    (multiple-value-bind (type local decl)
	(native-var-information variable env)

      (values type local (append decl extra)))))

(defun native-var-information (variable env)
  (acond
    ((c::cmp-env-search-var variable env)
     (if (c::var-p it)
	 (values
	  (var-kind it)
	  t
	  (list
	   (cons 'type (c::var-type it))
	   (cons 'ignore (var-ignorable? it))))

	 (values
	  (local-var-kind variable env)
	  t
	  (list
	   (cons 'type (c::variable-type-in-env variable env))))))

    ((c::cmp-env-search-symbol-macro variable env)
     (values
      :symbol-macro
      t
      nil))

    ((nth-value 1 (macroexpand variable env))
     (values
      :symbol-macro
      nil
      nil))

    ((constantp variable env)
     (global-var-info :constant variable env))

    ((sys:specialp variable)
     (global-var-info :special variable env))

    (t
     (values nil nil nil))))

(defun local-var-kind (var env)
  "Determine the kind (lexical, special, etc) of a local variable in
   an environment."

  (let ((spec (find-if
	      (lambda (spec)
		(match spec
		  ((list* (eql var) _) t)))

	      (c::cmp-env-variables env))))

    (match spec
      ((list* _ 'c::special _) :special)
      ((list* _ 'si::symbol-macro) :symbol-macro)
      (nil (si:specialp var))
      (_ :lexical))))

(defun var-kind (var)
  (acase (c::var-kind var)
    (c::lexical :lexical)
    (c::special :special)
    (otherwise it)))

(defun global-var-info (kind var env)
  (values
   kind
   nil
   (acons 'type (c::variable-type-in-env var env) nil)))

;; From HU.DWIM.WALKER
(defun var-ignorable? (var)
  #.(cl:if (cl:< ext:+ecl-version-number+ 100701)
           `(< (c::var-ref var) 0)
           `(and (c::var-ignorable var)
                 (< (c::var-ignorable var) 0))))


;;; Function Information

(defun function-information (function &optional env)
  "Return information about the function FUNCTION in the environment ENV."

  (let ((extra (function-binding function (get-environment env)))
	(env (or env c::*cmp-env-root*)))

    (multiple-value-bind (type local decl)
	(native-function-information function env)

      (values type local (append decl extra)))))

(defun native-function-information (function env)
  (cond
    ((and (symbolp function)
	  (special-operator-p function))

     (values :special-form nil nil))

    ((c::cmp-env-search-macro function env)
     (values :macro t	nil))

    ((c::cmp-env-search-function function env)
     (function-info function t env))

    ((macro-function function env)
     (values :macro nil nil))

    ((fboundp function)
     (function-info function nil env))

    (t
     (values nil nil nil))))

(defun function-info (function local env)
  (values
   :function
   local

   (list
    (cons 'inline (inlinep function env))
    (cons 'ftype (cons 'function (get-ftype function env))))))

(defun inlinep (function env)
  (cond
    ((c::declared-inline-p function env) 'inline)
    ((c::declared-notinline-p function env) 'notinline)))

(defun get-ftype (function env)
  (multiple-value-bind (arg-types got-arg-types-p)
      (c::get-arg-types function env)

    (multiple-value-bind (return-type got-return-type-p)
	(c::get-return-type function env)

      (when (or got-arg-types-p got-return-type-p)
	`(,(if got-arg-types-p
	       arg-types
	       '*)

	   ,(if got-return-type-p
		return-type
		'*))))))


;;; Declaration Information

(defun declaration-information (name &optional env)
  "Return information about the declaration DECL-NAME in the environment ENV."

  (case name
    (optimize
     (destructuring-bind (&optional (debug 1) (safety 1) (space 1) (speed 1) &rest others)
	 (c::cmp-env-all-optimizations env)

       (declare (ignore others))

       ;; 1 is always returned for COMPILATION-SPEED since ECL doesn't
       ;; record it.

       `((debug ,debug)
	 (safety ,safety)
	 (space ,space)
	 (speed ,speed)
	 (compilation-speed 1))))

    (declaration
     si:*alien-declarations*)

    (otherwise
     (let ((ext-env (get-environment env)))
       (declaration-info name ext-env)))))

(defmacro define-declaration (decl-name (arg-var &optional (env-var (gensym "ENV"))) &body body)
  "Defines a handler function for the a user-defined declaration."

  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (declaim (declaration ,decl-name))
     (setf (declaration-function ',decl-name)
	   (lambda (,arg-var ,env-var)
	     (declare (ignorable ,env-var))
	     ,@body))

     #+ccl (ccl:define-declaration ,decl-name (,arg-var)
       (let (,env-var)
	 (declare (ignorable ,env-var))
	 ,@body))

     ',decl-name))


;;; Augment Environment

(defun augment-environment (env &key variable symbol-macro function macro declare)
  "Augment an environment (ENV) with new information."

  (let ((env (c::cmp-env-copy (or env c::*cmp-env-root*))))
    (loop
       for name in variable
       for var = (c::make-var :name name :kind 'c::lexical)
       do
	 (setf env (c::cmp-env-register-var var env)))

    (loop for (name def) in symbol-macro
       do
	 (setf env (c::cmp-env-register-symbol-macro name def env)))

    (loop
       for fn in function
       for fun = (c::make-fun :name fn)
       do
	 (setf env (c::cmp-env-register-function fun env)))

    (loop for (name fn) in macro
       do
	 (setf env (c::cmp-env-register-macro name fn env)))

    (loop for (type . arguments) in declare
       do
	 (setf env (augment-declaration type arguments env)))

    env))

(defun augment-declaration (name args env)
  "Augment an environment with declaration information.

   NAME is the declaration name.

   ARGS is the argument list passed to the DECLARE expression.

   ENV is the environment.

   Returns the augmented environment."

  (case name
    (optimize
     (c::cmp-env-add-optimizations args env))

    (special
     (loop for name in args
	for var = (c::cmp-env-search-var var env)
	do
	  ;; If CMP-ENV-DECLARE-SPECIAL is called, on some
	  ;; environments, in which the variable is not a C::VAR
	  ;; structure, an access violation is signalled.

	  (if (c::var-p var)
	      (setf env (c::cmp-env-declare-special var env))
	      (setf env (c::cmp-env-register-var
			 (c::make-var :name name :kind 'c::special)
			 env))))
     env)

    (type
     (destructuring-bind (type &rest vars) args
       (loop
	  for name in vars
	  for var = (c::cmp-env-search-var name env)
	  do
	    (cond
	      ((c::var-p var)
	       (let ((var (c::copy-var var)))
		 (setf (c::var-type var) type)
		 (setf env
		       (c::cmp-env-register-var var env))))

	      (var)

	      ((si::specialp var)
	       (let ((var (c::make-var :name name
				       :kind 'c::special
				       :type type)))

		 (setf env
		       (c::cmp-env-register-var var env))))

	      (t
	       (aif (augment-symbol-macro-type name type env)
		    (setf env it)
		    (warn "AUGMENT-ENVIRONMENT: (TYPE ~s) declaration for unknown variable ~s." type name))))))
     env)

    ;; ECL doesn't record DYNAMIC-EXTENT and there isn't much use for
    ;; augmenting an environment with IGNORE declarations hence these
    ;; are ignored
    ((dynamic-extent ignore ignorable)
     env)

    (otherwise
     (c::add-one-declaration env (cons name args)))))

(defun augment-symbol-macro-type (name type env)
  "Augment a symbol-macro with type information."

  (when-let ((sym-macro (c::cmp-env-search-symbol-macro name env)))
    (let ((expression `(the ,type (funcall sym-macro name nil))))
      (c::cmp-env-register-symbol-macro name expression env))))


;;; ENCLOSE and PARSE-MACRO

(defun parse-macro (name lambda-list body &optional environment)
  "Parse a macro definition form (as found in MACROLET or DEFMACRO).

   NAME is the name of the macro. The body of the macro is enclosed in
   a block with this name.

   LAMBDA-LIST is the macro lambda-list.

   BODY is the list of forms comprising the macro body.

   ENVIRONMENT is the lexical environment in which the macro
   definition form is to be parsed. This is used to expand macros used
   in the macro definition.

   Returns a lambda expression of two arguments which is suitable for
   use as a macro function."

  (declare (ignore environment))

  (let ((env-var (gensym "ENV")))
    (flet ((walk-arg (type arg)
	     (case type
	       (:environment
		(setf env-var arg)
		nil)

	       ((nil)
		(unless (eq arg '&environment)
		  arg))

	       (otherwise arg))))

      (let ((lambda-list (map-lambda-list #'walk-arg lambda-list :destructure t :env t)))
	(with-gensyms (name-var whole-var)
	  `(lambda (,whole-var ,env-var)
	     (declare (ignorable ,env-var))
	     (block ,name
	       (destructuring-bind (,name-var ,@lambda-list) ,whole-var
		 (declare (ignore ,name-var))
		 ,@body))))))))

(defun enclose (lambda-expression &optional env)
  "Compile a lambda expression in a given environment ENV."

  (si:eval-with-env lambda-expression env nil env))

(defun enclose-macro (name lambda-list body &optional env)
  "Compile a local macro definition in a given environment ENV."

  (enclose
   (parse-macro name lambda-list body env)
   env))


;;; CL-ENVIRONMENT Utilities

(defmacro in-environment ((env-var &optional (environment env-var)) (&rest bindings) &body forms)
  (flet ((make-binding (binding)
	   (match binding
	     ((type symbol)
	      (list binding binding))

	     (_ binding))))

    `(let ((,env-var ,environment) ,@(mapcar #'make-binding bindings))
       ,@forms)))

(defun augmented-macroexpand-1 (form &optional environment)
  (macroexpand-1 form environment))

(defun augmented-macroexpand (form &optional environment)
  (macroexpand form environment))

(defun augmented-macro-function (symbol &optional environment)
  (macro-function symbol environment))

(defun augmented-get-setf-expansion (form &optional environment)
  (get-setf-expansion form environment))

(defun augmented-compiler-macro-function (name &optional environment)
  (compiler-macro-function name environment))

(defun augmented-constantp (form &optional environment)
  (constantp form environment))
