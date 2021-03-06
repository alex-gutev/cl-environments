;;;; augment-environment.lisp
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

;;;; Implements the AUGMENT-ENVIRONMENT function and the
;;;; macro-expanding functions for augmented environments.

(in-package :cl-environments.cltl2)


;;; Augmented Environment

(defclass augmented-environment (environment)
  ((base-environment
    :initarg :base-environment
    :accessor base-environment
    :documentation
    "The base environment which this environment is augmenting.")

   (macro-functions
    :initform (make-hash-table :test #'equal)
    :initarg :macro-functions
    :accessor macro-functions
    :documentation
    "Hash table mapping macro names to the corresponding macro
     functions.")

   (symbol-macros
    :initform (make-hash-table :test #'eq)
    :initarg :symbol-macros
    :accessor symbol-macros
    :documentation
    "Hash table mapping symbol-macro names to the corresponding
     symbol-macro expansion."))

  (:documentation
   "Extended environment with macro and symbol macro definitions added
    by AUGMENT-ENVIRONMENT."))

(defun copy-augmented-environment (environment)
  (let ((copy (copy-environment environment)))
    (change-class copy 'augmented-environment
		  :base-environment (base-environment environment)
		  :macro-functions (copy-hash-table (macro-functions environment))
		  :symbol-macros (copy-hash-table (symbol-macros environment)))))

(defun eval-in-augmented-env (env env-var form)
  "Evaluate a form in a lexical environment constructed from an `AUGMENTED-ENVIRONMENT'

   FORM is evaluated in an environment containing the macros and
   symbol macros added to ENV, as well the macros and symbol-macros in
   the BASE-ENVIRONMENT which ENV augments.

   ENV is the `AUGMENTED-ENVIRONMENT' from which to construct the
   lexical environment in which FORM is evaluated.

   ENV-VAR is the name of the variable to which the lexical
   environment is bound.

   FORM if the form to evaluate in the lexical environment."

  (labels ((make-macros (macros)
	     (iter (for (macro macro-fn) in-hashtable macros)
		   (collect (make-local-macro macro macro-fn))))

	   (make-symbol-macros (symbol-macros)
	     (iter (for (sym expansion) in-hashtable symbol-macros)
		   (collect (make-symbol-macro sym expansion))))

	   (make-local-macro (name fn)
	     (with-gensyms (whole rest env)
	       `(,name (&whole ,whole &rest ,rest &environment ,env)
		       (declare (ignore ,rest))
		       (funcall ,fn ,whole ,env))))

	   (make-symbol-macro (name expansion)
	     `(,name ,expansion))

	   (make-dummy-function (name)
	     `(,name () nil))

	   (macro-bindings (functions env)
	     (iter
	       (for (sym binding) in-hashtable functions)
	       (when (and (local binding) (eq (binding-type binding) :macro))
		 (when-let (fn (macro-function sym env))
		   (collect (cons sym fn))))))

	   (function-bindings (functions)
	     (iter
	       (for (sym binding) in-hashtable functions)
	       (when (and (local binding) (eq (binding-type binding) :function))
		 (collect sym))))

	   (symbol-macro-bindings (symbol-macros env)
	     (iter
	       (for (sym binding) in-hashtable symbol-macros)
	       (when (and (local binding) (eq (binding-type binding) :symbol-macro))
		 (multiple-value-bind (expansion expanded-p)
		     (macroexpand-1 sym env)

		   (when expanded-p
		     (collect (list sym expansion)))))))

	   (variable-bindings (variables)
	     (iter
	       (for (sym binding) in-hashtable variables)
	       (when (and (local binding)
			  (member (binding-type binding) '(:lexical :special)))
		 (collect sym)))))

    (with-slots (functions variables macro-functions symbol-macros base-environment) env
      (let ((macros (macro-bindings functions base-environment))
	    (symbol-macro-expansions (symbol-macro-bindings variables base-environment))
	    (local-fns (function-bindings functions)))

	(with-gensyms (expand)
	  (values-list
	   (eval
	    (enclose-in-env
	     env

	     `((flet ,(mapcar #'make-dummy-function local-fns)
		 (macrolet ,(mapcar
			     (lambda (mac) (make-local-macro (car mac) (cdr mac)))
			     macros)

		   (let ,(variable-bindings variables)
		     (symbol-macrolet ,symbol-macro-expansions

		       ;; Augmented Macros and Symbol-Macros
		       (macrolet ,(make-macros macro-functions)
	     		 (symbol-macrolet ,(make-symbol-macros symbol-macros)
	     		   (macrolet ((,expand (&environment ,env-var)
	     				`',(multiple-value-list ,form)))

	     		     (,expand)))))))))

	     :walk-forms nil))))))))

(defmacro with-augmented-environment (environment (env-var) &body forms)
  `(eval-in-augmented-env ,environment ',env-var '(progn ,@forms)))

(defmacro in-environment ((env-var &optional (environment env-var)) (&rest bindings) &body forms)
  "Evaluate FORMS with access to a given environment.

   ENV-VAR is the name of the variable to which the native environment
   object is bound. This binding is made available to forms.

   If ENVIRONMENT evaluates to an augmented environment object, FORMS
   are evaluated in an environment in which ENV-VAR is bound to a
   native lexical environment object which is equivalent to the
   augmented environment. Otherwise ENV-VAR is bound to
   ENVIRONMENT. If ENVIRONMENT is not given, the variable with name
   given by ENV-VAR is evaluated in the current environment, to obtain
   the environment object, and then a binding to the native
   environment object is made under the same variable in the
   environment in which FORMS are evaluated.

   ENVIRONMENT is a form which evaluates to an environment object,
   which may be either a native environment or an augmented
   environment object.

   BINDINGS is a list of bindings which will be made available to
   FORMS. Each element of is a list of the form (NAME INITFORM) where
   NAME is the name of the variable which will be available to FORMS
   and INITFORM is the form to which it is bound. INITFORM is
   evaluated in the environment in which this macro-form is found. A
   binding may be a symbol by itself in which case it is a short-form
   for (NAME NAME).

   FORMS is the list of forms which are evaluated, in an implicit
   PROGN. The binding to the variable name given by ENV-VAR and the
   bindings specified in BINDINGS are available to the forms. The
   forms may be executed in a dynamically created environment and thus
   they do not have access to any lexical variable, function and macro
   definitions in the environment of the macro form."

  (flet ((make-binding (var)
	   (ematch var
	     ((type symbol)
	      ``(,',var ',,var))

	     ((list (and (type symbol) name) initform)
	      ``(,',name ',,initform))))

	 (make-let-binding (var)
	   (ematch var
	     ((type symbol)
	      (list var var))

	     ((list (and (type symbol) name) initform)
	      (list name initform)))))

    (with-gensyms (env)
      `(let ((,env ,environment))
	 (typecase ,env
	   (augmented-environment
	    (eval-in-augmented-env
	     ,env
	     ',env-var
	     `(let ,(list ,@(mapcar #'make-binding bindings)) ,@',forms)))

	   (otherwise
	    (let ((,env-var ,env) ,@(mapcar #'make-let-binding bindings)) ,@forms)))))))


;;; Definition of AUGMENT-ENVIRONMENT

(defun augment-environment (env &key variable symbol-macro function macro declare)
  "Create a new environment by augmenting an existing environment with new information.

   ENV is the existing environment to augment which may be an
   implementation specific lexical environment or an
   AUGMENTED-ENVIRONMENT. This environment can be used as the
   environment parameter to the AUGMENTED-MACROEXPAND and
   AUGMENTED-MACROEXPAND-1 functions.

   VARIABLE is a list of symbols that will be bound as variables in
   the new environment.

   SYMBOL-MACRO is a list of symbol-macro definitions, each of the
   form (NAME DEFINITION).

   FUNCTION is a list of symbols that will be bound as local functions
   in the environment.

   MACRO is a list of macro definitions, each of the form (NAME
   MACRO-FUNCTION), where MACRO-FUNCTION is a function of two
   arguments, the entire macro form and the implementation specific
   lexical environment in which it is expanded.

   DECLARE is a list of declaration specifiers, in the same format as
   DECLARE expressions. Information about these declarations can be
   retrieved using VARIABLE-INFORMATION, FUNCTION-INFORMATION and
   DECLARATION-INFORMATION."

  (let ((aug-env (make-augmented-environment env)))
    (with-slots (macro-functions symbol-macros) aug-env
      (flet ((add-macro (macro)
	       (add-function (first macro) aug-env :binding-type :macro))

	     (add-variable (variable)
	       (add-variable variable aug-env))

	     (add-symbol-macro (macro)
	       (add-symbol-macro (first macro) aug-env))

	     (add-function (function)
	       (add-function function aug-env))

	     (add-declaration (decl)
	       (destructuring-bind (decl . args) decl
		 (walk-declaration decl args aug-env)))

	     (add-macro-function (macro)
	       (destructuring-bind (name fn) macro
		 (setf (gethash name macro-functions) fn)))

	     (add-symbol-macro-expansion (macro)
	       (destructuring-bind (name expansion) macro
		 (setf (gethash name symbol-macros) expansion))))

	(mapc #'add-variable variable)
	(mapc #'add-symbol-macro symbol-macro)
	(mapc #'add-function function)
	(mapc #'add-macro macro)
	(mapc #'add-declaration declare)

	(mapc #'add-macro-function macro)
	(mapc #'add-symbol-macro-expansion symbol-macro)

	aug-env))))

(defun make-augmented-environment (environment)
  "Make an `AUGMENTED-ENVIRONMENT' based on ENVIRONMENT, which may be
   either a lexical environment or another `AUGMENTED-ENVIRONMENT'."

  (typecase environment
    (augmented-environment
     (copy-augmented-environment environment))

    (otherwise
     (let ((env (copy-environment (get-environment environment))))
       (change-class env 'augmented-environment
		     :base-environment environment)))))

(defun augmented-macroexpand-1 (form &optional environment)
  "Expands a macro form, like CL:MACROEXPAND-1, in a given lexical or augmented environment.

   FORM is the form to macroexpand.

   ENVIRONMENT is the environment in which to expand FORM, which may
   be either an implementation specific lexical environment or an
   augmented environment returned by AUGMENT-ENVIRONMENT. If NIL
   defaults to the global environment."

  (in-environment (environment)
      (form)

    (macroexpand-1 form environment)))

(defun augmented-macroexpand (form &optional environment)
  "Expands a macro form, like CL:MACROEXPAND, in a given lexical or augmented environment.

   FORM is the form to macroexpand.

   ENVIRONMENT is the environment in which to expand FORM, which may
   be either an implementation specific lexical environment or an
   augmented environment returned by AUGMENT-ENVIRONMENT. If NIL
   defaults to the global environment."

  (in-environment (environment)
      (form)

    (macroexpand form environment)))

(defun augmented-macro-function (symbol &optional environment)
  "Retrieve the macro function for a symbol.

   SYMBOL the symbol for which the macro function is retrieved.

   ENVIRONMENT is the lexical environment which is searched for the
   macro definition. If NIL defaults to the global environment.

   If SYMBOL does not name a symbol in ENVIRONMENT, returns NIL."

  (typecase environment
    (augmented-environment
     (get-augmented-macro-function symbol environment))

    (otherwise
     (macro-function symbol environment))))

(defun get-augmented-macro-function (symbol environment)
  "Retrieve the macro function for a symbol in an `AUGMENTED-ENVIRONMENT'.

   If SYMBOL does not name a macro added to the augmented environment,
   the BASE-ENVIRONMENT of the augmented environment is searched.

   SYMBOL is the symbol naming the macro.

   ENVIRONMENT is the `AUGMENTED-ENVIRONMENT' object."


  (with-slots (macro-functions functions base-environment)
      environment

    (when-let (binding (gethash symbol functions))
      (case (binding-type binding)
	(:macro
	 (gethash symbol macro-functions))

	(:function nil)

	(otherwise
	 (macro-function symbol base-environment))))))

(defun augmented-get-setf-expansion (place &optional environment)
  "Determine the SETF expansion for PLACE in ENVIRONMENT."

  (in-environment (environment)
      (place)

    (get-setf-expansion place environment)))

(defun augmented-compiler-macro-function (name &optional environment)
  "Return the compiler-macro-function for function NAME in ENVIRONMENT."

  (in-environment (environment)
      (name)

    (compiler-macro-function name environment)))

(defun augmented-constantp (form &optional environment)
  "Determine if FORM is a constant form in ENVIRONMENT."

  (in-environment (environment)
      (form)

    (constantp form environment)))


;;; Definition of ENCLOSE and ENCLOSE-MACRO

(defun enclose (lambda-expression &optional environment)
  "Return a function object that is equivalent to what would be
   obtained by evaluating `(FUNCTION ,LAMBDA-EXPRESSION) in the
   environment ENVIRONMENT."

  (typecase environment
    (augmented-environment
     (with-gensyms (env-var)
       (eval-in-augmented-env
	environment
	env-var
	`(function ,lambda-expression))))

    (null (compile nil lambda-expression))

    (otherwise
     (enclose lambda-expression (augment-environment environment)))))

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

(defun enclose-macro (name lambda-list body &optional environment)
  "Parse a macro definition form (as found in MACROLET or DEFMACRO) into a macro function.

   NAME is the name of the macro. The body of the macro is enclosed in
   a block with this name.

   LAMBDA-LIST is the macro lambda-list.

   BODY is the list of forms comprising the macro body.

   ENVIRONMENT is the lexical environment in which the macro
   definition form is to be parsed. This is used to expand macros used
   in the macro definition.

   Returns a function object which is suitable as a macro-function
   passed in the :MACRO argument of AUGMENT-ENVIRONMENT."

  (enclose
   (parse-macro name lambda-list body environment)
   environment))
