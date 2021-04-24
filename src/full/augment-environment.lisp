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

  (let ((aug-env (copy-environment (get-environment env))))
    (flet ((macro-cons (macro)
	     (destructuring-bind (name fn) macro
	       (cons name fn)))

	   (symbol-macro-cons (macro)
	     (destructuring-bind (name expansion) macro
	       (cons name expansion)))

	   (add-macro (macro)
	     (add-function (first macro) aug-env :binding-type :macro))

	   (add-variable (variable)
	     (add-variable variable aug-env))

	   (add-symbol-macro (macro)
	     (add-symbol-macro (first macro) aug-env))

	   (add-function (function)
	     (add-function function aug-env))

	   (add-declaration (decl)
	     (destructuring-bind (decl . args) decl
	       (walk-declaration decl args aug-env))))

      (mapc #'add-variable variable)
      (mapc #'add-symbol-macro symbol-macro)
      (mapc #'add-function function)
      (mapc #'add-macro macro)
      (mapc #'add-declaration declare)

      (change-class
       aug-env
       'augmented-environment
       :base-environment env

       :macro-functions
       (alist-hash-table
	(mapcar #'macro-cons macro)
	:test #'eq)

       :symbol-macros
       (alist-hash-table
	(mapcar #'symbol-macro-cons symbol-macro))))))

(defun augmented-macroexpand-1 (form &optional environment)
  "Expands a macro form, like CL:MACROEXPAND-1, in a given lexical or augmented environment.

   FORM is the form to macroexpand.

   ENVIRONMENT is the environment in which to expand FORM, which may
   be either an implementation specific lexical environment or an
   augmented environment returned by AUGMENT-ENVIRONMENT. If NIL
   defaults to the global environment."

  (typecase environment
    (augmented-environment
     (with-gensyms (env)
       (eval-in-augmented-env
	environment
	env
	`(macroexpand-1 ',form ,env))))

    (otherwise
     (macroexpand-1 form environment))))

(defun augmented-macroexpand (form &optional environment)
  "Expands a macro form, like CL:MACROEXPAND, in a given lexical or augmented environment.

   FORM is the form to macroexpand.

   ENVIRONMENT is the environment in which to expand FORM, which may
   be either an implementation specific lexical environment or an
   augmented environment returned by AUGMENT-ENVIRONMENT. If NIL
   defaults to the global environment."

  (typecase environment
    (augmented-environment
     (with-gensyms (env)
       (eval-in-augmented-env
	environment
	env
	`(macroexpand ',form ,env))))

    (otherwise
     (macroexpand form environment))))

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

  (typecase environment
    (augmented-environment
     (with-gensyms (env)
       (eval-in-augmented-env
	environment
	env
	`(get-setf-expansion ',place ,env))))

    (otherwise
     environment)))
