;;;; environment.lisp
;;;;
;;;; Copyright 2017 Alexander Gutev
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

;;;; Extended environment implementation.

(in-package :cl-environments.cltl2)


(defconstant +optimize-qualities+
  '(speed safety compilation-speed space debug)
  "List of standardoptimization qualities.")


(defun initial-declarations ()
  "Returns the initial declaration information applying to a null
   environment. This contains the optimization qualities set to their
   default level 1."

  (aprog1 (make-hash-table :test #'eq)
    (setf (gethash 'optimize it) (mapcar (rcurry #'list 1) +optimize-qualities+))))


;;; Bindings

(defclass binding ()
  ((binding-type
    :initarg :binding-type
    :initform nil
    :reader binding-type
    :documentation "The type of binding.")

   (local
    :initarg :local
    :initform nil
    :reader local
    :documentation "True if there is a local binding.")

   (global
    :initarg :global
    :initform nil
    :reader global
    :documentation
    "True if there is a global binding this may be nil even if
     LOCAL is nil, indicating there is no binding Similarly GLOBAL
     may be true even if local is true indicating that there is both a
     global and local binding.")

   (declarations
    :initform nil
    :initarg :declarations
    :reader declarations
    :documentation
    "Association list containing declaration information."))

  (:documentation
   "Stores binding information. Can be used both for function and
    variable bindings"))

(defmethod make-load-form ((object binding) &optional env)
  (make-load-form-saving-slots object :environment env))


;;; Constructor functions

(defun make-binding (&rest args &key &allow-other-keys)
  (apply #'make-instance 'binding args))

(defun add-binding-info (binding key value)
  "Creates a new binding with the pair (KEY . VALUE) added to the
   front of its DECLARATIONS list"

  (slot-values (declarations) binding
    (change-binding-type binding :declarations (acons key value declarations))))

(defun change-binding-type (binding &key (binding-type nil binding-type-sp) (local nil local-sp) (global nil global-sp) (declarations nil decl-sp))
  "Creates a new `binding' with its slots initialized with the values
   provided in the keyword arguments BINDING-TYPE, LOCAL, GLOBAL and
   DECLARATIONS. If the keyword argument for a particular slot is
   omitted, the slot's value is set to the value of the corresponding
   in BINDING, or NIL if BINDING is NIL."

  (slot-values ((old-type binding-type)
		(old-local local)
		(old-global global)
		(old-declarations declarations))
      binding
    (make-binding :binding-type (if binding-type-sp binding-type old-type)
		  :local (if local-sp local old-local)
		  :global (if global-sp global old-global)
		  :declarations (if decl-sp declarations old-declarations))))


;;; Environments

(defclass environment ()
  ((variables
    :initform (make-hash-table :test #'eq)
    :initarg :variables
    :accessor variables
    :documentation
    "Hash table mapping symbols to variable bindings.")

   (functions
    :initform (make-hash-table :test #'equal)
    :initarg :functions
    :accessor functions
    :documentation
    "Hash table mapping symbols (and lists for SETF functions) to
     function bindings.")

   (declarations
    :initform (initial-declarations)
    :initarg :declarations
    :accessor declarations
    :documentation
    "Hash-table of declaration information neither applying to
     variables nor functions.")

   (decl-functions
    :initform (make-hash-table :test #'eq)
    :initarg :decl-functions
    :accessor decl-functions
    :documentation
    "Functions which handle user-defined declarations. Stored as a
     hash table where the key is the user-defined declaration name and
     the value is the function."))

  (:documentation
   "The extended environment class. Stores information about the
    lexical environment obtained via code walking."))

#-abcl
(defmethod make-load-form ((object environment) &optional env)
  (make-load-form-saving-slots object :environment env))

;;; The default method is not sufficient for ABCL as it doesn't make a
;;; load form for HASH-TABLE's.

#+abcl
(defmethod make-load-form ((object environment) &optional env)
  (declare (ignore env))

  (with-slots (variables functions declarations decl-functions) object
    (values
     `(make-instance
       'environment

       :variables (alist-hash-table ',(hash-table-alist variables) :test #'eq)
       :functions (alist-hash-table ',(hash-table-alist functions) :test #'eq)
       :declarations (alist-hash-table ',(hash-table-alist declarations) :test #'eq)
       :decl-functions (alist-hash-table ',(hash-table-alist decl-functions) :test #'eq))

     `(progn
	(setf (slot-value ,object 'variables)
	      (alist-hash-table ',(hash-table-alist variables) :test #'eq))
	(setf (slot-value ,object 'functions)
	      (alist-hash-table ',(hash-table-alist functions) :test #'eq))
	(setf (slot-value ,object 'declarations)
	      (alist-hash-table ',(hash-table-alist declarations) :test #'eq))
	(setf (slot-value ,object 'decl-functions)
	      (alist-hash-table ',(hash-table-alist decl-functions) :test #'eq))))))

(defvar *global-environment* (make-instance 'environment)
  "The global 'null' extended environment object.")


(defun copy-environment (env)
  "Copies the environment object ENV. A shallow copy of the hash
   tables containing the mappings between symbols and bindings is
   performed, that is the `binding' objects themselves are not
   copied. The LEXICAL-ENVIRONMENT slot of the new environment is set
   to LEX-ENV, if the LEX-ENV argument is supplied, otherwise it is
   set to the same value is in ENV."

  (with-slots (variables
	       functions
	       declarations
	       decl-functions) env

    (make-instance 'environment
		   :variables (copy-hash-table variables)
		   :functions (copy-hash-table functions)
		   :declarations (copy-hash-table declarations)
		   :decl-functions (copy-hash-table decl-functions))))



;;; Local Environments

(defvar *env-sym* (gensym "ENV")
  "The symbol of the symbol macro used to store the extended
   environment objects. The forms, located within the environment, are
   enclosed in a SYMBOL-MACROLET with the symbol macro expanding to
   the extended environment object. The extended environment objects
   can thus be obtained using (MACROEXPAND *ENV-SYM* <lexical
   environment>).")

(defun get-environment (env)
  "Returns the extended environment object of the local lexical
   environment or the global null environment if there is no local
   environment. ENV is the implementation specific lexical environment
   object, obtained via the &ENVIRONMENT macro parameters, if it is
   NIL the global null environment is returned."

  (if (null env)
      *global-environment*
      (get-local-environment env)))

(defun get-local-environment (env)
  "Returns the extended environment object of the lexical environment
   ENV (may not be NIL). Macro-expands *ENV-SYM*, using MACROEXPAND,
   in the lexical environment ENV, to obtain the extended environment
   object. If there is no expansion for *ENV-SYM* the global extended
   environment object is returned."

  (multiple-value-bind (ext-env expanded)
      (macroexpand *env-sym* env)
    (if expanded
	ext-env
	*global-environment*)))


(defun enclose-in-env (env forms)
  "Encloses FORMS in the extended environment object ENV. Returns
   FORMS wrapped in a SYMBOL-MACROLET, defining a symbol macro,
   *ENV-SYM*, which expands to the environment object ENV. If FORMS is
   already a list containing a SYMBOL-MACROLET form defining the
   symbol macro *ENV-SYM*, the SYMBOL-MACROLET form is simply
   returned."

  (match forms
    ((list (and body (list* 'cl:symbol-macrolet (list (list (eql *env-sym*) _)) _)))
     body)

    (_
     `(cl:symbol-macrolet ((,*env-sym* ,env))
	,@(enclose-forms forms)))))


;;; Variables

(defun variable-binding (sym env)
  "Returns the `binding' object for the variable with symbol SYM, in
   the environment ENV. If ENV does not have a binding for SYM, the
   `binding' for SYM in the global environment is copied and added to
   ENV. If the global environment does not have a binding for SYM,
   however the variable is declared special (determined using
   SPECIALP) or is a constant (determined using CONSTANTP) in the
   global Common Lisp environment a global `binding' of
   type :SPECIAL/:CONSTANT is added to ENV and returned, otherwise NIL
   is returned."

  (with-slots (variables) env
    (acond
      ((gethash sym variables)
       it)

      ((gethash sym (variables *global-environment*))
       (setf (gethash sym variables) it))

      ((constantp sym)
       (setf (gethash sym variables)
	     (make-binding :binding-type :constant :global t)))

      ((specialp sym)
       (setf (gethash sym variables)
	     (make-binding :binding-type :special :global t))))))

(defun (setf variable-binding) (binding sym env)
  "Sets the binding for the variable with symbol SYM, in the
   environment ENV, to the `binding' object BINDING."

  (setf (gethash sym (variables env)) binding))


(defconstant +global-special-vars+
  '(cl:*macroexpand-hook*
    cl:*standard-output*
    cl:*standard-input*

    #+clisp custom:*evalhook*)

  "List of predefined globally declared special variables, which
   should not be rebound temporarily by SPECIALP. If one of the
   symbols in the list, is passed to SPECIALP, T is returned
   immediately.")

(defun specialp (var)
  "Tests whether a variable is declared special in the global
   environment. A test form is created and compiled in which VAR is
   first bound to the value 1, a local function, which returns the
   value of VAR, is declared in the lexical scope of the binding. VAR
   is then bound to 2 (with a nested LET form) and its value is
   compared to the value returned by the function. If the values are
   EQL then the variable has dynamic scope and is thus declared
   special, otherwise it has lexical scope and is thus not declared
   special. Depending on the implementation this test may cause
   compiler warnings or errors to be generated for certain global
   variables, the test is wrapped in a LOCALLY with the type of the
   variable declared to T in an attempt to silence the warnings/errors
   however this is not guaranteed to work and may itself cause
   warnings/errors. Furthermore this will not return true for
   variables declared special in the same file.

   If VAR is a member of the list +GLOBAL-SPECIAL-VARS+ T is returned
   immediately without evaluating the test form."

  (or (member var +global-special-vars+)
      (let ((*macroexpand-hook* #'funcall))
	(with-gensyms (fn)
	  (with-open-stream (*standard-output* (make-broadcast-stream)) ; Suppress output
	    (handler-bind ((warning #'muffle-warning)) ; Suppress style warnings
	      (funcall
	       (compile nil
			`(lambda ()
			   (locally (declare (type t ,var))
			     (let ((,var 1))
			       (flet ((,fn () ,var))
				 (let ((,var 2))
				   (eql ,var (,fn)))))))))))))))


(defun add-variable (sym env &key (binding-type :lexical) (local t))
  "Adds the variable named by SYM to the environment ENV. The binding
   type is either that given by TYPE or :SPECIAL if the variable is
   declared special in the global environment."

  (slot-values ((old-type binding-type) global)
      (variable-binding sym env)

    (let ((binding-type (if (and global (eq old-type :special)) :special binding-type)))
      (setf (variable-binding sym env)
	    (make-binding :binding-type binding-type
			  :local local
			  :global global)))))

(defun ensure-variable-type (sym env &rest args &key &allow-other-keys)
  "Ensures that there is a variable binding for SYM in env. If ENV
   does not have a binding for SYM, a new `binding' (of type NIL) is
   created. The remaining keyword arguments specify the slot-values of
   the new `binding' object, where the keyword is the slot init-arg
   keyword and the value is the slot value. If a value for a slot is
   not provided, the corresponding slot value of the existing
   `binding' object is used."

  (setf (variable-binding sym env)
	(apply #'change-binding-type (variable-binding sym env) args)))

(defun ensure-special-variable (var env)
  "Ensures that the symbol VAR names a special variable in the
   environment ENV. If no binding for VAR exists, in ENV, a new
   binding of type :SPECIAL is created."

  (ensure-variable-type var env :binding-type :special))

(defun add-symbol-macro (sym env &key (local t))
  "Adds a new binding, of type :SYMBOL-MACRO, for SYM in ENV. If ENV
   already has a binding for SYM it is replaced unless it is of
   type :SPECIAL."

  (unless (aand (variable-binding sym env) (eq (binding-type it) :special))
    (add-variable sym env :binding-type :symbol-macro :local local)))


(defun add-variable-info (var key value env)
  "Adds the pair (KEY . VALUE) to the declaration information of the
   variable-binding for VAR in the environment ENV. If no binding for
   VAR exists, a new binding is created."

  (setf (variable-binding var env)
	(add-binding-info (variable-binding var env) key value)))

(defun add-variables-info (vars key value env)
  "Adds the pair (KEY . VALUE) to the declaration information of the
   variable-bindings of each symbol in VARS."

  (mapc (rcurry #'add-variable-info key value env) vars))


;;; Functions

(defun function-binding (fn env)
  "Returns the function `binding' object for the symbol FN in the
   environment ENV. If ENV does not have a binding for FN, the
   `binding' for FN in the global environment is copied and added to
   ENV. If the global environment does not have a binding for FN, the
   function type of FN in the global Common Lisp environment is
   determined (by GLOBAL-FUNCTION-TYPE) and a new global `binding' of
   that type is added to ENV and returned. If FN is not declared in
   the global CL environment NIL is returned."

  (with-slots (functions) env
    (acond
      ((gethash fn functions)
       it)

      ((gethash fn (functions *global-environment*))
       (setf (gethash fn functions) it))

      ((global-function-type fn)
       (setf (gethash fn functions)
	     (make-binding :binding-type it :global t))))))

(defun (setf function-binding) (binding fn env)
  "Sets the function binding for the symbol FN, in the environment
   ENV, to the `binding' object BINDING."

  (setf (gethash fn (functions env)) binding))


(defun add-function (fn env &key (binding-type :function) (local t))
  "Adds a function binding, for the symbol FN, to the environment
   ENV."

  (setf (function-binding fn env)
	(make-binding :binding-type binding-type :local local :global (not local))))

(defun ensure-function-type (fn env &rest args &key &allow-other-keys)
  "Ensures that there is a function binding for FN in ENV. If ENV does
   not have a binding for FN a new binding (of type NIL) is created.
   The remaining keyword arguments specify the slot-values of the new
   `binding' object, where the keyword is the slot keyword and the
   value is the slot value. If a value for a slot is not provided, the
   corresponding slot value of the existing `binding' object is used."

  (setf (function-binding fn env)
	(apply #'change-binding-type (function-binding fn env) args)))


(defun add-function-info (fn key value env)
  "Adds the pair (KEY . VALUE) to the declaration information of the
   function-binding for FN in the environment ENV. If no binding for
   FN exists a new binding is created."

  (setf (function-binding fn env)
	(add-binding-info (function-binding fn env) key value)))

(defun add-functions-info (fns key value env)
  "Adds the pair (KEY . VALUE) to the declaration information of the
   function-bindings of each symbol in FNS."

  (mapc (rcurry #'add-function-info key value env) fns))


(defun global-function-type (fn)
  "If FN is FBOUND, determines the type of the binding for FN in the
   global Common Lisp environment. If FN has a macro function :MACRO
   is returned, if FN names a special operator (checked using
   SPECIAL-OPERATOR-P), and has no macro function, :SPECIAL-FORM is
   returned, otherwise :FUNCTION is returned. If FN is not FBOUND NIL
   is returned."

  (when (fboundp fn)
    (if (symbolp fn)
	(cond
	  ((macro-function fn)
	   :macro)
	  ((special-operator-p fn)
	   :special-form)
	  (t :function))
	:function)))


;;; Declarations

(defun declaration-info (name env)
  "Returns the declaration information for the key NAME."
  (gethash name (declarations env)))

(defun (setf declaration-info) (value name env)
  "Sets the declaration information for the key NAME."
  (setf (gethash name (declarations env)) value))


(defun declaration-function (name env)
  "Returns the declaration function for the user-defined declaration
   NAME."
  (gethash name (decl-functions env)))

(defun (setf declaration-function) (fn name env)
  "Sets the declaration function for the user-defined declaration
   NAME."
  (setf (gethash name (decl-functions env)) fn))


;;; Clisp requires that the environment object be printed in a
;;; "readable" format, in order to serialize the output of
;;; macroexpansion to the *.lib files. The environment object is not
;;; used post-compilation thus there is no need to provide a full
;;; readable representation.

(defmethod print-object ((env environment) stream)
  "Prints `ENVIRONMENT' objects. If *PRINT-READABLY* is true, a form
   which creates an empty `ENVIRONMENT' object (using the read-eval
   #. reader macro). This is not done to provide a readable
   representation of the environment object (as that is not possible)
   but to silence compiler warnings and errors on some
   implementations."

  (if *print-readably*
      (write "#.(make-instance 'cl-environments:environment)" :stream stream)
      (call-next-method)))
