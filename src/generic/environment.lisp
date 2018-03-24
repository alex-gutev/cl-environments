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

(in-package :cl-environments)

(defconstant +optimize-qualities+
  '(speed safety compilation-speed space debug)  
  "List of optimization qualities.")


(defun initial-declarations ()
  "Returns the initial declaration information applying to a null
   environment. This contains the optimization qualities with their
   default level 1."
  
  (aprog1 (make-hash-table :test #'eq)
    (setf (gethash 'optimize it) (mapcar (rcurry #'list 1) +optimize-qualities+))))


;;; Bindings

(defclass binding ()
  ((type
    :initarg :type
    :initform nil
    :reader type
    :documentation "The type of binding.")
   
   (local
    :initarg :local
    :initform nil
    :reader local
    :documentation "True if there is a local definition.")
   
   (global
    :initarg :global
    :initform nil
    :reader global
    :documentation
    "True if there is a global definition, this may be nil even if
     LOCAL is nil, indicating there is no definition. Similarly GLOBAL
     may be true even if local is true indicating that there is both a
     global and local definition.")

   (declarations
    :initform nil
    :initarg :declarations
    :reader declarations
    :documentation
    "Association list containing declaration information, as a list of
     key-value pairs."))

  (:documentation
   "Stores binding information. Can be used both for function and
    variables bindings"))


;;; Constructor functions

(defun make-binding (&rest args &key &allow-other-keys)
  (apply #'make-instance 'binding args))  

(defun add-binding-info (binding key value)
  "Creates a new binding with the pair (KEY . VALUE) added to the
   front of its DECLARATIONS list"

  (slot-values (type local global declarations)
      binding
    (make-binding :type type
		  :local local
		  :global global
		  :declarations (acons key value declarations))))
	  

(defun change-binding-type (binding &key (type nil type-sp) (local nil local-sp) (global nil global-sp) (declarations nil decl-sp))
  "Creates a new `binding' which is a copy of BINDING, with the
   binding type set to TYPE and local flag set to LOCAL. If the TYPE,
   LOCAL, GLOBAL or DECLARATIONS argument is not provided, the slot
   value of BINDING is copied to the new binding, or NIL if BINDING is
   NIL."

  (slot-values ((old-type type)
		(old-local local)
		(old-global global)
		(old-declarations declarations))
      binding
    (make-binding :type (if type-sp type old-type)
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
    "Hash-table of declarations neither applying to variables nor
     functions.")

   (decl-functions
    :initform (make-hash-table :test #'eq)
    :initarg :decl-functions
    :accessor decl-functions
    :documentation
    "Functions which handle user-defined declarations. Stored as a
     hash table where the key is the user-defined declaration name and
     the value is the function.")

   (macro-functions
    :initform (make-hash-table :test #'eq)
    :initarg :macro-functions
    :accessor macro-functions
    :documentation
    "Macro expansion functions. Stores a hash table where the keys are
     the symbols naming the macros and the values are the
     corresponding macro functions. Only macro functions manually
     added, using AUGMENT-ENVIRONMENT, are stored.")

   (macro-forms
    :initform (make-hash-table :test #'eq)
    :initarg :macro-forms
    :accessor macro-forms
    :documentation
    "Symbol macro expansion forms. Stores a hash table where the keys
     are the macro symbols and the values are the corresponding
     expansion forms. Only symbol macros manually added, using
     AUGMENT-ENVIRONMENT, are stored.")

   (lexical-environment
    :initform nil
    :initarg :lexical-environment
    :accessor lexical-environment
    :documentation
    "The implementation-specific environment in which this environment
     is contained."))

  (:documentation
   "The extendend environment class. Stores information about the
    environment obtained via code walking."))

(defvar *global-environment* (make-instance 'environment)
  "The global null environment object.")


(defun copy-environment (env &optional (lex-env (lexical-environment env)))
  "Copies the environment object ENV. A shallow copy of the hash
   tables containing the mappings between symbols and bindings is
   performed, that is the `binding' objects themselves are not
   copied."

  (with-slots (variables
	       functions
	       declarations
	       decl-functions
	       macro-functions
	       macro-forms) env
  
    (make-instance 'environment
		   :variables (copy-hash-table variables)
		   :functions (copy-hash-table functions)
		   :declarations (copy-hash-table declarations)
		   :decl-functions (copy-hash-table decl-functions)

		   :macro-functions (copy-hash-table macro-functions)
		   :macro-forms (copy-hash-table macro-forms)
		   :lexical-environment lex-env)))

(defun enclose-environment (env lex-env)
  "Creates a shallow copy of the environment ENV and sets its
   LEXICAL-ENVIRONMENT slot to LEX-ENV."
  
  (with-slots (variables
	       functions
	       declarations
	       decl-functions
	       macro-functions
	       macro-forms) env
  
    (make-instance 'environment
		   :variables variables
		   :functions functions
		   :declarations declarations
		   :decl-functions decl-functions

		   :macro-functions macro-functions
		   :macro-forms macro-forms
		   :lexical-environment lex-env)))


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
   object, passed to macro functions, if it is NIL the global null
   environment is returned."
  
  (if (null env)
      *global-environment*
      (get-local-environment env)))

(defun get-local-environment (env)
  "Returns the extended enevironment object of the lexical environment
   ENV. Macroexpands *ENV-SYM*, using MACROEXPAND, in the lexical
   environment ENV, to obtain the extended environment object. If
   there is no expansion for *ENV-SYM* the global extended environment
   object is returned."

  (multiple-value-bind (ext-env expanded)
      (macroexpand *env-sym* env)
    (if expanded
	ext-env
	*global-environment*)))

(defun enclose-in-env (env forms)
  "Encloses FORMS in the extended environment object ENV. Returns
   FORMS wrapped in a SYMBOL-MACROLET, declararing a symbol macro,
   *ENV-SYM*, which expands to the environment object ENV."
  
  `(symbol-macrolet ((,*env-sym* ,env))
     ,@forms))


;;; Variables

(defun variable-binding (sym env)
  "Returns the `binding' object for the variable with symbol SYM, in
   the environment ENV. If ENV does not have a binding for SYM, the
   `binding' for SYM in the global environment is copied and added to
   ENV. If the global environment does not have a binding for SYM,
   however the variable is declared special in the global Common Lisp
   environment a `binding' of type :SPECIAL is added to ENV and
   returned, otherwise NIL is returned."

  (with-slots (variables) env
    (acond
      ((gethash sym variables)
       it)
      
      ((gethash sym (variables *global-environment*))
       (setf (gethash sym variables) it))

      ((constantp sym)
       (setf (gethash sym variables)
	     (make-binding :type :constant :global t)))
      
      ((specialp sym)
       (setf (gethash sym variables)
	     (make-binding :type :special :global t))))))

(defun (setf variable-binding) (binding sym env)
  "Sets the binding for the variable with symbol SYM, in the
   environment ENV, to the `binding' object BINDING."
  
  (setf (gethash sym (variables env)) binding))


(defun specialp (var)
  "Tests whether a variable is declared special in the global
   environment. A test form is created and EVAL'd in which VAR is
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
   warnings/errors. Furtheremore this will not return true for
   variables declared special in the same file."
  
  (with-gensyms (fn)
    (eval
     `(locally (declare (type t ,var))
	(let ((,var 1))
	  (flet ((,fn () ,var))
	    (let ((,var 2))
	      (eql ,var (,fn)))))))))


(defun add-variable (sym env &key (type :lexical) (local t))
  "Adds the variable named by SYM to the environment ENV. The binding
   type is either that given by TYPE or :SPECIAL if the variable is
   currently declared special in the global environment."

  (slot-values ((old-type type) global)
      (variable-binding sym env)

    (let ((type (if (and global (eq old-type :special)) :special type)))
      (setf (variable-binding sym env)
	    (make-binding :type type
			  :local local
			  :global global)))))

(defun ensure-variable-type (sym env &rest args &key &allow-other-keys)
  "Ensures that there is a variable binding for SYM in env. If ENV
   does not have a binding for SYM, a new `binding' (of type NIL) is
   created. The remaining keyword arguments specify the slot-values of
   the new `binding' object, where the keyword is the slot keyword and
   the value is the slot value. If a value for a slot is not provided,
   the corresponding slot value of the existing `binding' object is
   used."

  (setf (variable-binding sym env)
	(apply #'change-binding-type (variable-binding sym env) args)))

(defun ensure-special-variable (var env)
  "Ensures that the symbol VAR names a special variable in the
   environment ENV. If no binding for VAR exists, in ENV, a new
   binding of type :SPECIAL is created."

  (ensure-variable-type var env :type :special))

(defun add-symbol-macro (sym env &key (local t))
  "Adds a new binding, of type :SYMBOL-MACRO, for SYM in ENV. If ENV
   already has a binding for SYM it is replaced unless it is of
   type :SPECIAL."

  (unless (aand (variable-binding sym env) (eq (type it) :special))
    (add-variable sym env :type :symbol-macro :local local)))


(defun add-variable-info (var key value env)
  "Adds the pair (KEY . VALUE) to the variable information for VAR in
   the environment ENV. If no binding for VAR exists, a new binding is
   created."

  (setf (variable-binding var env)
	(add-binding-info (variable-binding var env) key value)))

(defun add-variables-info (vars key value env)
  "Adds the pair (KEY . VALUE) to the variable information of each
   symbol in VARS."
  
  (mapc (rcurry #'add-variable-info key value env) vars))


;;; Functions

(defun function-binding (fn env)
  "Returns the `binding' object for the function FN in the environment
   ENV. If ENV does not have a binding for FN, the `binding' for FN in
   the global environment is copied and added to ENV. If the global
   environment does not have a binding for FN, the function type of FN
   in the global Common Lisp environment is determined (by
   GLOBAL-FUNCTION-TYPE) and a new `binding' of that type is added to
   ENV and returned. If FN is not declared in the global CL
   environment NIL is returned."

  (with-slots (functions) env
    (acond
      ((gethash fn functions)
       it)
      
      ((gethash fn (functions *global-environment*))
       (setf (gethash fn functions) it))
      
      ((global-function-type fn)
       (setf (gethash fn functions)
	     (make-binding :type it :global t))))))

(defun (setf function-binding) (binding fn env)
  "Sets the binding for the function FN, in the environment ENV, to
   the `binding' object BINDING."
  
  (setf (gethash fn (functions env)) binding))


(defun add-function (fn env &key (type :function) (local t))
  "Adds a function binding, for the function FN, to the environment
   ENV."

  (setf (function-binding fn env)
	(make-binding :type type :local local :global (not local))))

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
  "Adds the pair (KEY . VALUE) to the function information of FN in
   the environment ENV. If no binding for FN exists a new binding is
   created."

  (setf (function-binding fn env)
	(add-binding-info (function-binding fn env) key value)))

(defun add-functions-info (fns key value env)
  "Adds the pair (KEY . VALUE) to the function information of each
   symbol in FNS."
  
  (mapc (rcurry #'add-function-info key value env) fns))

(defun global-function-type (fn)
  "If FN is FBOUND, determines the type of the binding for FN in the
   global Common Lisp environment. If FN has a macro function :MACRO
   is returned, if FN names a special operator (checked using
   SPECIAL-OPERATOR-P), and has no macro function, :SPECIAL-FORM is
   returned, otherwise :FUNCTION is returned. If FN is not FBOUND NIL
   is returned."
  
  (when (fboundp fn)
    (cond
      ((macro-function fn)
       :macro)
      ((special-operator-p fn)
       :special-form)
      (t :function))))


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


;;; Macro functions

(defun macro-fn (symbol env)
  "Returns the macro expansion function associated with SYMBOL in the
   environment ENV."

  (gethash symbol (macro-functions env)))

(defun (setf macro-fn) (fn symbol env)
  "Sets the macro expansion function associated with SYMBOL in the
   environment ENV."

  (check-type fn function)

  (setf (gethash symbol (macro-functions env)) fn))


(defun macro-form (symbol env)
  "Returns the symbol macro expansion form associated with SYMBOL in
   the environment ENV."

  (gethash symbol (macro-forms env)))

(defun (setf macro-form) (form symbol env)
  "Sets the symbol macro expansion form associated with SYMBOL in the
   environment ENV."

  (setf (gethash symbol (macro-forms env)) form))
