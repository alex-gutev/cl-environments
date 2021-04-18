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
    :initform (make-hash-table :test #'eq)
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

(defmethod make-load-form ((object environment) &optional env)
  (make-load-form-saving-slots object :environment env))

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

(defvar *env-sym* 'local-environment-symbol-macro
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
       (setf (gethash sym variables) it)))))

(defun (setf variable-binding) (binding sym env)
  "Sets the binding for the variable with symbol SYM, in the
   environment ENV, to the `binding' object BINDING."

  (setf (gethash sym (variables env)) binding))


(defun add-variable-info (var key value env)
  "Adds the pair (KEY . VALUE) to the declaration information of the
   variable-binding for VAR in the environment ENV. If no binding for
   VAR exists, a new binding is created."

  (push (cons key value) (variable-binding var env)))

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
       (setf (gethash fn functions) it)))))

(defun (setf function-binding) (binding fn env)
  "Sets the function binding for the symbol FN, in the environment
   ENV, to the `binding' object BINDING."

  (setf (gethash fn (functions env)) binding))



(defun add-function-info (fn key value env)
  "Adds the pair (KEY . VALUE) to the declaration information of the
   function-binding for FN in the environment ENV. If no binding for
   FN exists a new binding is created."

  (push (cons key value) (function-binding fn env)))

(defun add-functions-info (fns key value env)
  "Adds the pair (KEY . VALUE) to the declaration information of the
   function-bindings of each symbol in FNS."

  (mapc (rcurry #'add-function-info key value env) fns))



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
