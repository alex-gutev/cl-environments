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
  '(speed safety compilation-speed space debug))


(defun initial-declarations ()
  (aprog1 (make-hash-table :test #'eq)
    (setf (gethash 'optimize it) (mapcar (rcurry #'list 1) +optimize-qualities+))))


(defclass environment ()
  ((variables
    :initform (make-hash-table :test #'eq)
    :initarg :variables
    :accessor variables)
   
   (functions
    :initform (make-hash-table :test #'eq)
    :initarg :functions
    :accessor functions)
   
   (declarations
    :initform (initial-declarations)
    :initarg :declarations
    :accessor declarations)

   (decl-functions
    :initform (make-hash-table :test #'eq)
    :initarg :decl-functions
    :accessor decl-functions)))

(defvar *global-environment* (make-instance 'environment))


(defun copy-environment (env)
  (make-instance 'environment
		 :variables (copy-hash-table (variables env))
		 :functions (copy-hash-table (functions env))
		 :declarations (copy-hash-table (declarations env))
		 :decl-functions (copy-hash-table (decl-functions env))))


;;; Local Environments

(defvar *env-sym* (gensym "ENV"))

(defun get-environment (env)
  (if (null env)
      *global-environment*
      (get-local-environment env)))

(defun get-local-environment (env)
  (let ((ext-env (macroexpand *env-sym* env)))
    (if (eq ext-env *env-sym*) ; No local environment
	*global-environment* ; For now simply return global environment
	ext-env)))

(defun enclose-in-env (env forms)
  `(symbol-macrolet ((,*env-sym* ,env))
     ,@forms))


;;; Variables

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

(defun binding-type (var type)
  "If var is declared special, as determined by SPECIALP,
   then :special is returned otherwise TYPE is returned."
  
  (if (specialp var) :special type))


(defun add-variable (sym env &key (type :lexical) (local t))
  "Adds the variable named by SYM to the environment ENV. The binding
   type is either that given by TYPE or :SPECIAL if the variable is
   currently declared special in the global environment."
  
  (with-slots (variables) env
    (destructuring-bind (&optional old-type localp . decl)
	(gethash sym variables)
      (declare (ignore decl))
      
      (setf (gethash sym variables)
	    (list
	     (if (and (not localp) (eq old-type :special))
		 :special
		 (binding-type sym type))
	     local))))) 

(defun env-ensure-variable (sym env)
  "Establish a new variable binding for SYM in ENV, unless there is
   already such a binding. The binding type, if a new binding is
   established, is :SPECIAL if SYM names a special variable in the
   global environment (determined by SPECIALP), otherwise it is
   NIL. The value for the local flag is NIL."
  
  (ensure-gethash sym (variables env)
		  (list (binding-type sym nil) nil)))

(defun add-variable-info (var key value env)
  "Adds the pair (KEY . VALUE) to the variable information for VAR in
   the environment ENV. If no binding for VAR exists, a new binding is
   created."
  
  (destructuring-bind (type localp &rest info)
      (env-ensure-variable var env)
    (setf (gethash var (variables env))
	  (list* type localp (cons (cons key value) info)))))

(defun add-variables-info (vars key value env)
  "Adds the pair (KEY . VALUE) to the variable information of each
   variable that is an element of VARS."
  
  (mapc (rcurry #'add-variable-info key value env) vars))

(defun ensure-special-variable (var env)
  "Ensures that the symbol VAR names a special variable in the
   environment ENV. If no binding for VAR exists, in ENV, a new
   binding of type :SPECIAL is created."
  
  (with-slots (variables) env
    (let ((info (gethash var variables)))
      (let ((localp (second info))
	    (decl (third info)))
	(setf (gethash var variables)
	      (list* :special localp decl))))))

(defun get-var-info (var env)
  "Returns the variable information for VAR. The first element is the
   binding type, the second element is a flag for whether the binding
   is a local binding, the rest of the elements contain the
   declaration information applying to the variable."
  
  (gethash var (variables env)))


;;; Functions

(defun add-function (sym env &key (type :function) (local t))
  (with-slots (functions) env
    (setf (gethash sym functions)
	  (list type local))))

(defun env-ensure-function (sym env)
  (ensure-gethash sym (functions env) (list nil nil)))

(defun add-function-info (fn key value env)
  (destructuring-bind (type localp &rest info)
      (env-ensure-function fn env)
    (setf (gethash fn (functions env))
	  (list* type localp (cons (cons key value) info)))))

(defun add-functions-info (fns key value env)
  (mapc (rcurry #'add-function-info key value env) fns))


;;; Declarations

(defun declaration-info (name env)
  (gethash name (declarations env)))

(defun (setf declaration-info) (value name env)
  (setf (gethash name (declarations env)) value))


(defun declaration-function (name env)
  (gethash name (decl-functions env)))

(defun (setf declaration-function) (fn name env)
  (setf (gethash name (decl-functions env)) fn))


;;; CLTL2 Interface

(defun variable-information (variable &optional env)
  (let* ((ext-env (get-environment env))
	 (info (get-var-info variable ext-env))
	 (type (if info (first info) (binding-type variable nil))))
    (values type (second info) (cddr info))))
