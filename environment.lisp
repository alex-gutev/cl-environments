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

(defun initial-declarations ()
  (aprog1 (make-hash-table :test #'eq)
    (setf (gethash 'optimize it) (mapcar (rcurry #'list 1) +optimize-qualities+))))


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

(defun add-variable (sym env &key (type :lexical) (local t))
  (with-slots (variables) env
    (let ((var-info (gethash sym variables)))
      (setf (gethash sym variables)
	    (if (eq (first var-info) :special)
		(list* :special local var-info)
		(list type local)))))) 

(defun env-ensure-variable (sym env)
  ;; TODO: Add special test, if special set type special
  (ensure-gethash sym (variables env) (list nil nil)))

(defun add-variable-info (var key value env)  
  (destructuring-bind (type localp &rest info)
      (env-ensure-variable var env)
    (setf (gethash var (variables env))
	  (list* type localp (cons (cons key value) info)))))

(defun add-variables-info (vars key value env)
  (mapc (rcurry #'add-variable-info key value env) vars))

(defun ensure-special-variable (var env)
  (with-slots (variables) env
    (let ((info (gethash var variables)))
      (let ((localp (second info))
	    (decl (third info)))
	(setf (gethash var variables)
	      (list* :special localp decl))))))

(defun get-var-info (var env)
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
	 (info (get-var-info variable ext-env)))
    (values (first info) (second info) (cddr info))))
