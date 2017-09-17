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

(defclass environment ()
  ((variables :initform (make-hash-table :test #'eq)
	      :accessor variables)
   (functions :initform (make-hash-table :test #'eq)
	      :accessor functions)
   (declarations :initform nil
		 :accessor declarations)))

(defvar *global-environment* (make-instance 'environment))


(defun copy-environment (env)
  (let ((new-env (make-instance 'environment)))
    (setf (variables new-env) (copy-sym-table (variables env)))
    (setf (functions new-env) (copy-sym-table (functions env)))
    (setf (declarations new-env) (declarations env))
    new-env))

(defun copy-sym-table (table)
  (let ((new-table (make-hash-table :test #'eq)))
    (iter (for (sym (type local decl)) in-hashtable table)
	  (setf (gethash sym new-table) (list* type local decl)))
    new-table))


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

(defun add-var-info (var decl env)
  (with-slots (variables) env
    (awhen (gethash var variables)
      (destructuring-bind (type local &rest info) it
	(setf (gethash var variables)
	      (list* type local (cons decl info)))))))

(defun make-var-special (var env)
  (with-slots (variables) env
    (let ((info (gethash var variables)))
      (let-if ((local (second info) t)
	       (decl (third info)))
	  info
	(setf (gethash var variables)
	      (list* :special local decl))))))

(defun get-var-info (var env)
  (gethash var (variables env)))


;;; CLTL2 Interface

(defun variable-information (variable &optional env)
  (let* ((ext-env (get-environment env))
	 (info (get-var-info variable ext-env)))
    (values (first info) (second info) (cddr info))))
