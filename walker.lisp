;;;; walker.lisp
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

(in-package :cl-environments)

;;; Code Walker Interface

(defmacro walk-form-macro (form &environment env)
  (walk-form form env))

(defun enclose-form (form)
  `(walk-form-macro ,form))

(defun enclose-forms (forms)
  (mapcar #'enclose-form forms))



(defun walk-form (form env)
  (match form
    ((cons op args)
     (walk-fn-form op args env))

    (_ (walk-atom-form form env))))


;;; Walking symbol forms

(defgeneric walk-atom-form (form env))

(defmethod walk-atom-form ((form symbol) env)
  (multiple-value-bind (form expanded-p) (macroexpand-1 form env)
    (if expanded-p
	(enclose-form form)
	form)))

(defmethod walk-atom-form (form (env t))
  form)



;;; Walking function call forms

(defgeneric walk-fn-form (op args env))

(defmethod walk-fn-form (op args env)
  (multiple-value-bind (form expanded-p) (macroexpand-1 (cons op args) env)
    (cond
      (expanded-p
       (enclose-form form))
      ((special-operator-p op)
       (cons op args)) ; Cannot be walked
      (t
       (cons op (enclose-forms args))))))


;;; Let forms

(defmethod walk-fn-form ((op (eql 'cl:let)) args env)
  (destructuring-bind (bindings . body) args
    (let* ((old-env (get-environment env))
	   (new-env (copy-environment old-env)))
      `(let ,(walk-let-bindings bindings new-env)
	 ,@(walk-body body new-env)))))

(defun walk-let-bindings (bindings env)
  (flet ((enclose-binding (binding)
	   (match binding
	     ((list var initform)
	      `(,var ,(enclose-form initform)))
	     (_ binding))))
    (iter (for binding in bindings)
	  (add-variable (ensure-car binding) env)
	  (collect (enclose-binding binding)))))

(defun walk-body (body ext-env)
  (multiple-value-bind (forms decl) (parse-body body)
    `(,@(walk-declarations decl ext-env)
	,(enclose-in-env
	  ext-env
	  (enclose-forms forms)))))


;;; Declarations



