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
  (ignore-type-errors (forms)
    (mapcar #'enclose-form forms)))



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


;;; Code walker definition macro

(defmacro! defwalker (op (arg-var &optional (env-var (gensym))) &body body)
  "Defines a code-walker method for the operator OP. ARG-VAR is bound
   to the operator arguments and ENV-VAR is bound to the lexical
   environment in which the operator appears. The forms in BODY,
   enclosed in an implicit PROGN, should return the new operator
   arguments. The result returned by the walker method is the form
   with the operator OP and the arguments returned by the forms in
   BODY, effectively (CONS ,OP (PROGN ,@BODY))."
  
  (multiple-value-bind (body decl doc)
      (parse-body body :documentation t)
    
    `(defmethod walk-fn-form ((,g!op (eql ',op)) ,arg-var (,env-var t))
       ,doc ,@decl
       (cons ,g!op (ignore-type-errors (,arg-var) ,@body)))))
     
