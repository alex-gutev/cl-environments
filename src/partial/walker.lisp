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

(defvar *env* nil
  "The implementation-specific environment in which the form,
   currently being walked, occurs.")

;;; Code-Walker Macro

(defmacro %walk-form (form &environment *env*)
  "Code-walker macro, simply invokes the WALK-FORM function. This
   macro is used when FORM needs to be walked in an augmented
   environment."
  
  (walk-form form))

(defun enclose-form (form)
  "Encloses FORM in the code-walker macro."

  (match form
    ((not (list* '%walk-form _))
     `(%walk-form ,form))
    
    (_ form)))

(defun enclose-forms (forms)
  "Encloses each form, in FORMS, in the code-walker macro."
  
  (mapcar #'enclose-form forms))


;;; Code-Walker function

(defun walk-form (form)
  "Walks the form FORM and its sub-forms, enclosing them in an
   augmented environment if necessary."

  (match form
    ((cons op args)
     (walk-fn-form op args))

    (_ (walk-atom-form form))))

(defun walk-forms (forms)
  "Walks each form in FORMS."
  
  (mapcar #'walk-form forms))


;;; Walking atom forms

(defun walk-atom-form (form)
  "Walks atom forms. If the form is a symbol-macro, it is expanded and
   the result is walked otherwise FORM is returned as is."
  
  (multiple-value-bind (form expanded-p) (macroexpand-1 form *env*)
    (if expanded-p
	(walk-form form)
	form)))


;;; Walking function call forms

(defgeneric walk-fn-form (op args)
  (:documentation
   "Walks a function call expression with function/macro/special
    operator OP and arguments ARGS."))

(defmethod walk-fn-form (op args)
  "Walks a function call expression which is not one of the recognized
   CL special forms. If OP names a macro it is expanded and the result
   is walked. If OP is a special operator the function call expression
   is simply returned as is (the arguments are not walked). If OP is
   neither a macro nor special operator it is assumed to be a
   function, all arguments are walked."
  
  (multiple-value-bind (form expanded-p) (macroexpand-1 (cons op args) *env*)
    (if expanded-p
	form ; Not walked as that should be already done by *macroexpand-hook*
	(walk-function op args))))

(defun walk-function (op args)
  "Walks the function call expression where OP names an ordinary
   function. If op is a LAMBDA expression it is walked otherwise OP is
   left as is. The form arguments ARGS are walked."
  
  (flet ((walk-args (args)
	   (check-list args
	     (walk-forms args))))
    
    (match op
      ((cons 'cl:lambda _)
       (cons (second (walk-fn-form 'function (list op))) (walk-args args)))

      ((type symbol)
       (if (special-operator-p op)
	   (cons op args) ; Cannot be walked
	   (cons op (walk-args args))))

      (_ (cons op args)))))

;;; Code walker definition macro

(defmacro! defwalker (op (arg-var) &body body)
  "Defines a code-walker method for the operator OP. ARG-VAR is bound
   to the operator arguments and ENV-VAR is bound to the lexical
   environment in which the operator appears. The forms in BODY,
   enclosed in an implicit PROGN, should return the new operator
   arguments. The result returned by the walker method is the form
   with the operator OP and the arguments returned by the last form in
   BODY, effectively (CONS ,OP (PROGN ,@BODY))."
  
  (multiple-value-bind (body decl doc)
      (parse-body body :documentation t)
    
    `(defmethod walk-fn-form ((,g!op (eql ',op)) ,arg-var)
       ,@(ensure-list doc) ,@decl

       (cons ,g!op (skip-walk-errors
		     (restart-case (progn ,@body)
		       (skip-walk () ,arg-var)))))))
     
