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
     (walk-list-form op args))

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

(defvar *walker-functions* (make-hash-table :test #'eq)
  "Hash-table mapping special operator symbols to their walker
   functions. The walker function is responsible for walking the
   arguments, according to the special operator's evaluation rules and
   returning a new form with an augmented environment if
   necessary. Walker functions can be added either using
   SET-WALKER-FUNCTION, SET-WALKER-FUNCTIONS or the DEFWALKER macro.")

(defun walk-list-form (operator args)
  "Walks a function call expression with function/macro/special
   operator OPERATOR and arguments ARGS."

  (aif (gethash operator *walker-functions*)
       (funcall it operator args)
       (walk-function-call operator args)))

(defun walk-function-call (function args)
  "Walks a function call expression which is not a recognized special
   form. If FUNCTION names a macro or a special operator, the form is
   simply returned unchanged (the arguments are not walked). If
   FUNCTION is neither a macro nor special operator it is assumed to
   be a function, all arguments are walked."

  (if (and (symbolp function) (macro-function function *env*))
      (cons function args)
      (walk-function function args)))


(defun walk-function (function args)
  "Walks the function call expression where FUNCTION does not name a
   macro function. If FUNCTION is a LAMBDA expression it is walked
   otherwise FUNCTION is left as is. The form arguments ARGS are
   walked, if FUNCTION is not a special operator."
  
  (flet ((walk-args (args)
	   (check-list args
	     (walk-forms args))))
    
    (match function
      ((cons 'cl:lambda _)
       (cons (second (walk-list-form 'function (list function))) (walk-args args)))

      ((type symbol)
       (if (special-operator-p function)
	   (cons function args) ; Cannot be walked
	   (cons function (walk-args args))))

      (_ (cons function args)))))


;;; Utilities for adding walker functions

(defun set-walker-function (operator function)
  "Sets the walker function for OPERATOR."

  (setf (gethash operator *walker-functions*) function))

(defun set-walker-functions (operators function)
  "Sets the walker function for each operator in the list OPERATORS to
   FUNCTION."

  (mapc (rcurry #'set-walker-function function) operators))

(defmacro! defwalker (op (arg-var) &body body)
  "Defines a walker function for the operator OP. ARG-VAR is bound to
   the arguments of the form. The forms in BODY, enclosed in an
   implicit PROGN, should return the new operator arguments. The
   result returned by the walker method is the form with the operator
   OP and the arguments returned by the last form in BODY,
   effectively (CONS ,OP (PROGN ,@BODY)). BODY is additionally
   surrounded in a RESTART-CASE, which establishes the restart
   SKIP-WALK (returns the arguments unchanged) and in a HANDLER-BIND
   which invokes the restart in the case of a WALK-PROGRAM-ERROR."
  
  (multiple-value-bind (body decl doc)
      (parse-body body :documentation t)
    (declare (ignore doc))

    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (set-walker-function ',op
			    (compile nil
				     (lambda (,g!op-var ,arg-var)
				       ,@decl
				       (declare (ignore ,g!op-var))
				       (cons ',op (skip-walk-errors
						    (restart-case (progn ,@body)
						      (skip-walk () ,arg-var))))))))))
     
