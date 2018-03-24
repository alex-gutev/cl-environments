;;;; special-forms.lisp
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

;;;; Code-walkers for the standard Common Lisp forms, which are not
;;;; macros, excluding the lexical binding forms (LET, FLET, etc)
;;;; which are implemented in let-forms.lisp.

;;; BLOCK

(defwalker cl:block (args)
  "Walks CATCH forms. Encloses the body of the BLOCK form (excluding
   the block name symbol) in the code walking macro."

  (match-form (name &rest forms) args
    `(,name ,@(enclose-forms forms))))

(defwalker cl:return-from (args)
  "Walks RETURN-FROM forms. Encloses the result expression form in the
   code-walking macro."

  (match-form (name form) args
    `(,name ,@(enclose-form form))))


;;; CATCH

(defwalker cl:catch (args)
  "Walks CATCH forms. Encloses the CATCH tag form and
   body in the code-walking macro."

  (check-list args
    (enclose-forms args)))

(defwalker cl:throw (args)
  "Walks THROW forms. Encloses the CATCH tag form and result form in
   the code-walking macro."

  (match-form (tag result) args
    `(,(enclose-form tag) ,(enclose-form result))))


;;; EVAL-WHEN

(defwalker cl:eval-when (args)
  "Walks EVAL-WHEN forms. Encloses the body in the code-walking
   macro."
  
  (match-form (situation &rest forms) args
    (cons situation (enclose-forms forms))))


;;; FUNCTION

(defwalker cl:function (args env)
  "Walks FUNCTION forms. If the body of the form is a LAMBDA
   expression, it is walked directly (i.e. not enclosed in the
   code-walking macro as the function form does not expand macros). If
   the body of the form is not a LAMBDA expression, the entire form is
   simply returned as is."

  (match args
    ((list* 'cl:lambda expr)
     (cons 'cl:lambda (walk-fn-def expr (enclose-environment (get-environment env) env))))
    (_ args)))


;;; IF

(defwalker cl:if (args)
  "Walks IF forms. Encloses all body forms in the code-walking macro."

  (check-list
    (enclose-forms args)))


;;; LOAD-TIME-VALUE

(defwalker cl:load-time-value (args)
  "Walks LOAD-TIME-VALUE forms. Encloses the form in the global
   environment."

  (match-form (form &optional read-only-p) args
    `(,(enclose-in-env *global-environment* (list form)) ,read-only-p)))


;;; LOCALLY

(defwalker cl:locally (args env)
  "Walks LOCALLY forms. Creates a copy of the extended lexical
   environment object in ENV, which is augmented with the declaration
   information and encloses the body of the form in this environment."
  
  (let ((ext-env (copy-environment (get-environment env) env)))
    (walk-body args ext-env)))



;;; MULTIPLE-VALUE-CALL

(defwalker cl:multiple-value-call (args)
  "Walks MULTIPLE-VALUE-CALL forms. Encloses the function form and the
   argument forms in the code-walking macro."

  (check-list args
    (enclose-forms args)))


;;; MULTIPLE-VALUE-PROG1

(defwalker cl:multiple-value-prog1 (args)
  "Walks MULTIPLE-VALUE-PROG1 forms. Encloses the expression form and
   body forms in the code-walking macro."

  (check-list args
    (enclose-forms args)))


;;; PROGN

(defwalker cl:progn (args)
  "Walks PROGN forms. Encloses the body of the PROGN in the code
   walking macro."

  (check-list args
    (enclose-forms args)))


;;; PROGV

(defwalker cl:progv (args)
  "Walks PROGV forms. Encloses the symbols list form, the values list
   form, and the body forms in the code-walking macro. Does not add
   anything to the lexical environment as the variable symbols are
   only known at run-time."

  (check-list args
    (enclose-forms args)))


;;; QUOTE

(defwalker cl:quote (args)
  "Walks QUOTE forms. Simply returns the form unchanged."

  args)


;;; SETQ

(defwalker cl:setq (args)
  "Walks SETQ forms. Encloses the value forms in the code walking
   macros. Does not expand symbol macros occurring in the variable
   symbol positions."

  (check-list args
    (loop for (var form) on args by #'next-2
       nconc (list var (enclose-form form)))))


;;; TAGBODY

(defwalker cl:tagbody (args)
  "Walks TAGBODY forms. Encloses function forms, in the body of the
   TAGBODY, in the code-walking macro. Does not enclose atom forms."

  (flet ((enclose-form (form)
	   (if (atom form)
	       form
	       (enclose-form form))))
    
    (check-list args
      (mapcar #'enclose-form args))))


(defwalker cl:go (args)
  "Walks GO forms. Returns the form as is."
  
  args)

;;; THE

(defwalker cl:the (args)
  "Walks THE forms. Encloses the form in the code-walking macro."

  (match-form (type form) args
    `(,type ,(enclose-form form))))



;;; UNWIND-PROTECT

(defwalker cl:unwind-protect (args)
  "Walks UNWIND-PROTECT forms. Encloses the protected form and cleanup
   forms in the code-walking macro."

  (check-list args
    (enclose-forms args)))
