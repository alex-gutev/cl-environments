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
  "Walks the body of the BLOCK form (excluding the block name symbol)"

  (match-form (name &rest forms) args
    `(,name ,@(walk-forms forms))))

(defwalker cl:return-from (args)
  "Walks the result-form of the RETURN-FROM form."

  (match-form (name form) args
    `(,name ,(walk-form form))))


;;; CATCH

(defwalker cl:catch (args)
  "Walks the tag and body of the CATCH form."

  (check-list args
    (walk-forms args)))

(defwalker cl:throw (args)
  "Walks the result form of the THROW form."

  (match-form (tag result) args
    `(,(walk-form tag) ,(walk-form result))))


;;; EVAL-WHEN

(defwalker cl:eval-when (args)
  "Walks the body of the EVAL-WHEN form."
  
  (match-form (situation &rest forms) args
    (cons situation (walk-forms forms))))


;;; FUNCTION

(defwalker cl:function (args)
  "If the body of the FUNCTION form is a lambda expression, it is
   walked as a function definition. Otherwise the form arguments are
   returned as is."

  (match args
    ((list (list* 'cl:lambda expr))
     (list (cons 'cl:lambda (walk-fn-def expr (get-environment *env*)))))

    #+clisp
    ((list name (and (list* 'cl:lambda _) expr))
     (list name (second (walk-fn-form 'function (list expr)))))
    
    (_ args)))


;;; IF

(defwalker cl:if (args)
  "Walks the test, then and else forms."

  (check-list args
    (walk-forms args)))


;;; LOAD-TIME-VALUE

(defwalker cl:load-time-value (args)
  "Walks the value form in the global NIL environment."

  (match-form (form &optional read-only-p) args
    (let ((*env* nil))
      `(,(walk-form form) ,read-only-p))))


;;; LOCALLY

(defwalker cl:locally (args)
  "Encloses the body of the LOCALLY form in an environment, augmented
   with the declaration information."
  
  (let ((ext-env (copy-environment (get-environment *env*))))
    (walk-body args ext-env)))



;;; MULTIPLE-VALUE-CALL

(defwalker cl:multiple-value-call (args)
  "Walks all argument forms."

  (check-list args
    (walk-forms args)))


;;; MULTIPLE-VALUE-PROG1

(defwalker cl:multiple-value-prog1 (args)
  "Walks the result and body forms."

  (check-list args
    (walk-forms args)))


;;; PROGN

(defwalker cl:progn (args)
  "Walks the body forms."

  (check-list args
    (walk-forms args)))


;;; PROGV

(defwalker cl:progv (args)
  "Walks the symbols, values and body forms. Does not add anything to
   the lexical environment as the variable symbols are only known at
   run-time."

  (check-list args
    (walk-forms args)))


;;; QUOTE

(defwalker cl:quote (args)
  "Returns the arguments unchanged."

  args)


;;; SETQ

(defwalker cl:setq (args)
  "Walks the value forms."

  (check-list args
    (loop for (var form) on args by #'next-2
       nconc (list var (walk-form form)))))


;;; TAGBODY

(defwalker cl:tagbody (args)
  "Walks the body forms (excluding the tags)."

  (flet ((enclose-form (form)
	   (if (atom form)
	       form
	       (walk-form form))))
    
    (check-list args
      (mapcar #'enclose-form args))))


(defwalker cl:go (args)
  "Returns the argument as is."
  
  args)

;;; THE

(defwalker cl:the (args)
  "Walks the value form."

  (match-form (type form) args
    `(,type ,(walk-form form))))



;;; UNWIND-PROTECT

(defwalker cl:unwind-protect (args)
  "Walks the protected form and the cleanup forms."

  (check-list args
    (walk-forms args)))


;;; Clisp specific special forms

#+clisp
(defwalker system::function-macro-let (args)
  "Encloses the body of the form in an environment augmented with the
   lexical functions introduced by the form. The bodies of the
   functions are not walked as this form is only used internally by
   Clisp's implementation of CLOS."
  
  (match-form ((&rest fns) . body) args
    (let ((ext-env (copy-environment (get-environment *env*))))
      (loop for (fn) in fns do (add-function fn ext-env))
      `(,fns ,@(walk-body body ext-env)))))
