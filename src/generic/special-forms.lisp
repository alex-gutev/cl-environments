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

(defwalker cl:block (args env)
  "Walks the body of the BLOCK form (excluding the block name symbol)"

  (match-form (name &rest forms) args
    `(,name ,@(walk-forms forms env))))

(defwalker cl:return-from (args env)
  "Walks the result-form of the RETURN-FROM form."

  (match-form (name form) args
    `(,name ,(walk-form form env))))


;;; CATCH

(defwalker cl:catch (args env)
  "Walks the tag and body of the CATCH form."

  (check-list args
    (walk-forms args env)))

(defwalker cl:throw (args env)
  "Walks the result form of the THROW form."

  (match-form (tag result) args
    `(,(walk-form tag env) ,(walk-form result env))))


;;; EVAL-WHEN

(defwalker cl:eval-when (args env)
  "Walks the body of the EVAL-WHEN form."
  
  (match-form (situation &rest forms) args
    (cons situation (walk-forms forms env))))


;;; FUNCTION

(defwalker cl:function (args env)
  "If the body of the FUNCTION form is a lambda expression, it is
   walked as a function definition. Otherwise the form arguments are
   returned as is."

  (match args
    ((list (list* 'cl:lambda expr))
     (list (cons 'cl:lambda (walk-fn-def expr (enclose-environment (get-environment env) env)))))

    #+clisp
    ((list name (list* 'cl:lambda expr))
     (list name
	   (cons 'cl:lambda
		 (walk-fn-def expr (enclose-environment (get-environment env) env)))))
    
    (_ args)))


;;; IF

(defwalker cl:if (args env)
  "Walks the test, then and else forms."

  (check-list args
    (walk-forms args env)))


;;; LOAD-TIME-VALUE

(defwalker cl:load-time-value (args)
  "Walks the value form in the global NIL environment."

  (match-form (form &optional read-only-p) args
    `(,(walk-form form nil) ,read-only-p)))


;;; LOCALLY

(defwalker cl:locally (args env)
  "Encloses the body of the LOCALLY form in an environment, augmented
   with the declaration information."
  
  (let ((ext-env (copy-environment (get-environment env) env)))
    (walk-body args ext-env)))



;;; MULTIPLE-VALUE-CALL

(defwalker cl:multiple-value-call (args env)
  "Walks all argument forms."

  (check-list args
    (walk-forms args env)))


;;; MULTIPLE-VALUE-PROG1

(defwalker cl:multiple-value-prog1 (args env)
  "Walks the result and body forms."

  (check-list args
    (walk-forms args env)))


;;; PROGN

(defwalker cl:progn (args env)
  "Walks the body forms."

  (check-list args
    (walk-forms args env)))


;;; PROGV

(defwalker cl:progv (args env)
  "Walks the symbols, values and body forms. Does not add anything to
   the lexical environment as the variable symbols are only known at
   run-time."

  (check-list args
    (walk-forms args env)))


;;; QUOTE

(defwalker cl:quote (args)
  "Returns the arguments unchanged."

  args)


;;; SETQ

(defwalker cl:setq (args env)
  "Walks the value forms."

  (check-list args
    (loop for (var form) on args by #'next-2
       nconc (list var (walk-form form env)))))


;;; TAGBODY

(defwalker cl:tagbody (args env)
  "Walks the body forms (excluding the tags)."

  (flet ((enclose-form (form)
	   (if (atom form)
	       form
	       (walk-form form env))))
    
    (check-list args
      (mapcar #'enclose-form args))))


(defwalker cl:go (args)
  "Returns the argument as is."
  
  args)

;;; THE

(defwalker cl:the (args env)
  "Walks the value form."

  (match-form (type form) args
    `(,type ,(walk-form form env))))



;;; UNWIND-PROTECT

(defwalker cl:unwind-protect (args env)
  "Walks the protected form and the cleanup forms."

  (check-list args
    (walk-forms args env)))


;;; Clisp specific special forms

#+clisp
(defwalker system::function-macro-let (args env)
  "Encloses the body of the form in an environment augmented with the
   lexical functions introduced by the form. The bodies of the
   functions are not walked as this form is only used internally by
   Clisp's implementation of CLOS."
  
  (match-form ((&rest fns) . body) args
    (let ((ext-env (copy-environment (get-environment env) env)))
      (loop for (fn) in fns do (add-function fn ext-env))
      `(,fns ,@(walk-body body ext-env)))))
