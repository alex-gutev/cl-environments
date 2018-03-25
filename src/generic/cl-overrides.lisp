;;;; cl-overrides.lisp
;;;;
;;;; Copyright 2018 Alexander Gutev
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


(eval-when (:compile-toplevel :load-toplevel :execute)  
  (defun add-form-macros (ops)
    (dolist (cl-op ops)
      (shadow cl-op :cl-environments)
      (setf (macro-function (intern (symbol-name cl-op) :cl-environments))
	    (lambda (form env)
	      (declare (ignore env))
	      (cons cl-op (rest form))))))

  (add-form-macros
   '(block
     catch
     eval-when
     flet
     function
     go
     if
     labels
     let
     let*
     load-time-value
     locally
     macrolet
     multiple-value-call
     multiple-value-prog1
     progn
     progv
     return-from
     setq
     symbol-macrolet
     tagbody
     the
     throw
     unwind-protect)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Shadow CL functions which take an optional environment parameter
  
  (shadow '(macro-function
	    macroexpand-1
	    macroexpand
	    get-setf-expansion
	    compiler-macro-function) :cl-environments))


;;; The functions implemented below simply call the CL functions,
;;; however if the environment passed is an `environment' object, the
;;; implementation specific environment stored in the
;;; LEXICAL-ENVIRONMENT slot is passed as the environment parameter
;;; instead.


(defun macro-function (symbol &optional env)
  (cl:macro-function symbol (lexical-environment env)))

(defun (setf macro-function) (fn symbol &optional env)
  (setf (cl:macro-function symbol (lexical-environment env)) fn))


(defun macroexpand-1 (form &optional env)
  (cl:macroexpand-1 form (lexical-environment env)))

(defun macroexpand (form &optional env)
  (cl:macroexpand form (lexical-environment env)))


(defun get-setf-expansion (place &optional env)
  (cl:get-setf-expansion place (lexical-environment env)))


(defun compiler-macro-function (name &optional env)
  (cl:compiler-macro-function name (lexical-environment env)))

(defun (setf compiler-macro-function) (fn name &optional (env nil env-sp))
  (if env-sp
      (setf (cl:compiler-macro-function name env) fn)
      (setf (cl:compiler-macro-function name) fn)))
