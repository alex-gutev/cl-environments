;;;; sbcl.lisp
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-cltl2))

(defpackage :cl-environments.cltl2
  (:use :common-lisp
	:sb-cltl2
	:alexandria
	:anaphora
	:iterate
	:optima

	:cl-environments.util)

  (:shadow :define-declaration :declaration-information :augment-environment)

  (:export :variable-information
	   :function-information
	   :declaration-information
	   :define-declaration

	   :augment-environment
	   :enclose
	   :parse-macro
	   :enclose-macro

	   :in-environment
	   :augmented-macroexpand-1
	   :augmented-macroexpand
	   :augmented-macro-function
	   :augmented-get-setf-expansion
	   :augmented-compiler-macro-function
	   :augmented-constantp

	   :enable-hook
	   :disable-hook

	   :walk-environment))

(defpackage :cl-environments-cl
  (:nicknames :cl-environments)
  (:use :common-lisp
	:cl-environments.util
	:cl-environments.cltl2)

  (:export :variable-information
	   :function-information
	   :declaration-information
	   :define-declaration

	   :augment-environment
	   :enclose
	   :enclose-macro

	   :in-environment
	   :augmented-macroexpand-1
	   :augmented-macroexpand
	   :augmented-macro-function
	   :augmented-get-setf-expansion
	   :augmented-compiler-macro-function
	   :augmented-constantp

	   :enable-hook
	   :disable-hook

	   :walk-environment))

(in-package :cl-environments.cltl2)


;; SBCL includes declaration name in arguments

(defmacro define-declaration (decl-name (arg-var &optional (env-var (gensym "ENV"))) &body body)
  (with-gensyms (args)
    `(sb-cltl2:define-declaration ,decl-name (,args ,env-var)
       (declare (ignorable ,env-var))
       (let ((,arg-var (rest ,args)))
	 (multiple-value-call #'wrap-declaration-result (progn ,@body))))))

(defun wrap-declaration-result (type value)
  "Wrap user-define declarations (TYPE = :DECLARE) in a list to
   silence errors due to SBCL bug."

  (values
   type
   (case type
     (:declare
      (cons (car value) (list 'user-declaration (cdr value))))

     (otherwise
      value))))

(defun declaration-information (name &optional env)
  (match (sb-cltl2:declaration-information name env)
    ((list 'user-declaration value)
     value)

    (value value)))

;;; SBCL's AUGMENT-ENVIRONMENT errors out when augmenting an
;;; environment with type information for a variable which is not
;;; simultaneously present in the :VARIABLE argument, even if the
;;; environment being augmented does contain a binding for the
;;; variable.
;;;
;;; This wrapper function handles that case by adding variables, and
;;; functions, appearing in type declarations to the :VARIABLE and
;;; :FUNCTION lists.

(defun augment-environment (env &key variable symbol-macro function macro declare)
  (labels ((extract-var (decl)
	     "Extract variable names from TYPE declarations."

	     (match decl
	       ((list* 'type _
		       (guard vars
			      (and (proper-list-p vars)
				   (every #'symbolp vars))))
		vars)))

	   (function-name-p (name)
	     (match name
	       ((or (type symbol)
		    (list 'cl:setf (type symbol)))
		t)))

	   (extract-func (decl)
	     "Extract function names from FTYPE declarations."

	     (match decl
	       ((list* 'ftype _
		       (guard fns
			      (and (proper-list-p fns)
				   (every #'function-name-p fns))))
		fns)))

           (decl-special-var (var)
             (when (eq :special (variable-information var env))
               `((special ,var)))))

    (let ((decl-vars (set-difference (mappend #'extract-var declare)
                                     symbol-macro))
          (decl-fns (set-difference (mappend #'extract-func declare)
                                    macro)))

      (sb-cltl2:augment-environment
       env
       :variable (union decl-vars variable)

       :function (union decl-fns function)

       :symbol-macro symbol-macro
       :macro macro
       :declare (append (mappend #'decl-special-var decl-vars)
                        declare)))))

(defun enclose-macro (name lambda-list body &optional env)
  (enclose (parse-macro name lambda-list body env) env))

(defmacro in-environment ((env-var &optional (environment env-var)) (&rest bindings) &body forms)
  (flet ((make-binding (binding)
	   (match binding
	     ((type symbol)
	      (list binding binding))

	     (_ binding))))

    `(let ((,env-var ,environment) ,@(mapcar #'make-binding bindings))
       ,@forms)))

(defun augmented-macroexpand-1 (form &optional env)
  (macroexpand-1 form env))

(defun augmented-macroexpand (form &optional env)
  (macroexpand form env))

(defun augmented-macro-function (name &optional env)
  (macro-function name env))

(defun augmented-get-setf-expansion (form &optional env)
  (get-setf-expansion form env))

(defun augmented-compiler-macro-function (name &optional environment)
  (compiler-macro-function name environment))

(defun augmented-constantp (form &optional environment)
  (constantp form environment))


(defun enable-hook (&optional previous-hook)
  "Does nothing, provided for compatibility with implementations where
   the code walker is required."

  (declare (ignore previous-hook)))

(defun disable-hook (&optional previous-hook)
  "Does nothing, provided for compatibility with implementations where
   the code walker is required."

  (declare (ignore previous-hook)))

(defmacro walk-environment (&body forms)
  `(progn ,@forms))

(defmacro disable-walker (&body body)
  `(progn ,@body))


(in-package :cl-environments-cl)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (reexport-all-symbols :cl))
