;;;; cltl2-interface.lisp
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

;;;; Implements the environment access API described in Common Lisp
;;;; the Language 2 (CLTL2)

(in-package :cl-environments)

(defun variable-information (variable &optional env)
  "Returns information about the variable binding for the symbol
   VARIABLE, in the environment ENV. ENV is the implementation
   specific lexical environment, passed as the &ENVIRONMENT parameter
   to macros, not the extended environment object. Returns three
   values: the first value is the binding type
   nil, :SPECIAL, :LEXICAL, :SYMBOL-MACRO or :CONSTANT, the second
   value is true if there is a local binding and the third value is
   the association list of the declaration information."

  (typecase env
    (environment
     (let ((ext-env (get-environment env)))
       (slot-values (type local declarations)
	   (variable-binding variable ext-env)
	 (values type local declarations))))

    (otherwise (values nil nil nil))))

(defun function-information (function &optional env)
  "Returns information about the function binding for the symbol
   FUNCTION in the environment ENV. ENV is the implementation specific
   lexical environment, passed as the &ENVIRONMENT parameter to
   macros, not the extended environment object. Returns three values:
   the first value is the binding type nil, :FUNCTION, :MACRO
   or :SPECIAL-FORM, the second value is true if there is a local
   binding and the third value is the association list of the
   declaration information."

  (typecase env
    (environment
  
     (let ((ext-env (get-environment env)))
       (slot-values (type local declarations)
	   (function-binding function ext-env)
	 (values type local declarations))))

    (otherwise (values nil nil nil))))


(defun declaration-information (decl-name &optional env)
  "Returns information about the declaration DECL-NAME in the
   environment ENV."

  (when (typep env 'environment)
    (declaration-info decl-name (get-environment env))))


;;; Augmenting Environments

(defgeneric augment-environment (env &key variable symbol-macro function macro declare))

(defmethod augment-environment (env &key variable symbol-macro function macro declare)
  (aprog1
      (augment-environment (get-environment env)
			   :variable variable
			   :symbol-macro symbol-macro
			   :function function
			   :macro macro
			   :declare declare)
    (setf (lexical-environment it) env)))


;;; TODO: According to CLTL2 (DECLARE (TYPE ... X)) where X is a symbol
;;; macro should result in the symbol macro form being enclosed in a
;;; THE form

(defmethod augment-environment ((env environment) &key variable symbol-macro function macro declare)
  (when (or (set-difference variable symbol-macro)
	    (set-difference function macro))
    (error 'program-error))
  
  (let ((new-env (copy-environment env)))
    (mapc (rcurry #'add-variable new-env) variable)
    (mapc (rcurry #'add-function new-env) function)

    (loop for (decl . args) in declare
       do
	 (when (and (eq decl 'special)
		    (set-difference args symbol-macro))
	   (error 'program-error))
	   
	 (walk-declaration decl args new-env))
    
    (loop for (name def) in macro
       do
	 (add-function name new-env :type :macro :local t)
	 (setf (macro-fn name new-env) def))

    (loop for (name def) in symbol-macro
       do
	 (add-symbol-macro name new-env :local t)
	 (setf (macro-form name new-env) def))))

(defun! parse-macro (name lambda-list body &optional env)
  ;; TODO: Add greater error checking for arguments themselves

  (let ((env-arg (gensym "ENV")))
    (flet ((map-arg (type arg)
	     (cond
	       ((eq type 'environment)
		(setf env-arg arg)
		nil)
	       
	       ((not (eq arg '&environment))
		arg))))

      (let ((list (map-lambda-list #'map-arg lambda-list :destructure t :env t)))
	(enclose
	 `(lambda (,g!whole ,env-arg)
	    (declare (ignorable ,g!whole ,env-arg))
	    (block ,name
	      (destructuring-bind ,list ,g!whole
		,@body)))
	 env)))))


(defun enclose (lambda-expr &optional env)
  ;; Macro expand body of lambda-expr entirely
  lambda-expr
  )


(defmacro define-declaration (decl-name lambda-list &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (declaim (declaration ,decl-name))
     (setf (declaration-function ,decl-name *global-environment*)
	   (lambda ,lambda-list ,@body))
     ,decl-name))
