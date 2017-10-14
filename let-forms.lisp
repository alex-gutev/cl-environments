;;;; let-forms.lisp
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

(defun walk-body (body ext-env &optional documentation)
  (multiple-value-bind (forms decl docstring)
      (parse-body body :documentation documentation)
    `(,@(ensure-list docstring)
      ,@(walk-declarations decl ext-env)
	,(enclose-in-env
	  ext-env
	  (enclose-forms forms)))))


(defmethod walk-fn-form ((op (eql 'cl:let*)) args env)
  (destructuring-bind (bindings . body) args
    (multiple-value-bind (bindings env)
	(walk-plet-bindings bindings (copy-environment (get-environment env)))
      `(cl:let* ,bindings
	 ,@(walk-body body env)))))

;;; Declarations has to be extracted before walking bindings
(defun walk-plet-bindings (bindings env)
  (flet ((enclose-binding (binding env)
	   (match binding
	     ((list var initform)
	      `(,var ,(enclose-in-env env (list (enclose-form initform)))))
	     (_ binding))))
    (iter
      (for ext-env initially env then (copy-environment ext-env))
      (for binding in bindings)
      (add-variable (ensure-car binding) ext-env)
      (collect (enclose-binding binding ext-env) into new-bindings)
      (finally (return (values new-bindings ext-env))))))


;;; Destructuring Bind

(defmethod walk-fn-form ((op (eql 'cl:destructuring-bind)) args env)
  (let ((env (get-environment env)))
    (destructuring-bind (lambda-list form . body) args
      (multiple-value-bind (lambda-list env)
	  (walk-lambda-list lambda-list env :destructure t)
	`(cl:destructuring-bind ,lambda-list ,form
	   ,@(walk-body body env))))))


;;; Lambda

(defmethod walk-fn-form ((op (eql 'cl:lambda)) args env)
  (let ((env (get-environment env)))
    (destructuring-bind (lambda-list . body) args
      (multiple-value-bind (lambda-list env)
	  (walk-lambda-list lambda-list env)
	`(cl:lambda ,lambda-list
	   ,@(walk-body body env))))))



;;; Lexical functions

(defmethod walk-fn-form ((op (eql 'cl:flet)) args env)
  (let* ((env (get-environment env))
	 (new-env (copy-environment env)))
    (destructuring-bind (fns . body) args
      `(flet ,(mapcar (rcurry #'walk-local-fn env new-env) fns)
	 ,@(walk-body body new-env)))))

;; TODO:
;;
;; inline/notinline declarations have to be extracted from body and
;; applied to the bodies of the functions defined by the labels form.
(defmethod walk-fn-form ((op (eql 'cl:labels)) args env)
  (let ((env (copy-environment (get-environment env))))
    (flet ((walk-local-fn (def)
	     (cons (first def) (walk-fn-def def env))))
      (destructuring-bind (fns . body) args
	(mapc (compose (rcurry #'add-function env) #'first) fns)
	`(cl:labels ,(mapcar #'walk-local-fn fns)
	   ,@(walk-body body env))))))


(defun walk-local-fn (def fn-env new-env)
  (destructuring-bind (name . def) def
    (add-function name new-env)
    `(,name ,@(walk-fn-def def fn-env))))

(defun walk-fn-def (def env)
  (destructuring-bind (lambda-list . body) def
    (multiple-value-bind (lambda-list env)
	(walk-lambda-list lambda-list env)
      `(,lambda-list
	,@(walk-body body env t)))))


;;; Lexical Macros

(defmethod walk-fn-form ((op (eql 'cl:macrolet)) args env)
  (destructuring-bind (macros . body) args
    (let* ((env (get-environment env))
	   (new-env (copy-environment env)))
      `(cl:macrolet ,(mapcar (rcurry #'walk-local-macro env new-env) macros)
	 ,@(walk-body body new-env)))))

(defun walk-local-macro (def mac-env new-env)
  (destructuring-bind (name . def) def
    (add-function name new-env :type :macro)
    `(,name ,@(walk-macro-def def mac-env))))

(defun walk-macro-def (def env)
  (destructuring-bind (lambda-list . body) def
    (multiple-value-bind (lambda-list env)
	(walk-lambda-list lambda-list env :destructure t :env t)
      `(,lambda-list
	,@(walk-body body env t)))))


;;; Lexical Symbol Macros

(defmethod walk-fn-form ((op (eql 'cl:symbol-macrolet)) args env)
  (destructuring-bind (macros . body) args
    (let ((env (copy-environment (get-environment env))))
      (mapc (compose (rcurry #'add-symbol-macro env) #'first) macros)
      `(cl:symbol-macrolet ,macros
	 ,@(walk-body body env)))))
