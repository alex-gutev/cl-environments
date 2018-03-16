;;;; macroexpand-all.lisp
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

(defun macroexpand-all (form &optional env)
  (match form
    ((cons op args)
     (expand-fn-expr op args env))
    
    (_
     (expand-atom form env))))

(defun expand-forms (forms &optional env)
  (mapcar (rcurry #'macroexpand-all env) forms))

(defun expand-atom (atom env)
  (multiple-value-bind (form expanded-p) (macroexpand-1 atom env)
    (if expanded-p
	(macroexpand-all form env)
	form)))


;;; Function expressions

(defgeneric expand-fn-expr (op args env)
  (:method (op args env)
    (multiple-value-bind (form expanded-p) (macroexpand-1 (cons op args) env)
      (if expanded-p
	  (macroexpand-all form env)
	  (expand-fn-args op args env)))))

(defun expand-fn-args (op args env)
  (if (special-operator-p op)
      (cons op args)
      (cons op (expand-forms args env))))


(defmacro! defexpander (op (args &optional (env (gensym "ENV"))) &body body)
  (multiple-value-bind (body decl doc)
      (parse-body body :documentation t)

    `(defmethod expand-fn-expr ((,g!op (eql ',op)) ,args (,env t))
       ,doc ,@decl

       (flet ((call-next-expander ()
		(call-next-method)))
	 (cons ,g!op (progn ,@body))))))


;;; BLOCK

(defexpander cl:block (args env)
  (match-form (name . forms) args
    `(,name ,@(expand-forms forms env))))

(defexpander cl:return-from (args env)
  (match-form (name form) args
    `(,name ,@(macroexpand-all form env))))


;;; CATCH

(defexpander cl:catch (args env)
  (expand-forms args env))

(defexpander cl:throw (args env)
  (match-form (tag result) args
    `(,(macroexpand-all tag env) ,(macroexpand-all result env))))


;;; EVAL-WHEN

(defexpander cl:eval-when (args env)
  (match-form (situation . forms) args
    `(,situation ,@(expand-forms forms env))))


;;; FLET

(defexpander cl:flet (args env)
  (flet ((macroexpand-fn (fn)
	   (match-form (name . def) fn
	     `(,name ,@(expand-fn-def def env t)))))

    (match-form ((&rest fns) . body) args
      `(,(mapcar #'macroexpand-fn fns)
	 ,@(expand-forms body)))))

(defun expand-fn-def (def env &optional documentation)
  (match-form ((&rest lambda-list) . body) def
    (multiple-value-bind (lambda-list env) (expand-lambda-list lambda-list env)
      `(,lambda-list ,@(expand-body body env documentation)))))

(defun expand-lambda-list (list env)
  (flet ((add-var (var)
	   (setf env (augment-environment env :variable (list var)))))
    (values
     (map-lambda-list
      (lambda (type arg)
	(multiple-value-match (values type arg)
	  (((or 'required 'rest) (type symbol))
	   (add-var arg)
	   arg)
	  
	  (('optional
	    (or (list var initform var-sp)
		(list var initform)
		(list var)
		var))
	   
	   (add-var var)
	   (when var-sp (add-var var-sp))

	   `(,var ,(macroexpand-all initform env) ,var-sp))

	  (('key
	    (or (cons (and (or (list keyword var)
			       var) arg)
		      (or
		       (list initform var-sp)
		       (list initform)
		       nil))))

	   (add-var var)
	   (when var-sp (add-var var-sp))

	   `(,arg ,(macroexpand-all initform env) ,var-sp))

	  (('aux
	    (or (list var initform)
		(list var)
		var))

	   (add-var var)
	   `(var ,(macroexpand-all initform env)))))
      list)
     env)))

(defun expand-body (body env &optional documentation)
  (multiple-value-bind (body decl doc)
      (parse-body body :documentation documentation)

    (let ((env (augment-environment env :declare (mappend #'rest decl))))
      `(,@(ensure-list doc) ,@decl
	  ,@(expand-forms body env)))))

(defexpander cl:labels (args env)
  (expand-fn-expr 'cl:flet args env))


;;; FUNCTION

(defexpander cl:function (args env)
  (match args
    ((list* 'cl:lambda def)
     (cons 'cl:lambda (expand-fn-def def env)))

    (_ args)))


;;; IF

(defexpander cl:if (args env)
  (expand-forms args env))


;;; LOAD-TIME-VALUE

(defexpander cl:load-time-value (args env)
  (match args
    ((or (list form read-only-p)
	 (list form))
     `(,(macroexpand-all form env) ,read-only-p))
    (_ args)))


;;; LOCALLY

(defexpander cl:locally (args env)
  (expand-body args env))


;;; MACROLET

(defexpander cl:macrolet (args env)
  (flet ((parse-macro (macro)
	   (match-form (name (&rest lambda-list) . body) macro
	     (list name (parse-macro name lambda-list body env)))))
    (match-form ((&rest macros) . body) args
      (let ((env (augment-environment env :macro (mapcar #'parse-macro macros))))
	(expand-body body env)))))

(defexpander cl:symbol-macrolet (args env)
  (match-form ((&rest macros) . body) args
    (let ((env (augment-environment env :symbol-macro macros)))
      (expand-body body env))))
    


;;; MULTIPLE-VALUE-CALL

(defexpander cl:multiple-value-call (args env)
  (expand-forms args env))


;;; MULTIPLE-VALUE-PROG1

(defexpander cl:multiple-value-prog1 (args env)
  (expand-forms args env))


;;; PROGN

(defexpander cl:progn (args env)
  (expand-forms args env))


;;; PROGV

(defexpander cl:progv (args env)
  (expand-forms args env))


;;; QUOTE

(defexpander cl:quote (args)
  args)


;;; SETQ

;;; Has to be converted to SETF if any of the vars is a symbol macro

(defmethod expand-fn-expr ((op (eql 'cl:setq)) args env)
  ;; Check that list is of even length
  (let ((args (loop for (var form) on args by #'cddr
		 nconc (list var (macroexpand-all form env))
		 do
		   (when (nth-value 1 (macroexpand var env))
		     (setf op 'cl:setf)))))
    (if (eq op 'cl:setf)
	(macroexpand-all (cons op args) env)
	(cons op args))))


;;; TAGBODY

(defexpander cl:tagbody (args env)
  (loop for form in args
     collect
       (if (atom form)
	   form
	   (macroexpand-all form env))))

(defexpander cl:go (args)
  args)


;;; THE

(defexpander cl:the (args env)
  (match-form (type form) args
    `(,type ,(macroexpand-all form env))))


;;; UNWIND-PROTECT

(defexpander cl:unwind-protect (args env)
  (expand-forms args env))
