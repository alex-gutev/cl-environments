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

;;;; Implementation of a portable (excluding implementation specific
;;;; special forms) MACROEXPAND-ALL function using the environment
;;;; access interface implemented.

(in-package :cl-environments)


(defun macroexpand-all (form &optional env)
  "Expands all macro forms occurring in FORM."
  
  (match form
    ((cons op args)
     (expand-fn-expr op args env))
    
    (_
     (expand-atom form env))))

(defun expand-forms (forms &optional env)
  "Expands all macros (by use of MACROEXPAND-ALL) occurring in each
   form in FORMS."
  
  (check-list forms
    (mapcar (rcurry #'macroexpand-all env) forms)))

(defun expand-atom (atom env)
  "Expands symbol macros if ATOM is a symbol naming a symbol macro. If
   ATOM does not name a symbol macro or it is not a symbol it is
   simply returned."
  
  (multiple-value-bind (form expanded-p) (macroexpand-1 atom env)
    (if expanded-p
	(macroexpand-all form env)
	form)))


;;; Function expressions

(defgeneric expand-fn-expr (op args env)
  (:documentation
   "Expands all macros in a function call/macro/special-form
    expression where OP is the first element of the list (the
    operator) and ARGS is the rest of the elements of the list (the
    arguments). If OP names a Common Lisp special form, macros are
    only expanded in the arguments which are treated as ordinary CL
    forms to be evaluated.")
  
  (:method (op args env)
    "Default method for when OP does not name a standard CL form (or a
     known implementation-specific special form). If OP names a macro,
     in the environment ENV, it is expanded first, if OP names a
     function the arguments (ARGS) are expanded."
    
    (multiple-value-bind (form expanded-p) (macroexpand-1 (cons op args) env)
      (if expanded-p
	  (macroexpand-all form env)
	  (expand-fn-args op args env)))))

(defun expand-fn-args (op args env)
  "Expands the arguments ARGS to the function call expression OP. If
   OP is a special operator (determined by SPECIAL-OPERATOR-P) the
   form, (CONS OP ARGS), is simply returned as is."
  
  (if (special-operator-p op)
      (cons op args)
      (cons op (expand-forms args env))))


(defmacro! defexpander (op (args &optional (env (gensym "ENV"))) &body body)
  "Defines a EXPAND-FN-ARGS method for the special operator OP. ARGS
   is a symbol which is bound to the form arguments and ENV (optional)
   is a symbol which is bound to the environment. The forms in BODY
   are evaluated in an implicit PROGN, of which the last form should
   return the expanded arguments."
  
  (multiple-value-bind (body decl doc)
      (parse-body body :documentation t)

    `(defmethod expand-fn-expr ((,g!op (eql ',op)) ,args (,env t))
       ,@(ensure-list doc) ,@decl

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
  "Expands all macros in the lexical function bodies and the body of
   the FLET form."
  
  (flet ((macroexpand-fn (fn)
	   (match-form (name . def) fn
	     `(,name ,@(expand-fn-def def env t)))))

    (match-form ((&rest fns) . body) args
      `(,(mapcar #'macroexpand-fn fns)
	 ,@(expand-forms body)))))

(defun expand-fn-def (def env &optional documentation)
  "Expands all macros in a function definition. DEF is a list where
   the first element is the function's lambda list and the remaining
   elements are the forms making up the function's body. If
   DOCUMENTATION is true the body may contain a documentation string."
  
  (match-form (lambda-list . body) def
    (let ((lambda-list (expand-lambda-list lambda-list env)))
      `(,lambda-list ,@(expand-body body env documentation)))))

(defun expand-lambda-list (list env &optional macro)
  "Expands all macros in an lambda list. If MACRO is true the lambda
   list is parsed as a macro lambda list otherwise it is parsed as an
   ordinary lambda list."
  
  (map-lambda-list
   (rcurry #'expand-lambda-list-arg env)
   list :destructure macro :env macro))

(defun expand-lambda-list-arg (type arg env)
  (multiple-value-match (values type arg)
    (((or 'required 'rest) (type symbol))
     arg)
       
    ((:optional
      (or (list var initform var-sp)
	  (list var initform)
	  (list var)
	  var))

     `(,var ,(macroexpand-all initform env) ,var-sp))

    ((:key
      (or (list arg initform var-sp)
	  (list arg initform)
	  (list arg)
	  arg))
     
     `(,arg ,(macroexpand-all initform env) ,var-sp))

    ((:aux
      (or (list var initform)
	  (list var)
	  var))
	
     `(var ,(macroexpand-all initform env)))

       ((_ _) arg)))

(defun expand-body (body env &optional documentation)
  "Expands all macros in each form in BODY. Declarations and a
   documentation string (if DOCUMENTATION is true) may appear as the
   first elements of the list BODY."
  
  (check-list body
    (multiple-value-bind (body decl doc)
	(parse-body body :documentation documentation)

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


;;; LET

(defexpander cl:let (args env)
  "Expands all macros in the init-forms of the bindings and in the
   body of the LET form."
  
  (labels ((expand-binding (binding)
	     (match binding
	       ((list var initform)
		(list var (macroexpand-all initform env)))

	       (_ binding))))
    
    (match-form ((&rest bindings) . body) args
      `(,(mapcar #'expand-binding bindings)
	 ,@(expand-body body env)))))

(defexpander cl:let* (args env)
  (expand-fn-args 'cl:let args env))

;;; LOAD-TIME-VALUE

(defexpander cl:load-time-value (args env)
  (match-form (form &optional read-only-p) args
    `(,(macroexpand-all form env) ,read-only-p)))


;;; LOCALLY

(defexpander cl:locally (args env)
  (expand-body args env))


;;; MACROLET

(defexpander cl:macrolet (args env)
  "Expands all macros in the lexical macro definitions and in the body
   of the MACROLET form within the environment ENV augmented with the
   macro definitions."
  
  (flet ((parse-macro (macro)
	   (match-form (name lambda-list . body) macro
	     (list name (parse-macro name lambda-list body env)))))
    (match-form ((&rest macros) . body) args
      (let ((env (augment-environment env :macro (mapcar #'parse-macro macros))))
	(expand-body body env)))))

(defexpander cl:symbol-macrolet (args env)
  "Expands all macros in the body of the SYMBOL-MACROLET form within
   the environment ENV augmented with the symbol macro definitions."
  
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

(defmethod expand-fn-expr ((op (eql 'cl:setq)) args env)
  "Expands all macros in the value forms. If any of the symbols name a
   symbol macro, in the environment ENV, the SETQ form is converted to
   a SETF form and expanded."

  (restart-case
      (let ((args (loop for (var form) on args by #'next-2
		     nconc (list var (macroexpand-all form env))
		     do
		       (when (nth-value 1 (macroexpand var env))
			 (setf op 'cl:setf)))))
	(if (eq op 'cl:setf)
	    (macroexpand-all (cons op args) env)
	    (cons op args)))
    (skip-walk () (cons op args))))


;;; TAGBODY

(defexpander cl:tagbody (args env)
  (check-list args
    (loop for form in args
       collect
	 (if (atom form)
	     form
	     (macroexpand-all form env)))))

(defexpander cl:go (args)
  args)


;;; THE

(defexpander cl:the (args env)
  (match-form (type form) args
    `(,type ,(macroexpand-all form env))))


;;; UNWIND-PROTECT

(defexpander cl:unwind-protect (args env)
  (expand-forms args env))
