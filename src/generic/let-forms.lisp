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

;;;; Code-walkers for: LET, LET*, FLET, LABELS, MACROLET and
;;;; SYMBOL-MACROLET.

;;; LET forms

(defwalker cl:let (args env)
  "Walks LET binding forms, augments the environment ENV with the
   bindings introduced, adds the declaration information to the
   bindings and encloses the body of the LET form in the augmented
   environment."

  (match-form ((&rest bindings) . body) args
    (let* ((old-env (get-environment env))
	   (new-env (copy-environment old-env env)))
      (cons (walk-let-bindings bindings new-env env)
	    (walk-body body new-env)))))

(defun walk-let-bindings (bindings env lex-env)
  "Walks the bindings of a LET form. Adds the variable bindings to the
   environment ENV and encloses the init-forms of the bindings (if any)
   in the code walking macro. Returns the new bindings list."
  
  (flet ((enclose-binding (binding)
	   (match binding
	     ((list var initform)
	      `(,var ,(walk-form initform lex-env)))
	     (_ binding))))

    (loop
       for binding in bindings
       collect (enclose-binding binding)
       do
	 (add-variable (ensure-car binding) env))))

(defun walk-body (body ext-env &optional documentation)
  "Walks the body of forms which create a local environment, such as
   LET/FLET/LOCALLY. Adds the declaration information to the bindings
   in the environment EXT-ENV, and encloses the body in the augmented
   environment. If DOCUMENTATION is true the body may contain a
   documentation string preceding or following the declarations."

  (check-list body
    (multiple-value-bind (forms decl docstring)
	(parse-body body :documentation documentation)
      (walk-declarations decl ext-env)
      `(,@(ensure-list docstring)
	  ,@decl
	  ,(enclose-in-env
	    ext-env
	    forms)))))


;;; LET* forms

(defwalker cl:let* (args env)
  "Walks LET* binding forms: encloses each init-form in an environment
   containing all variables in the preceding bindings. Encloses the
   body in an environment containing all the variable bindings
   introduced by the LET*."

  (match-form ((&rest bindings) . body) args
    (let* ((old-env (get-environment env))
	   (body-env (create-let*-env bindings old-env env))
	   (body (walk-body body body-env))
	   (bindings (walk-let*-bindings bindings old-env body-env)))

      (cons bindings body))))

(defun create-let*-env (bindings env lex-env)
  "Creates the lexical environment for the body of a LET* form, by
   adding the BINDINGS to a copy of the environment ENV. Returns the
   new environment."

  (let ((env (copy-environment env lex-env)))
    (dolist (binding bindings env)
      (add-variable (ensure-car binding) env))))

(defun walk-let*-bindings (bindings env body-env)
  "Walks the bindings of a LET*. ENV is the environment containing the
   LET* form and BODY-ENV is the environment in which the body of the
   LET* form is enclosed, i.e. the environment containing all the
   bindings introduced by the LET*. Each init-form is enclosed in an
   environment which contains all the previous bindings copied from
   BODY-ENV."
  
  (flet ((enclose-binding (binding env)
	   (match binding
	     ((list var initform)
	      `(,var ,(enclose-in-env env (list initform))))
	     (_ binding))))

    (loop
       for binding in bindings
       collect (enclose-binding binding env)
       do
	 (setf env (copy-environment env))
	 (let ((var (ensure-car binding)))
	   (setf (variable-binding var env) ; Copy binding in BODY-ENV to ENV
		 (variable-binding var body-env))))))


;;; Lexical functions

(defwalker cl:flet (args env)
  "Walks FLET forms. The functions introduced by the FLET are added to
   a copy of the environment ENV, in which the body, of the FLET form,
   is enclosed. The body of each function is enclosed in an
   environment containing the variables in the function's lambda list,
   however it does not contain the functions themselves."
  
  (let* ((env (enclose-environment (get-environment env) env))
	 (new-env (copy-environment env)))

    (match-form ((&rest fns) . body) args
      (cons (mapcar (rcurry #'walk-local-fn env new-env) fns)
	    (walk-body body new-env)))))

(defwalker cl:labels (args env)
  "Walks LABELS forms. The functions introduced by the LABELS are
   added to a copy of the environment ENV, in which the body, of the
   LABELS form, is enclosed. The body of each function is enclosed in
   an environment containing: all the functions, introduced by the
   LABELS, and the variables in the function's lambda list. All
   declarations, bound to the functions introduced by the LABELS, are
   added to the environments of the function bodies."
  
  (let* ((env (copy-environment (get-environment env) env))
	 (body-env (copy-environment env)))
    
    (labels
	((make-fn-env (fns)
	   (dolist (fn fns env)
	     (match-form (name . _) fn
	       (setf (function-binding name env) ; Copy binding from BODY-ENV to ENV
		     (function-binding name body-env)))))
	 
	 (walk-fns (fns)
	   (loop
	      for fn in fns
	      with env = (make-fn-env fns)
	      collect
		(match-form (name . def) fn
		    (cons name (walk-fn-def def env))))))

      ;; Add all function-bindings to BODY-ENV
      (match-form ((&rest fns) . body) args
	(loop
	   for fn in fns
	   do
	     (match-form (name . _) fn
	       (add-function name body-env)))
	
	(let ((body (walk-body body body-env)))
	  (cons (walk-fns fns)
		body))))))


(defun walk-local-fn (def fn-env new-env)
  "Walks a lexical function, defined using FLET or LABELS. Adds the
   function to the environment NEW-ENV and encloses the body of the
   function in a copy of the environment FN-ENV containing the
   variables introduced by the function's lambda list."

  (match-form (name . def) def
    (add-function name new-env)
    `(,name ,@(walk-fn-def def fn-env t))))

(defun walk-fn-def (def env &optional documentation)
  "Walks a function definition, DEF is a list where the first element
   is the function's lambda-list and the rest of the elements are the
   forms making up the function's body. The variables introduced by
   the lambda-list are added to a copy of the environment ENV, the
   body is enclosed in this environment. The new lambda-list and body
   are returned. This function can be used both for lexical function
   definitions and for global function definitions."
  
  (match-form (lambda-list . body) def
    (multiple-value-bind (lambda-list env)
	(walk-lambda-list lambda-list env)
      (cons lambda-list (walk-body body env documentation)))))


;;; Lexical Macros

(defwalker cl:macrolet (args env)
  "Walks MACROLET forms. Each macro is added to a copy of the
   environment ENV, in which the body of the MACROLET form is
   enclosed. The body of each macro is enclosed in an environment
   containing the variables introduced by the macro's lambda-list, but
   does not contain the macro itself."

  (match-form ((&rest macros) . body) args
    
    (let* ((env (enclose-environment (get-environment env) env))
	   (new-env (copy-environment env)))
      
      (cons (mapcar (rcurry #'walk-local-macro env new-env) macros)
	    (walk-body body new-env)))))

(defun walk-local-macro (def mac-env new-env)
  "Walks a lexical macro, defined using MACROLET. Adds the macro to
   the environment NEW-ENV and encloses the body of the macro in a
   copy of the environment MAC-ENV containing the variables introduced
   by the macro's lambda list."

  (match-form (name . def) def
    (add-function name new-env :binding-type :macro)
    (cons name (walk-macro-def def mac-env))))

(defun walk-macro-def (def env)
  "Walks a macro definition, DEF is a list where the first element is
   the macro's lambda-list and the rest of the elements are the forms
   making up the body of the macro. The variables introduced by the
   lambda-list are added to a copy of the environment ENV with the
   body enclosed in this environment. The new lambda-list and body is
   returned. This function can be used both for lexical macro
   definitions and for global macro definitions."

  (match-form (lambda-list . body) def
    (multiple-value-bind (lambda-list env)
	(walk-lambda-list lambda-list env :destructure t :env t)
      
      (cons lambda-list (walk-body body env t)))))


;;; Lexical Symbol Macros

(defwalker cl:symbol-macrolet (args env)
  "Walks SYMBOL-MACROLET forms. Each symbol macro is added to a copy
   of the environment ENV, and the body is enclosed in this
   environment."

  (match-form ((&rest macros) . body) args
    (if (not (eq (caar macros) *env-sym*))
	(let ((env (copy-environment (get-environment env) env)))
	  (mapc (compose (rcurry #'add-symbol-macro env) #'first) macros)
	  (cons macros (walk-body body env)))
	args)))
