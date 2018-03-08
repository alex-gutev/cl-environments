;;;; lambda.lisp
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

(in-package :cl-environments.walker)


(define-condition malformed-lambda-list ()
  ((list-position
    :reader list-position
    :initarg :position)))


(defun lambda-list-keyword-p (sym)
  "Checks wether SYM names a lambda list keyword, i.e. checks whether
   SYM is a member of LAMBDA-LIST-KEYWORDS."
  
  (member sym lambda-list-keywords))

(defun ignore-error (c)
  (declare (ignore c))
  (invoke-restart 'ignore-error))

;;; TODO: Fix issue with consume-arg consuming lambda-list keywords

(defun map-lambda-list (fn list &key ((:destructure destructurep)) ((:env envp)))
  (declare (special envp))

  (let* (new-list
	 (collector (make-simple-collector-to-place new-list)))
    (declare (special collector))

    (labels
	((map-collect (state thing)
	   (awhen (funcall fn state thing)
	     (funcall collector it)))

	 (collect (state &rest things)
	   (mapc (curry #'map-collect state) things))
	 
	 (guard (condition list)
	   (unless condition
	     (signal-error list)))

	 (signal-error (list)
	   (restart-case
		 (error 'malformed-lambda-list :position list)
	       (ignore-error ())))

	 (consume-arg (state list)
	   (match list
	     ((list* arg rest)
	      (collect state arg)
	      rest)
	     
	     (_
	      (signal-error list)
	      list))))

      (match-state list
	(whole
	 (and (cons '&whole rest) list)

	 (guard (and destructurep (eq from-state :start)) list)
	 (collect nil '&whole)
	 
	 (next (consume-arg 'whole rest) :from 'required))

	(optional
	 (and (cons '&optional rest) list)

	 (guard (member from-state '(:start required)) list)
	 (collect nil '&optional)

	 (next rest))

	(rest
	 (and (cons (and (or '&rest '&body) keyword) rest) list)

	 (guard (and (or (eq keyword '&rest) destructurep)
		     (member from-state '(:start required optional))) list)
	 (collect nil keyword)

	 (next (consume-arg 'rest rest)))

	(key
	 (and (cons '&key rest) list)

	 (guard (member from-state '(:start required optional rest)) list)
	 (collect nil '&key)

	 (next rest))

	(allow-other-keys
	 (and (cons '&allow-other-keys rest) list)

	 (guard (eq from-state 'key) list)
	 (collect nil '&allow-other-keys)

	 (next rest :from 'key :force t))

	(aux
	 (cons '&aux rest)

	 (collect nil '&aux)
	 (next rest))

	(environment
	 (and (cons '&environment rest) list)

	 (guard envp list)
	 (setf envp nil)

	 (collect nil '&environment)
	 (next (consume-arg 'environment rest) :from from-state :force t))

	;; Argument states

	(required
	 (guard (cons (guard var-list (listp var-list)) rest) destructurep)
	 :from (:start required)

	 (let (new-list)
	   (let ((collector (make-simple-collector-to-place new-list))
		 envp)
	     (declare (special collector envp))
	     (next var-list :from :start))
	   (funcall collector new-list))
	 
	 (next rest))

	(required
	 (cons var rest)
	 :from (:start required)

	 (collect 'required var)
	 (next rest))
	
	(arg
	 (cons var rest)
	 :from (optional key aux)

	 (collect from-state var)
	 (next rest :from from-state))

	(end-of-list nil)

	(dotted-list
	 (guard var (atom var))

	 (guard (member from-state '(required optional)) var)

	 (awhen (funcall fn '&rest var)
	   (nconc new-list it)))

	(else
	 rest
	 (signal-error rest)
	 (nconc new-list rest)))
      
      new-list)))

(defun walk-lambda-list (list env &key ((:destructure destructurep)) ((:env envp)))
  "Walks the lambda list LIST, augments the environment ENV with the
   bindings introduced by the lambda list, and encloses the initforms
   of the arguments in the environments augmented with the bindings
   introduced before that argument in the lambda list. Returns the new
   lambda list and the augmented environment. The keyword
   argument :DESTRUCTURE indicates whether the lambda list should be
   treated as a destructuring lambda list, :ENV whether &ENVIRONMENT
   parameters are accepted and :GENERIC whether the lambda list is a
   DEFMETHOD lambda list. This function performs a best effort, that
   is it does not check for all syntax errors and if a syntax error is
   found, it simply returns the rest of the lambda list letting,
   leaving the CL implementation to deal with the error."

  (let ((env (copy-environment env)))
    (flet ((walk-arg (type arg)
	     (multiple-value-bind (arg new-env)
		 (walk-lambda-list-arg type arg env)
	       (setf env new-env)
	       arg)))

      (handler-bind
	  ((malformed-lambda-list #'ignore-error))
	(map-lambda-list #'walk-arg list
			 :destructure destructurep
			 :env envp)))))

(defgeneric walk-lambda-list-arg (type arg env))

(defmethod walk-lambda-list-arg ((type (eql nil)) keyword env)
  (values keyword env))

(defmethod walk-lambda-list-arg ((type (eql 'optional)) arg env)
  (match arg
    ((or (list var initform var-sp)
	 (list var initform)
	 (list var)
	 var)

     (values
      `(,var ,(enclose-in-env env (list initform)) ,var-sp)
      (aprog1 (copy-environment env)
	(add-variable var it)
	(when var-sp (add-variable var-sp it)))))

    (_ (values arg env))))


(defmethod walk-lambda-list-arg ((type (eql 'key)) arg env)
  (match arg
    ((or (cons
	   (and (or (list keyword var)
		    var)
		arg)
	   (or
	    (list initform var-sp)
	    (list initform)
	    nil))
	  (and var arg))

     (values
      `(,arg ,(enclose-in-env env (list initform)) ,var-sp)
      (aprog1 (copy-environment env)
	(add-variable var it)
	(when var-sp (add-variable var-sp it)))))
    
    (_ (values arg env))))

(defmethod walk-lambda-list-arg ((type (eql 'aux)) arg env)
  (match arg
    ((or (list var initform) var)

     (values
      `(,var ,(enclose-in-env env (list initform)))
      (aprog1 (copy-environment env)
	(add-variable var it))))
    (_ (values arg env))))

(defmethod walk-lambda-list-arg ((type t) arg env)
  (add-variable arg env)
  (values arg env))
