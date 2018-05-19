;;;; declarations.lisp
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


;;; DECLAIM code-walker

(defwalker cl:declaim (args)
  "Walks DECLAIM forms. Walks the declarations, as global
   declarations, and adds the declaration information to the global
   environment."
  
  (check-list args
    (dolist (arg args args)
      (match-form (decl &rest args) arg
	(let ((*env* nil))
	  (walk-declaration decl args *global-environment*))))))


;;; Walking Declarations

(defun walk-declarations (decl ext-env)
  "Walks the declare expressions in DECL and adds the information to
   EXT-ENV."
  
  (labels ((walk-decl (decl)
	     (match-form (decl . args) decl
	       (walk-declaration decl args ext-env)))
	   
	   (walk-declare (decl)
	     (match-form ('cl:declare &rest decl) decl
	       (mapc #'walk-decl decl))))

    (check-list decl
      (mapc #'walk-declare decl))))



(defun walk-declaration (decl args ext-env)
  "If there is a declaration function, for DECL, (defined using
   DEFINE-DECLARATION) in EXT-ENV, it is called and the information
   returned by the function is added to the environment."

  (awhen (declaration-function decl ext-env)
    (multiple-value-call #'add-decl-info (funcall it args *env*) ext-env)))

(defun add-decl-info (type info ext-env)
  "Adds the information returned by a declaration function to the
   environment. TYPE is the first return
   value (either :VARIABLE :FUNCTION or :DECLARE), INFO is the second
   return value."
  
  (flet ((add-binding-info (add-info)
	   (loop
	      for (sym key value) in info
	      do (funcall add-info sym key value ext-env))))
    (case type
      (:variable
       (add-binding-info #'add-variable-info))
      (:function
       (add-binding-info #'add-function-info))
      (:declare
       (setf (declaration-info (car info) ext-env) (cdr info))))))
     
