;;;; define-declaration
;;;;
;;;; Copyright 2021 Alexander Gutev
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

;;;; Test defining custom declarations

(defpackage :cl-environments.test.cltl2.define-declaration
  (:use :cl-environments-cl
	:cl-environments.test.cltl2
	:alexandria
	:fiveam))

(in-package :cl-environments.test.cltl2.define-declaration)

(def-suite define-declaration
    :description "Test defining custom declarations using DEFINE-DECLARATION"
    :in cltl2-test)

(in-suite define-declaration)


;;; Variable Declarations

(define-declaration foo-type (args)
  (destructuring-bind (foo-type &rest vars) args
    (values
     :variable
     (mapcar
      (lambda (var)
	`(,var foo-type ,foo-type))
      vars))))


;;; Function Declarations

(define-declaration bar-type (args)
  (destructuring-bind (bar-type &rest fns) args
    (values
     :function
     (mapcar
      (lambda (fn)
	`(,fn bar-type ,bar-type))
      fns))))


;;; Other Declarations

(define-declaration foobar-types (args)
  (destructuring-bind (&rest types) args
    (values
     :declare
     (cons 'foobar-types types))))


;;; Custom declarations in global functions

(defun test-function (a b)
  (declare (foo-type foo1 a b))

  (+ a b)
  (values
   (info variable a)
   (info variable b)))

(defmacro test-macro (form)
  (declare (foo-type macfoo form))

  (list form)
  `',(info variable form))

(defgeneric test-generic (a)
  (:method ((a number))
    (declare (foo-type genericfoo a))
    (1+ a)
    (info variable a)))

(defmethod test-generic ((str string))
  (declare (foo-type stringfoo str))
  (string-capitalize str)
  (info variable str))


;;; Tests

(test variable-declarations
  :description "Custom declarations applying to variables"

  (is
   (info=
    (let ((x 1))
      (declare (foo-type my-foo x))
      (1+ x)
      (info variable x))

    '(:lexical t ((foo-type . my-foo))))))

(test function-declarations
  :description "Custom declarations applying to functions"

  (is
   (info=
    (flet ((f (x) (1+ x)))
      (declare (bar-type my-bar f))
      (f 1)
      (info function f))

    '(:function t ((bar-type . my-bar))))))

(test other-declarations
  :description "Custom declarations neither applying to variables nor functions"

  (is
   (equal
    (locally (declare (foobar-types foo1 foo2 bar3))
      (first (info declaration foobar-types)))

    '(foo1 foo2 bar3))))

(test in-global-definitions
  :descript "Test usage of custom declarations in global definitions"

  (multiple-value-bind (info1 info2)
      (test-function 1 2)

    (is (info= info1 '(:lexical t ((foo-type . foo1)))))
    (is (info= info2 '(:lexical t ((foo-type . foo1))))))

  (is
   (info= (test-macro x) '(:lexical t ((foo-type . macfoo)))))

  (is
   (info= (test-generic 1)
	  '(:lexical t ((foo-type . genericfoo)))))

  (is
   (info= (test-generic "hello")
	  '(:lexical t ((foo-type . stringfoo))))))
