;;;; flet.lisp
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

;;;; Test that environment information is extracted from FLET forms.

(defpackage :cl-environments.test.cltl2.flet-forms
  (:use :cl-environments-cl
	:cl-environments.test.cltl2
	:fiveam))

(in-package :cl-environments.test.cltl2.flet-forms)

(def-suite flet-forms
    :description "Test extraction of environment information from FLET forms"
    :in cltl2-test)

(in-suite flet-forms)

(defun global-fn (a b c)
  (/ (* a b) c))

(declaim (ftype (function (integer integer integer) number) global-fn))

(defmacro test-macro (form)
  form)

(test (function-types :compile-at :run-time)
  "Test extracting function information"

  (flet ((inc (a)
	   (1+ a))

	 (add (a b)
	   (+ a b)))

    (declare (ftype (function (integer) integer) inc)
	     (ftype (function (number number) number) add)
	     (ignorable #'inc #'add))

    (declare (dynamic-extent #'inc)
	     (inline add)
	     (notinline global-fn))

    (is (info=
	 ;; CMUCL ignores DYNAMIC-EXTENT here
	 ;; ECL ignores DYNAMIC-EXTENT entirely

	 #-(or cmucl ecl) '(:function t ((ftype . (function (integer) integer))
				(dynamic-extent . t)))

	 #+(or cmucl ecl) '(:function t ((ftype . (function (integer) integer))))

	 (info function inc)))

    (is (info=
	 '(:function t ((ftype . (function (number number) number))
			(inline . inline)))

	 (info function add)))

    (is (info=
	 ;; CCL sometimes doesn't store global declarations
	 #-ccl '(:function nil ((ftype . (function (integer integer integer) number))
				(inline . notinline)))

	 #+ccl '(:function nil ((inline . notinline)))

	 (info function global-fn)))

    (is (info=
	 '(:macro nil nil)
	 (info function test-macro)))

    (is (info=
	 '(:macro nil nil)
	 (info function cl:defun)))

    (is (info=
	 '(:special-form nil nil)
	 (info function cl:if)))

    (is (info=
	 '(nil nil nil)
	 (info function not-a-function)))))

(test (shadowing :compile-at :run-time)
  "Test lexical shadowing of functions"

  (flet ((f2 (a b)
	   (+ a b)))
    (declare (ftype (function (number number) number) f2)
	     (inline f2)
	     (ignorable #'f2))

    (flet ((f1 (x)
	     (declare (type integer x)
		      (ignore x))

	     (values
	      (info variable x)
	      (info function f2)
	      (info function global-fn)))

	   (f2 (a b)
	     (declare (ignore a b))

	     (values
	      (info variable a)
	      (info variable b)
	      (info function f1)))

	   (global-fn (x) x))

      (declare (notinline f2)
	       (ignore #'global-fn)
	       (ignorable #'f1 #'f2))

      (multiple-value-bind (info-x info-f2 info-global-fn)
	  (f1 1)

	(is-every info=
	  #-ecl ('(:lexical t ((type . integer) (ignore . t))) info-x)

	  ;; ECL does not recognize IGNORE declarations referring to
	  ;; function argument variables.

	  #+ecl ('(:lexical t ((type . integer))) info-x)

	  ('(:function t ((ftype . (function (number number) number))
      			  (inline . inline)))
	    info-f2)

	  #-ccl ('(:function nil ((ftype . (function (integer integer integer) number)))) info-global-fn)
	  #+ccl ('(:function nil nil) info-global-fn)))

      (multiple-value-bind (info-a info-b info-f1)
	  (f2 1 2)

	(is-every info=
	  ('(:lexical t ((ignore . t))) info-a)
	  ('(:lexical t ((ignore . t))) info-b)
	  ('(nil nil nil) info-f1)))

      (is (info=
	   '(:function t ((inline . notinline)))
      	   (info function f2)))

      (is (info=
	   #-(or sbcl ccl cmucl ecl) '(:function t ((ignore . t)))
	   #+(or sbcl ccl cmucl ecl) '(:function t nil)

	   (info function global-fn))))))
