;;;; package.lisp
;;;;
;;;; Copyright 2017-2021 Alexander Gutev
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

(defpackage :cl-environments.cltl2
  (:use :common-lisp
	:alexandria
	:anaphora
	:iterate
	:optima

	:cl-environments.util)

  (:import-from :collectors
		:make-simple-collector
		:make-simple-collector-to-place)

  (:export :variable-information
	   :function-information
	   :declaration-information
	   :define-declaration

	   :augment-environment
	   :augmented-macroexpand-1
	   :augmented-macroexpand
	   :augmented-macro-function
	   :augmented-get-setf-expansion

	   :enclose
	   :enclose-macro

	   :enable-hook
	   :disable-hook

	   :walk-environment)

  (:documentation
   "Package exporting the base CLTL2 environments API."))
