;;;; cl-environments.asd
;;;;
;;;; Copyright 2017-2019 Alexander Gutev
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

(asdf:defsystem #:cl-environments
  :description "Implements the CLTL2 environment access functionality
                for implementations which do not provide the
                functionality to the programmer."

  :author "Alexander Gutev"
  :license "MIT"
  :version "0.4"
  :serial t
  :components ((:module "src"
		:serial t
	        :components

		((:module "common"
		  :serial t
		  :components
		  ((:file "package")
		   (:file "let-over-lambda")
		   (:file "util")
		   (:file "macro-util")))

		 (:file "other/sbcl"
			:if-feature :sbcl)

		 (:file "other/allegro"
			:if-feature :allegro)

		 (:file "other/lispworks"
			:if-feature :lispworks)

		 (:module
		  "partial"
		  :serial t
		  :if-feature (:or :ccl :cmucl)
		  :components
		  ((:file "../full/package")
		   (:file "../full/util")
		   (:file "../full/walker")
		   (:file "cl-environments")
		   (:file "environment")
		   (:file "declarations")
		   (:file "lambda")
		   (:file "let-forms")
		   (:file "def-forms")
		   (:file "../full/special-forms")
		   (:file "special-forms")
		   (:file "cltl2-interface")
		   (:file "../full/hook")
		   (:file "../full/cl-overrides")))

		 (:module
		  "full"
		  :serial t
		  :if-feature (:not (:or :ccl :sbcl :cmucl :allegro :lispworks))
		  :components
		  ((:file "package")
		   (:file "util")
		   (:file "walker")
		   (:file "cl-environments")
		   (:file "environment")
		   (:file "declarations")
		   (:file "lambda")
		   (:file "let-forms")
		   (:file "def-forms")
		   (:file "special-forms")
		   (:file "cltl2-interface")
		   (:file "augment-environment")
		   (:file "hook")
		   (:file "cl-overrides")))

		 (:module
		  "tools"
		  :serial t
		  :components
		  ((:file "package")
		   (:file "types"))))))

  :depends-on (:alexandria
	       :anaphora
	       :optima
	       :collectors)

  :in-order-to ((asdf:test-op (asdf:test-op :cl-environments/test))))

(asdf:defsystem #:cl-environments/test
  :description "Tests for cl-environments."
  :author "Alexander Gutev"
  :license "MIT"
  :serial t
  :depends-on (:cl-environments :fiveam)
  :components ((:module "test"
		:components
		((:module
		  "cltl2"
		  :components
		  ((:file "test")
		   (:file "let")
		   (:file "letstar")
		   (:file "flet")
		   (:file "labels")
		   (:file "macrolet")
		   (:file "lambda")
		   (:file "special-forms")
		   (:file "def-forms")
		   (:file "define-declaration")
		   (:file "augment-environment"))))))

  :perform (test-op (o s)
		    (uiop:symbol-call :cl-environments.test.cltl2 :test-cl-environments)))
