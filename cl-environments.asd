;;;; cl-environments.asd
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

(asdf:defsystem #:cl-environments
  :description "Implements the CLTL2 environment access functionality
                for implementations which do not provide the
                functionality to the programmer."
  
  :author "Alexander Gutev"
  :license "MIT"
  :serial t
  :components ((:module "src"
	        :components

		((:module "common"
		  :components
		  ((:file "package")
		   (:file "util")
		   (:file "macro-util")))

		 #+ccl
		 (:module "ccl"
		  :serial t
		  :components
		  ((:file "../generic/package")
		   (:file "../generic/cl-overrides")
		   (:file "../generic/util")
		   (:file "../generic/walker")
		   (:file "environment")
		   (:file "declarations")
		   (:file "lambda")
		   (:file "let-forms")
		   (:file "special-forms")
		   (:file "cltl2-interface")
		   (:file "hook")))
		 
		 #-(or ccl sbcl ecl abcl cmucl allegro lispworks)
		 (:module "generic"
		  :serial t
		  :components
		  ((:file "package")
		   (:file "cl-overrides")
		   (:file "util")
		   (:file "walker")
		   (:file "environment")
		   (:file "declarations")
		   (:file "lambda")
		   (:file "let-forms")
		   (:file "def-forms")
		   (:file "special-forms")
		   (:file "cltl2-interface")
		   (:file "hook"))))))
  
  :depends-on (:alexandria
	       :anaphora
	       :iterate
	       :optima
	       :collectors
	       :let-over-lambda
	       :named-readtables)
  
  :in-order-to ((asdf:test-op (asdf:test-op :cl-environments-test))))

(asdf:defsystem #:cl-environments-test
  :description "Tests for cl-environments."
  :author "Alexander Gutev"
  :license "MIT"
  :serial t
  :depends-on (:cl-environments :prove)
  :defsystem-depends-on (:prove-asdf)
  :components ((:module "test"
		:components
		((:module "generic"
		  :components
		  ((:file "package")
		   (:test-file "declarations"))))))
  :perform (asdf:test-op :after (op c)
			 (funcall (intern #.(string :run) :prove) c)))
