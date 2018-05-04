# CL-ENVIRONMENTS - CLTL2 Environment Access Compatibility Layer

This library provides a compatibility layer for the Syntactic
Environment Access API, specified in [Common Lisp the Language
2](https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node102.html), which
can be used both on implementations which provide this API natively
and those which don't.

On implementations, which provide the CLTL2 environment access API,
this library is simply a wrapper which handles the peculiarities of
each implementation.

For implementations, which do not provide the API, the environment
information is extracted using a code-walker and can be accessed using
implementations of the environment API functions.

The following functions/macros are provided by this library on all
implementations:

* *VARIABLE-INFORMATION*
* *FUNCTION-INFORMATION*
* *DECLARATION-INFORMATION*
* *DEFINE-DECLARATION*

*AUGMENT-ENVIRONMENT* cannot be implemented on implementations which
do not provide the functionality natively, since it requires that the
macro-expansion functions be overridden to handle extended
environment objects, however functions cannot be reliably overridden
in Common Lisp due to inlining.


## Status

Tested on Clisp and CCL.
