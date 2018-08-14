# CL-ENVIRONMENTS - CLTL2 Environment Access Compatibility Layer

This library provides a compatibility layer for the Syntactic
Environment Access API, specified in [Common Lisp the Language
2](https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node102.html), which
can be used both on implementations which provide this API natively
and those which don't.

On implementations, which provide the CLTL2 environment access API,
this library is simply a wrapper which handles the peculiarities of
each implementation.

ON implementations, which do not provide the API, the environment
information is extracted using a code-walker and can be accessed using
implementations of the environment API functions.

The following functions/macros are provided by this library on all
implementations:

* `VARIABLE-INFORMATION`
* `FUNCTION-INFORMATION`
* `DECLARATION-INFORMATION`
* `DEFINE-DECLARATION`

`AUGMENT-ENVIRONMENT` is not provided as it cannot be implemented on
implementations which do not provide the functionality natively, since
it requires that all functions, which take an environment parameter,
be overridden to handle extended environment objects, however
functions cannot be reliably overridden in Common Lisp due to
inlining.

## Documentation

To be able to use the environment access functions, **`ENABLE-HOOK`**
has to be called to ensure that the code walker is set as the
macroexpansion hook on implementations where the code-walker is
required. On implementations where the code-walker is not required
`ENABLE-HOOK` and `DISABLE-HOOK` do nothing.

#### ENABLE-HOOK

Function: `ENABLE-HOOK &OPTIONAL PREVIOUS-HOOK`

Sets the code-walker as the `*MACROEXPAND-HOOK*` if
necessary. `PREVIOUS-HOOK`, if provided, is the function to restore
`*MACROEXPAND-HOOK*` to when calling `DISABLE-HOOK`. If not provided
defaults to the current value of `*MACROEXPAND-HOOK*`.


#### DISABLE-HOOK

Function: `DISABLE-HOOK &OPTIONAL PREVIOUS-HOOK`

Restores `*MACROEXPAND-HOOK*` to its previous value prior to calling
`ENABLE-HOOK`.

If `PREVIOUS-HOOK` is provided restores `*MACROEXPAND-HOOK*` to that
value instead.


### Environment Access Functions

See [Common Lisp the Language
2](https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node102.html) for
the Environments API specification.


#### VARIABLE-INFORMATION

Function: `VARIABLE-INFORMATION SYMBOL &OPTIONAL ENV`

Returns information about the variable binding for symbol `SYMBOL`, in
the lexical environment `ENV`. `ENV` defaults to the `NIL` global
environment if it is not provided.

Returns three values.

The first value is one of the following identifying the type of
binding:

* `NIL`
    No apparent variable binding for `SYMBOL` in `ENV`
* `:LEXICAL`
    SYMBOL refers to a lexical variable.
* `:SPECIAL`
    SYMBOL refers to a special variable.
* `:SYMBOL-MACRO`
    SYMBOL refers to a symbol macro.
* `:CONSTANT`
    SYMBOL refers to a constant, defined by `DEFCONSTANT`, or is a keyword.

The second return value is `T` if there is a local variable binding for
`SYMBOL`, `NIL` otherwise.

The third return value is an alist containing the declaration
information applying to the variable `SYMBOL`.


#### FUNCTION-INFORMATION

Function: `FUNCTION-INFORMATION SYMBOL &OPTIONAL ENV`

Returns information about the function binding for `SYMBOL` in the
environment `ENV`. `ENV` defaults to the global `NIL` environment if it is
not provided.

Returns three values.

The first value is one of the following identifying the type of
binding:

* `NIL`
    No apparent function binding for SYMBOL in ENV.
* `:FUNCTION`
    SYMBOL refers to a function.
* `:MACRO`
    SYMBOL refers to a macro.
* `:SPECIAL-FORM`
    SYMBOL refers to a special operator, which does not have an associated macro function.

The second return value is `T` if there is a local fucntion binding for
`SYMBOL`, `NIL` otherwise.

The third return value is an alist containing the declaration
information applying to the function SYMBOL.


#### DECLARATION-INFORMATION

Function: `DECLARATION-INFORMATION NAME &OPTIONAL ENV`

Returns information about the declaration named `NAME` in the
environment `ENV`, which neither applies to variables nor
functions. `ENV` defaults to the global `NIL` environment if it is not
provided.


#### DEFINE-DECLARATION

Macro: `DEFINE-DECLARATION NAME (ARG-VAR &OPTIONAL ENV-VAR) &BODY BODY`

Defines a handler for the declaration named `NAME`. `NAME` must not name a
standard CL declaration, nor an implementation-specific declaration.

`ARG-VAR` is the name of the variable which will be bound to the
argument list of the declaration (the `CDR` of the declaration where the
declaration is of the form `(NAME . ARGS)`).

If provided `ENV-VAR` is the name of the variable which will be bound to
the lexical environment in which the declaration occurs.

`BODY` is the body of the declaration handler which should return two
values. The first is a keyword which indicates what the declaration
applies to:

* `:VARIABLE` - The declaration applies to variable bindings.
* `:FUNCTION` - The declaration applies to function bindings.
* `:DECLARE` - The declaration neither applies to variable nor function bindings.

If the first value is either `:VARIABLE` or `:FUNCTION` the second
value should be a list where each element is of the form `(SYMBOL KEY
VALUE)` where `SYMBOL` is the `SYMBOL` naming the binding to which the
declaration applies. The `CONS` `(KEY . VALUE)` will be included in the
alist returned by `VARIABLE-INFORMATION`/`FUNCTION-INFORMATION` for the
symbol `SYMBOL`.

If the first value is `:DECLARE` the second value should be a `CONS` of
the form `(KEY . VALUE)`. `VALUE` will be returned by
`DECLARATION-INFORMATION` for the declaration named `KEY`.


## Status

Supports: Clisp, CCL, ECL, ABCL, CMUCL, SBCL, Allegro CL and LispWorks.

Defaults to the full code-walker implementation on other CL
implementations. This will work if the implementation does not expand
the standard CL macros to implementation-specific special forms.


Tested on: Clisp, CCL, ECL, ABCL, CMUCL and SBCL.


## Issues

### ABCL

* Some individual forms (such as `DEFUN`) cannot be compiled using C-c
  C-c in slime while `*MACROEXPAND-HOOK*` is set to the code walker,
  the entire file can still be compiled using C-c C-k.

* ABCL passes `NIL` as the environment parameter to compiler macro
  functions thus there is no way to obtain any information about the
  lexical environment in which the form appears. The environment
  information functions: `VARIABLE-INFORMATION`, `FUNCTION-INFORMATION`
  and `DECLARATION-INFORMATION` can only return information about global
  bindings/declarations when called from inside a compiler macro.