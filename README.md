# trivial-form-type

This library is another attempt at [compiler-macro:form-type](https://github.com/Bike/compiler-macro), due to reasons of personal taste. It does use [introspect-environment](https://github.com/Bike/introspect-environment) by the same author though.

## Limitations of Common Lisp - and even CLTL2 - for purposes of this library

| Functionality        | ANSI 1994 Spec | CLTL2 |
|----------------------|----------------|-------|
| constantp            | Yes            | Yes   |
| variable-information | No             | Yes   |
| function-information | No             | Yes   |
| constant-form-value  | No             | No    |

## Difference with respect to compiler-macro:form-type

- Returns two values: the second value indicates whether the form-type was obtained *using* the declarations in the environment, or due to the *absence* of declarations in the environment.
- Avoids an additional (albeit minimal) layer of complexity by using `defmethod` instead of `define-inferrer`

## Dependencies outside quicklisp

- [trivial-types:function-name](https://github.com/digikar99/trivial-types)

## Test

```lisp
(asdf:test-system "trivial-form-type")
```
