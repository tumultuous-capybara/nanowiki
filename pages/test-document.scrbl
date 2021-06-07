#lang nanowiki

@title{Test Document}

@(define (empty-a . forms)
  `(a ([href "#"]) ,@forms))

@(define (backtick-code . forms)
  `(span ([style "font-family: monospace;font-weight: 700;font-size: .875em;"])
    "`" ,@forms "`"))

Aw yeah, paragraph time.

If the property value is a procedure of one argument, then the procedure serves as a syntax transformer and for set! transformations. If the property value is a procedure of two arguments, then the first argument is the structure whose type has @empty-a{prop:set!-transformer} property, and the second argument is a syntax object as for a @backtick-code{@empty-a{syntax transformer}} and for @backtick-code{set! transformations}; set!-transformer-procedure applied to the structure produces a new function that accepts just the syntax object and calls the procedure associated through the property. Finally, if the property value is an integer, the target identifier is extracted from the structure instance; if the field value is not a procedure of one argument, then a procedure that always calls @empty-a{raise-syntax-error} is used, instead.

@section{Test Section}

Raw test @italic{paragraph}.
It doesn't actually matter how many newlines there are, because the program still ignores them for now.

@(page/add-raw-connection "Connection Test 1" "foo bar baz")
@(page/add-raw-connection "Connection Test 1" "bar baz qux")
