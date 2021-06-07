#lang nanowiki

@(define (subtitle . name)
  `(p ([style "font-size: 1.5em;max-width: 500px;font-style: italic;margin-bottom: 1.6em;line-height: 1.4;"])
    ,@name))

@(define (link href name)
  `(p () (a ([href ,href]) ,name)))

@title{The Personal Wiki of a Capybaba}

@subtitle{This is my bag of tricks — patterns, rules-of-thumb, tools, cheatsheets, gimmicks, leverage points, questions, risks, and unknowns.}

Typeclass metaprogramming is a powerful technique available to Haskell programmers to automatically generate term-level code from static type information. It has been used to great effect in several popular Haskell libraries (such as the servant ecosystem), and it is the core mechanism used to implement generic programming via GHC generics. Despite this, remarkably little material exists that explains the technique, relegating it to folk knowledge known only to advanced Haskell programmers.

This blog post attempts to remedy that by providing an overview of the foundational concepts behind typeclass metaprogramming. It does not attempt to be a complete guide to type-level programming in Haskell—such a task could easily fill a book—but it does provide explanations and illustrations of the most essential components. This is also not a blog post for Haskell beginners—familiarity with the essentials of the Haskell type system and several common GHC extensions is assumed—but it does not assume any prior knowledge of type-level programming.

@(link "./nanowiki-architecture-decisions.html" "Architecture")
