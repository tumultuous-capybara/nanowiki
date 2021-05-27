#lang racket

(require "page.rkt" (for-syntax syntax/stx syntax/kerncase syntax/parse))

(define-syntax (markup-module-begin stx)
  (syntax-parse stx
    ((_ req forms ...) #'(#%module-begin req (filter-toplevel forms) ...))))

(define-syntax (filter-toplevel expr)
  (define (stx-second stx)
    (car (stx-cdr stx)))
  (define (is-top-level? x)
    (and (stx-list? x)
         (member (syntax->datum (stx-car x))
              '(require provide define-values define-syntaxes begin-for-syntax
                 module module* #%require #%provide #%declare))))
  (define (handle-form x s)
    (if (is-top-level? x)
      (stx-second s)
      #`(append-to-current-contents #,(stx-second s))))
  (handle-form (expand (stx-second expr)) expr))

(define (append-to-current-contents v)
  (set-page-contents! current-page (append (page-contents current-page) (list v))))

(provide (rename-out [markup-module-begin #%module-begin]))
(provide #%top #%app #%datum #%top-interaction require append-to-current-contents)
