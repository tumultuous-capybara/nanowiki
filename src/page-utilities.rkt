#lang racket

(require racket/hash "page.rkt")

(provide collapse index-page? title page/add-raw-connection bold italic section reprovide)

(define (title . forms)
  (set-page-title! current-page (collapse forms)))

(define (index-page? v)
  (let ((o (page-options current-page)))
    (hash-set! o 'index? v)))

;; TODO -- this needs to walk a xexpr tree and pull out text contents
(define (collapse forms) (string-join forms))

(define (page/add-raw-connection name value)
  (let ((target-page (get-page name))
        (connections (page-connections current-page)))
    (set-page-connections! current-page (append connections (list name value)))))

(define (section . forms)
  (let ((text (collapse forms)))
    `(h2 ([id ,text]) (a ([href ,(format "#~a" text)]) ,text))))

(define (bold . forms)
  `(strong () ,@forms))

(define (italic . forms)
  `(em () ,@forms))

;; -- reprovide

(require syntax/parse/define
         (for-syntax racket/base
                     racket/require-transform))

(begin-for-syntax
  (define-syntax-class reprov-spec
    [pattern require-spec:expr
      #:with [[local-id phase] ...]
      (let-values ([[imports import-sources] (expand-import #'require-spec)])
        (for/list ([import (in-list imports)])
          (list (import-local-id import)
                (import-mode import))))]))

(define-simple-macro (reprovide spec:reprov-spec ...)
  (begin
    (require spec.require-spec ...)
    (provide (for-meta spec.phase spec.local-id) ... ...)))
