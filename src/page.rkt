#lang racket

; a very light connection system for pages, the bulk of the abstraction will fall on the document pages

(require racket/hash xml json "config.rkt")

(provide load-page current-page write-all-pages page-index set-page-contents! page set-page-title! page-contents build-json-index page-options get-page get-connection page-connections set-page-connections!)

(struct page
  (path [title #:mutable] [connections #:mutable] [contents #:mutable] [options #:mutable]))

(struct connection
  ([target] [static-data] [dynamic-data #:mutable] [title #:mutable] [is-hidden #:mutable] [link-fragment #:mutable]))

(define current-page #f)
(define page-index '())

(define (default-options)
  (make-hash
    `((index? . #true))))

(define (load-page path)
  (let ([p (page path #f '() '() (default-options))])
    (set! current-page p)
    (set! page-index (append page-index (list p)))
    (dynamic-require path #f)
    (process-contents p)))

; --- processes the document contents into a more usable form
;   - remove #<void> results from expression evaluation
;   - collect strings and xexpr results into paragraphs, wrapping multiple elements with <p> tags,
;     code has to explicitly return a div to have that alone in the document root
;   - remove newlines, they don't do anything in the html output anyway
;   - exposes methods to allow functions to quickly parse and manage document inputs
(define (process-contents page)
  (let ([src (page-contents page)] [acc '()] [new '()] [last-newline #f])
    (define (handle-acc)
      (if (and (equal? (length acc) 1) (not (string? (car acc))))
          (car acc)
         `(p () ,@acc)))
    (define (shift-paragraph)
      (let ([v (handle-acc)])
        (set! last-newline #f)
        (set! new (append new (list v)))
        (set! acc (list))))
    (define (collect-form f)
      (set! acc (append acc (list f))))
    (define (acc-empty?) (equal? acc '()))
    (for ([form (filter-void src)])
      (if (is-newline-str? form)
          (unless (acc-empty?)
            (if last-newline
              (shift-paragraph)
              (set! last-newline #t)))
          (begin
            (when last-newline
              (set! last-newline #f)
              (collect-form " "))
            (collect-form form))))
    (when (not (acc-empty?))
      (shift-paragraph))
    (set-page-contents! page new)))

(define (filter-void lst)
  (filter (lambda (x) (not (void? x))) lst))

(define (is-newline-str? s)
  (and (string? s) (equal? (string-join (string-split s #px"[ \t]+")) "\n")))

(define (get-page s)
  (or (findf (lambda (p) (equal? (page-title p) s)) page-index)
      (error "page not found")))

(define (get-connection p key)
  (let ((v (findf (lambda (c) (equal? (first c) key)) (page-connections p))))
    (and v (second v))))

(define (build-json-index)
  (define (build-index-entry p)
    (list (page-title p) (strip-path (page-path p))))
  (define (handle-page p)
    (if (and (hash-ref (page-options p) 'index?) (page-title p))
        (build-index-entry p)
        (void)))
  (filter-void (map handle-page page-index)))

(define (write-page p)
  (define (call-template path forms title)
    ((dynamic-require path 'build-page) forms title))
  (let ([x (call-template default-template (page-contents p) (or (page-title p) "$:/fragment"))])
    (display-to-file (xexpr->string x)
      (format "./public/~a.html" (strip-path (page-path p)))
      #:exists 'replace)))

(define (write-all-pages)
  (for ([p page-index]) (write-page p)))

(define strip-path
  (compose
    (lambda (s) (string-replace s "/" "-"))
    (lambda (s) (substring s 8 (- (string-length s) 6)))
    path->string))
