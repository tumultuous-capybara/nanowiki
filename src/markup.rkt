#lang racket

(require scribble/reader "page.rkt")
(provide read-syntax)

(define (read-syntax path port)
  (define src-forms (read-inside port))
  (datum->syntax #f
    `(module markup-document nanowiki/src/markup-lang
        (require racket nanowiki/src/page nanowiki/src/page-utilities)
        ,@src-forms)))
