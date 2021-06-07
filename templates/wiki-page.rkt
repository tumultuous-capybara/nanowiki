#lang racket

;; simple page template with a colorful header and room for nav links

(require nanowiki/src/config)

(provide build-page)

(define (build-page forms title)
  `(html ()
    (head ()
      (meta ([http-equiv "content-type"] [content "text/html; charset=UTF-8"]))
      (meta ([name "HandheldFriendly"] [content "True"]))
      (link ([rel "shortcut icon"] [href "./favicon.svg"]))
      (link ([type "text/css"] [rel "stylesheet"] [href "./nanowiki.css"]))
      (meta ([name "viewport"] [contents "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=yes"]))
      (title ,(format "~a - ~a" title site-title)))
    (body ()
      ,(header)
      (article
        (h1 ([class "title"]) ,(format "~a" title))
        ,@forms))))

(define (header)
  `(header ()
    (nav ()
      (a ([href "./index.html"]) ,site-title)
      (span
        (a ([href "./index.html"]) "About")
        (a ([href "./index.html"]) "Guides")))))
