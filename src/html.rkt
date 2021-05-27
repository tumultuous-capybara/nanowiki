#lang racket

(provide build-page-xexpr header footer)

(define (build-page-xexpr forms title)
  `(html ()
    (head ()
      (meta ([http-equiv "content-type"] [content "text/html; charset=UTF-8"]))
      (meta ([name "HandheldFriendly"] [content "True"]))
      (link ([rel "shortcut icon"] [href "./favicon.svg"]))
      (link ([type "text/css"] [rel "stylesheet"] [href "./nanowiki.css"]))
      (meta ([name "viewport"] [contents "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=yes"]))
      (title ,(format "~a" title)))
    (body ()
      ,(header)
      (article
        (h1 ([class "title"]) ,(format "~a" title))
        ,@forms)
      ,(footer))))

(define (header)
  `(header () (a ([href "./index.html"]) "capybara.wiki")))

(define (footer)
  `(footer () "— Powered by Nanowiki"))
