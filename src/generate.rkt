#lang racket

(require json "page.rkt" "markup.rkt")

(define (load-pages path)
  (for ([file-path (in-directory path)] #:when (path-has-extension? file-path ".scrbl"))
    (displayln (format "Starting load: ~a" file-path))
    (load-page file-path)))

(define (copy-resources src-dir dest-dir)
  (for ([file-path (in-directory src-dir)])
    (displayln (format "Copying resource: ~a" file-path))
    (copy-file file-path (format "~a~a" dest-dir (last (explode-path file-path))) #t)))

(define (write-index path)
  (let ((data (build-json-index)))
    (write-json data (open-output-file path #:exists 'replace))))

(displayln "--- NanoWiki version 0.3.2")
(load-pages "./pages/")
(displayln (format "--- ~a pages loaded" (length page-index)))
(delete-directory/files "./public/")
(make-directory* "./public/")
(write-all-pages)
(displayln (format "--- ~a pages written" (length page-index)))
(write-index "./public/index.json")
(displayln "--- Index data written")
(copy-resources "./src/resources" "./public/")
(displayln "--- All resources copied")
