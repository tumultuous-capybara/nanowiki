(module lang racket
  (module reader racket
    (require "./src/markup.rkt")
    (provide read-syntax)))
