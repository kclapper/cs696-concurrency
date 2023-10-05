#lang racket

(define size 250000) ;; 1 MB

(define (random-char)
  (random 65 90))

(parameterize ([current-output-port (open-output-file "input.txt"
                                                      #:exists 'replace)])
  (for ([i (in-range size)])
    (write-byte (random-char))))
