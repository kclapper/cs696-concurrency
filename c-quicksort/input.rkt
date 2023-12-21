#lang racket

;;(define size 10000) ;; 10 KB
;;(define size 250000) ;; 250 KB
;;(define size 1000000) ;; 1000 KB
;;(define size 100000000) ;; 100 MB
;;(define size 1000000000) ;; 1 GB
(define size 10000000000) ;; 10 GB

(define (random-char)
  (random 65 91))

(parameterize ([current-output-port (open-output-file "input.txt"
                                                      #:exists 'replace)])
  (for ([i (in-range size)])
    (write-byte (random-char))))
