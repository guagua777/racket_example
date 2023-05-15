#lang racket

(require racket/set racket/stream)
(require racket/fixnum)
(require graph)

(define g (directed-graph '((a b) (b c))))

(add-directed-edge! g 'a 'c)

(add-directed-edge! g 'a 'd)

(has-edge? g 'a 'c)

(has-edge? g 'c 'a)

(get-edges g)

(get-vertices g)

(get-neighbors g 'b)

(add-directed-edge! g 'd 'e)

(tsort g)

(define g^ (transpose g))

(get-edges g^)

(tsort g^)

;(add-directed-edge! g^ 'd (set 'm 'n))

(get-edges g^)

(add-directed-edge! g^ 'f 'c)

(get-edges g^)

(sequence->list (in-neighbors g^ 'c))

;;Each edge is represented by a list of two vertices
;;where the first vertex in the list is the source and the second vertex is the destination. 

