#lang racket/base

;car regresa el primer elemento de la lista
;cdr regresa toda la lista excepto el primer elemento


;Suma de elementos


(define lista '(1 2 3))

(define (suma-elementos lst)
  (if (null? lst)
      0
      (+ (car lst) (suma-elementos (cdr lst)))))

(define x (suma-elementos lista))
(display x)
(newline)
;Promedio de elementos


(define (promedio-elementos lst)
  (if (null? lst)
      0
      (/ (suma-elementos lst) (length lst))))

(define x2 (promedio-elementos lista))
(display x2)
(newline)

;Lista inversa

(define (lista-inversa lst)
  (if (null? lst)
      '()
      (append (lista-inversa (cdr lst)) (list (car lst)))))

(define x3 (lista-inversa lista))
(display x3)
(newline)

;Eliminar duplicados
(require racket/set)
(define (eliminar-duplicados lst)
  (if (null? lst)
      '()
      (let ((my-set (list->set lst)))
        (set->list my-set))))

(define x4 (eliminar-duplicados '(1 1 2 3 2 4)))
(display x4)
(newline)

;Sortear

;de https://gist.github.com/lambrospetrou/970bcf188eb6061e1bfbfa9e36c29042
(define (qsrt iarr lt)
  (cond
    [(> (length iarr) 1)
     (let* ([pivot (car iarr)]
            [gt (lambda (l r) (not (or (lt l r) (equal? l r))))])
       (append
        (qsrt (filter (lambda (x) (lt x pivot)) iarr) lt)
        (filter (lambda (x) (equal? x pivot)) iarr)
        (qsrt (filter (lambda (x) (gt x pivot)) iarr) lt)))]
    [else iarr]))

;(displayln (qsrt '("hello" "world" "hi" "hell") string<?))
(displayln (qsrt '(6 5 4 7 4 2 1 8 9 0) <))


