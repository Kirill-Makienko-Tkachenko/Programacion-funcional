#lang racket/base

;Contar objetos en una pila

(define (contar-libros libros)
  (cond
    ((null? libros) 0)            ; Base case: if libros is empty, return 0
    ((not (null? (car libros)))    ; If the first element of libros is not null, car es el primer elemento de libros
     (+ 1 (contar-libros (cdr libros)))) ; increment count by 1 and recursively call contar-libros with the rest of the list, cdr es elimier primer elemento de la lista
    (else "Error")))               ; Handle the case if neither of the above conditions is met




(define (qsrt iarr lt)
  (cond
    [(< 1 (length iarr))
     (let (
           [pivot (car iarr)]
           [gt (lambda (l r) (not (or (lt l r) (equal? l r))))])
       (append
        (qsrt (filter (lambda (x) (lt x pivot)) iarr) lt)
        (filter (lambda (x) (equal? x pivot)) iarr)
        (qsrt (filter (lambda (x) (gt x pivot)) iarr) lt)))]
    [else iarr]))

(qsrt '("hello" "world" "hi" "hell") string<?)
  
  


(define (main)
  (define pila-de-libros '(libro1 libro2 libro3 libro4))
  (display "La cantidad de libros en la pila es: ")
  (display(contar-libros pila-de-libros))
  (newline))

(main)