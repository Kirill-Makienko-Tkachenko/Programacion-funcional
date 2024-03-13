#lang racket/base


(define (fahrenheit-a-celsius f)
    (* (/ (- f 32) 9) 5))

; Imprimir
(define temperatura-fahrenheit 68)
  (define temperatura-celsius (fahrenheit-a-celsius temperatura-fahrenheit))
  (display "La temperatura en grados Celsius es: ")
  (display temperatura-celsius)
  (newline)

;Nueva funcion

(define (sign n)
  (cond
    ((< n 0 ) "Negativo")
    ((> n 0) "Positivo")
    (else 0)))

(define x (sign -10))
(display "El numero es: ")
(display x)
(newline)

(define (roots a b c)
  (define discriminante (-(* b b) (* 4 a c)))
    (if (< discriminante 0)
        ("La ecuacion no tiene raiz real")
    (list (/ (+ (- b) (sqrt discriminante)) (* 2 a))
          (/ (- (- b) (sqrt discriminante)) (* 2 a)))))
(define x2 (roots 1 -3 2))
(display "El numero es: ")
(display x2)
(newline)

(define (calcular-bmi peso altura)
  (define bmi (/ peso (* altura altura)))
  (if (<= bmi 18.5)
      "Bajo peso"
      (if(<= bmi 24.9)
         "Peso Normal"
         (if (<= 29.9)
             (begin
                (display "Sobrepeso")
                (newline)
                (display "El BMI es: ")
                (display bmi))
             ;else
             "Obesidad"))))

(define x3 (calcular-bmi 102 1.97))
(display x3)
(newline)

(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(define (duplicate lst)
  (cond
    ((null? lst) '())  ; Si la lista es vacía, devuelve una lista vacía
    (else (cons (car lst) (cons (car lst) (duplicate (cdr lst))))))) ; Duplica el primer elemento y llama recursivamente a duplicate con el resto de la lista

;pow function

(define (pow a b)
  (if (= b 0)
      1 ;return implicito
      (* a( pow a(- b 1)))))

(define funcion-poder (pow 15 10))
(display funcion-poder)
(newline)
;Fibonacci

(define (fib n)
   (if (<= n 1)
      n
      (+ (fib(- n 1))(fib(- n 2)))))

(define fibo (fib 3))
(display fibo)

;Enlist

(define (enlist lst)
  (cond ((null? lst)'())
        ((list?(car lst))
         (append (enlist(car lst))(enlist(cdr lst))))
        (else (cons(car lst)(enlist(cdr lst))))))

        
(define enlist2 (enlist '(a b c d e f g)))
(display enlist2)


;positivos

(define (positivos 'list)
   (cond
    ((null? list) list); Base case: if libros is empty, return 0
    ((not (null? append((car list)))    ; If the first element of libros is not null, car es el primer elemento de libros
     ((positivos (cdr positivos)))))))
(define numeros '(15 16 1 0 -2 -5))
(display numeros)


;Lista

(define (add-list lst)
  (if (null? lst)              
      0                        
      (+ (car lst)             
         (add-list (cdr lst))))) 


(display (add-list '())) 
(newline)
(display (add-list '(2 4 1 3))) 
(newline)
(display (add-list '(1 2 3 4 5 6 7 8 9 10)))


;Pares invertidos

(define (invert-pairs lst)
  (if (null? lst)
      '()
      (cons (list (cadr (car lst)) (car (car lst)))
            (invert-pairs (cdr lst)))))

(display (invert-pairs '()))
(newline)
(display (invert-pairs '((a 1) (a 2) (b 1) (b 2))))
(newline)
(display (invert-pairs '((January 1) (February 2) (March 3))))



;solo caracteres

(define (list-of-symbols? lst)
  (cond ((null? lst) #t)
        ((symbol? (car lst)) (list-of-symbols? (cdr lst)))
        (else #f)))

(display (list-of-symbols? '()))
(newline)
(display (list-of-symbols? '(a b c d e)))
(newline)
(display (list-of-symbols? '(a b c d 42 e)))

;Swapper

(define (swapper a b lst)
  (map (lambda (x) (if (equal? x a) b (if (equal? x b) a x))) lst))

(define lst '(1 2 3 4 5))
(define nueva_lst (swapper 2 4 lst))
(display "Lista después de intercambiar 2 y 4: ")
(display nueva_lst)
(newline)

;

