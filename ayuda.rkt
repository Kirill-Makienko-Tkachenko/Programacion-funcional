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

;Calcular el promedio de una lista

(define (promedio-lista n x)
  (define total 0)
  (for ([i (in-range n)]) ;for i in range 0 -> x
    (cons x lista)
    (+ total x)
        )
  (define promedio (/ total n)))

(define y (promedio-lista 3 '(1,2,3)))

  
  