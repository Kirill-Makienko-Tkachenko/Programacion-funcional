#lang racket/base
; Problema 1

(define (fahrenheit-a-celsius f)
  (* (/ (- f 32) 9) 5))

(define temperatura-fahrenheit 68)
(define temperatura-celsius (fahrenheit-a-celsius temperatura-fahrenheit))
(display "La temperatura en grados Celsius es: ")
(display temperatura-celsius)
(newline)
; Problema 2
(define (sign n)
  (cond
    ((< n 0) -1) ; Si n es negativo, devuelve -1
    ((> n 0) 1) ; Si n es positivo mayor que
    cero, devuelve 1
    (else 0))) ; En cualquier otro caso (n es cero), devuelve 0

; Problema 3
(define (roots a b c)
  (define discriminante (- (* b b) (* 4 a c)))
  (if (< discriminante 0)
      "La ecuación no tiene raíces reales"
      (list (/ (+ (- b) (sqrt discriminante)) (* 2 a))
            (/ (- (- b) (sqrt discriminante)) (* 2 a)))))

; Problema 4
(define (calcular-bmi peso altura)
  (define bmi (/ peso (* altura altura)))
  (if (<= bmi 18.5)
      "Bajo peso"
      (if (<= bmi 24.9)
          "Peso normal"
          (if (<= bmi 29.9)
              "Sobrepeso"
              "Obesidad"))))

; Problema 5
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

; Problema 6
define (duplicate lst)
(cond
  ((null? lst) '()) ; Si la lista es vacía,
  devuelve una lista vacía
  (else
   (cons (car lst) (cons (car lst) (duplicate (cdr lst)))))))
; Problema 7
(define (pow a b)
  (if (= b 0)
      1
      (* a (pow a (- b 1)))))

; Problema 8
(define (fib n)
  (if (or (= n 1) (= n 2))
      1
      (+ (fib (- n 1)) (fib (- n 2)))))
; Problema 9
(define (enlist lst)
  (cond ((null? lst) '())
        ((list? (car lst))
         (append (enlist (car lst)) (enlist (cdr lst))))
        (else (cons (car lst) (enlist (cdr lst))))))
; Problema 10
(define (positives lst)
(cond ((null? lst) '()) ; Si lst es vacía, devolver una lista vacía
      ((positive? (car lst)) ; Si el primer elemento de lst es positivo,
       (cons (car lst) (positives (cdr lst)))) ; agregarlo a la lista resultante y llamar recursivamente a la función con el resto de la lista
      (else (positives (cdr lst))))) ;Si el primer elemento de lst es negativo o cero, llamar recursivamente a la función con el resto de la lista
; Problema 11
(define (add-list lst)
  (cond ((null? lst) 0) ; Si lst es vacía, devolver 0
        (else (+ (car lst) (add-list (cdr lst))))))

; Problema 12
(define (invert-pairs lst)
  (cond ((null? lst) '())
        (else (cons (list (cadr (car lst)) (car (car lst)))
                    (invert-pairs (cdr lst))))))
; Problema 13
(define (list-of-symbols? lst)
  (cond ((null? lst) #t) ; Si lst es vacía, devolver verdadero
        ((symbol? (car lst)) ; Si el primer elemento de lst es un símbolo,
         (list-of-symbols? (cdr lst))) ; llamar recursivamente a la función con el resto de la lista
        (else #f))) ; Si el primer elemento de lst no es un símbolo, devolver falso
; Problema 14
(define (swapper a b lst)
  (cond ((null? lst) '()) ; Si la lista está vacía, devuelve una lista vacía
        ((eq? (car lst) a) (cons b (swapper a b (cdr lst)))) ; Si el primer elemento es igual a 'a', lo reemplaza por 'b'
        ((eq? (car lst) b) (cons a (swapper a b (cdr lst)))) ; Si el primer elemento es igual a 'b', lo reemplaza por 'a'
        (else (cons (car lst) (swapper a b (cdr lst)))))) ; Si el primer elemento no es igual ni a 'a' ni a 'b', lo deja como está y sigue con el resto de la lista
; Problema 15
(define (dot-product a b)
  (if (or (empty? a) (empty? b))
      0
      (+ (* (first a) (first b))
         (dot-product (rest a) (rest b)))))

; Problema 16
(define (average lst)
  (if (null? lst) ; Si lst es vacía, devolver 0
      0
      (/ (apply + lst) (length lst)))) ; Sumar los elementos de lst y dividir por la cantidad de elementos en lst

; Problema 17
(define (standard-deviation lst)
  (if (null? lst) ; Si lst es vacía, devolver 0
      0
      (let* ((n (length lst)) ; Obtener la cantidad de elementos en lst
             (mean (/ (apply + lst) n)) ; Calcular la media aritmética de lst
             (squares (map (lambda (x) (* (- x mean) (- x mean))) lst)) ; Calcular el cuadrado de la diferencia entre cada elemento de lst y la media aritmética
             (variance (/ (apply + squares) n))) ; Calcular la varianza de lst
        (sqrt variance)))) ; Calcular la raíz cuadrada de la varianza para obtener la desviación estándar

(define (replic lst n)
  (if (or (null? lst) (zero? n)) ; Si lst es vacía o n es cero, devolver una lista vacía
      '()
      (apply append (map (lambda (x) (make-list n x)) lst)))) ; Crear una lista que replica n veces cada elemento en lst y concatenarlas

(define (expand lst)
  (define result '())
  (for ([i (length lst)])
    (set! result (append result (make-list (+ i 1) (list-ref lst i)))))result)

(define (binary n)
  (cond ((zero? n) '())
        (else (append (binary (quotient n 2)) (list (modulo n 2))))))