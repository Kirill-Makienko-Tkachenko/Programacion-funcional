#lang racket/base

; Problema 1

(define (insert n lst)
  (cond
    [(null? lst) (list n)] ; Cuando lst es una lista vacía
    [(<= n (car lst)) (cons n lst)] ; Si n es menor o igual al primer elemento de lst (car), crea una lista (cons)
    [else (cons (car lst) (insert n (cdr lst)))])) ; En cualquier otro caso

; Ejemplos de uso de la función insert
(display (insert 14 '())) ; Salida esperada: (14)
(newline)
(display (insert 4 '(5 6 7 8))) ; Salida esperada: (4 5 6 7 8)
(newline)
(display (insert 5 '(1 3 6 7 9 16))) ; Salida esperada: (1 3 5 6 7 9 16)
(newline)
(display (insert 10 '(1 5 6))) ; Salida esperada: (1 5 6 10)

; Problema 2

(define (insertion-sort lst)
  (cond
    [(null? lst) '()] ;caso donde lista el vacia
    [else (insert (car lst) (insertion-sort (cdr lst)))])) ;Regresa una lista con un element


; Problema 3

(define (rotate-left n lst)
  (let ((len(length lst)) ; Define len como longitud de la lst
        (m (modulo n(length lst)))) ; define m como n modulo longitud de la lista
    (if (negative? n); 
        (rotate-left (+ len n) lst)
        (append (drop m lst) (take m lst))))) ; crea una lista con los elementos que deja del final de la lista original y luego elementos que toma de la lista original asi "reordenando" la lista
 ; Take: toma los primeros n elementos de una lista
; (take 3 '(1 2 3 4 5) = '(1 2 3)

; Drop: omite los primeros n elementos de una lista
; (drop 3 '(1 2 3 4 5) = '(4 5)

; Problema 4
(define (prime-factors n)
  (let loop ((n n) ; loop es nombre de variable del bloque de codigo de abajo, en este caso se inicializa variable n con valor de n
             ; En este caso usamos loop como nombre dado al blouqe de codigo de n n hasta acc '()
             (f 2) ; se Inicializa variable f con valor 2
             (acc '())) ; se inicia un acumulador con una lista vacia
    (cond ((<= n 1) acc)
          ((= (modulo n f) 0) (loop (/ n f) f (cons f acc)))
          (else (loop n (+ f 1) acc)))))

; Problema 5

(define (gcd a b) ; Maximo comun divisor
  (if(= b 0) ; caso base si b es igual a 0
     a ; Regresa a
     (gcd b (modulo a b)))) ; caso contrario regresa a como b y b como a modulo b

; Problema 6
(define (deep-reverse lst)
  (cond ((null? lst) '()) ; caso base
        ((list? (car lst)) (append (deep-reverse(cdr lst)) ; si el dato es una lista entonces solamente esa lista la regresa a deep reverse para que se reverse
                                   (list (deep-reverse (car lst))))) ; luego regresa y añade al final la lista revertida a la lista original
        (else (append (deep-reverse (cdr lst)) (list (car lst)))))) ; caso para cualquier otro caso

; Problema 7

(define (insert-anywhere x lst)
  (if (null? lst)
      (list (list x))
      (append (list (cons x lst))
              (map (lambda (lst2) (cons (car lst) lst2)) (insert-anywhere x (cdr lst))))))

; Problema 8

(define (pack lst)
  (if (null? lst)
      '()
      (let ((first (car lst))
            (rest (cdr lst)))
        (let loop ((current-group (list first))
                   (remaining rest)
                   (result '()))
          (cond ((null? remaining) (reverse (cons current-group result)))
                ((equal? first (car remaining))
                 (loop (cons (car remaining) current-group) (cdr remaining) result))
                (else
                 (loop (list (car remaining)) (cdr remaining) (cons current-group result))))))))

; Problema 9

(define (compress lst)
  (if (null? lst)
      '()
      (let loop ((current (car lst))
                 (remaining (cdr lst))
                 (result '()))
        (cond ((null? remaining) (reverse (cons current result)))
              ((equal? current (car remaining))
               (loop current (cdr remaining) result))
              (else
               (loop (car remaining) (cdr remaining) (cons current result)))))))

; Problema 10

(define (encode lst)
  (define (encode-helper lst current count)
    (cond ((null? lst) (reverse current))
          ((eq? (car lst) (car current))
           (encode-helper (cdr lst) current (+ count 1)))
          (else
           (encode-helper (cdr lst) (cons (cons count (car current)) current) 1))))
  (encode-helper lst (list (cons 0 'dummy)) 0))

; Problema 11

(define (encode-modified lst)
  (define (encode-helper lst prev-count prev-elem result)
    (cond ((null? lst)
           (reverse (cons
                     (list prev-count prev-elem) result)))
          ((equal? prev-elem (car lst))
           (encode-helper (cdr lst) (+ 1 prev-count) prev-elem result))
          (else
           (encode-helper (cdr lst) 1 (car lst) (cons (list prev-
                                                            count prev-elem) result)))))
  (encode-helper (cdr lst) 1 (car lst) '()))

; Problema 12

(define (decode lst)
  (define (repeat e n) ; Función auxiliar para repetir un elemento e, n veces.
    (if (= n 0)
        '()
        (cons e (repeat e (- n 1)))))
  (define (decode-helper lst)
    (if (null? lst)
        '()
        (let ((elem (car lst)))
          (cond ((list? elem) ; Si elem es una lista, significa que tenemos el formato (n e).
                 (append (repeat (cadr elem) (car elem)) (decode-helper (cdr lst))))
                (else ; De lo contrario, simplemente agregamos elem a la lista resultante.
                 (cons elem (decode-helper (cdr lst))))))))
  (decode-helper lst))


; Problema 13

(define (args-swap f)
  (lambda (x y)
    (f y x)))

; Problema 14

(define (there-exists-one? pred lst)
  (define (helper lst count)
    (cond ((null? lst) (= count 1)) ; Si la lista está vacía, verifica si el conteo es exactamente 1.
          ((pred (car lst)) ; Si pred sobre el primer elemento es verdadero, incrementa el conteo.
           (if (> count 0) ; Si ya encontramos un elemento que satisface pred, retorna falso inmediatamente.
               #f
               (helper (cdr lst) (+ count 1))))
          (else (helper (cdr lst) count)))) ; Si pred es falso, continúa con el resto de la lista sin cambiar el conteo.
  (helper lst 0))

; Problema 15

(define (linear-search lst x eq-fun)
  (define (search-helper lst x eq-fun index)
    (cond ((null? lst) #f) ; Si la lista está vacía, retorna #f.
          ((eq-fun (car lst) x) index) ; Si el elemento actual es igual a x, retorna el índice.
          (else (search-helper (cdr lst) x eq-fun (+ index 1))))) ; De lo contrario, continúa con el siguiente elemento.
  (search-helper lst x eq-fun 0)) ; Inicia la búsqueda con índice 0.

; Problema 16

(define (df x)
(/ (- (f (+ x 0.001)) (f x)) 0.001))

(define (ddf x)
(/ (- (df (+ x 0.001)) (df x)) 0.001))

; Problema 17

(define (newton f guess)
  (define (next guess)
    (- guess (/ (f guess) (derivative f guess))))
  (define (derivative f x)
    (/ (- (f (+ x 0.0001)) (f x)) 0.0001))
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 0.000001))
  (define (iter guess)
    (if (close-enough? guess (next guess))
        guess
        (iter (next guess))))
  (iter guess))


; Problema 18


(define (integral a b n f)
  (define h (/ (- b a) n))
  (define (simpson k)
    (* (/ h 3)
       (+ (f (+ a (* k h)))
          (* 4 (f (+ a (* (+ k 1) h))))
          (f (+ a (* (+ k 2) h))))))
  (define (sum k)
    (if (> k (- n 2))
        0
        (+ (simpson k)
           (sum (+ k 2)))))
  (sum 0))

