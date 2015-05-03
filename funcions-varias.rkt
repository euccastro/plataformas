#lang racket

;; FUNCIÓNS VARIAS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "structs.rkt")
(require 2htdp/image)

;;Función para por un elemento en un vector en unha posición especifica

(define (por-e-vector elemento posicion vector)
  (cond
    ((or
      (>= posicion (vector-length vector))
      (< posicion 0))
     vector)
    (else
     (vector-append
      (vector-take vector posicion)
      (make-vector 1 elemento)
      (vector-take-right vector (-  (vector-length vector) (+ posicion 1)))))))

(provide por-e-vector)

;;convertir puntos en texto
;lista

(define (puntos->texto lista-puntos)
  (cond 
    ((empty? lista-puntos)
     "")
    (else
     (string-append 
      "(punto-c " (number->string (punto-c-x (car lista-puntos))) " " (number->string (punto-c-y (car lista-puntos))) ") "
      (puntos->texto (cdr lista-puntos))))))

(provide puntos->texto)

;;voltear lista
;lista + lista(empty) -> lista

(define (voltear lista emp)
  (cond
    ((empty? lista)
     emp)
    (else
     (voltear (cdr lista) (cons (car lista) emp)))))

(provide voltear)

;;FUNCIÓNS PARA ACELERAR COLISIÓNS:
;;COLLÉN DIRECTAMENTE OS PUNTOS ADECUADOS:

;;ORDENA PUNTO A PUNTO
; (punto 0 0) -> (punto 0 1) -> (punto 0 2)

(define (ordenar-punto-rev px py lista-p lista n-c-alt n-c-anch)
  (cond
    ((>= py n-c-alt)
     (list->vector (reverse lista)))
    ((< px (- n-c-anch 1))
     (ordenar-punto-rev (+ px 1) py lista-p (cons
                                             (filter (lambda (x)
                                                       (and (= px (punto-c-x x))
                                                            (= py (punto-c-y x))))
                                                     lista-p)
                                             lista) n-c-alt n-c-anch))
    (else
     (ordenar-punto-rev 0 (+ py 1) lista-p (cons
                                            (filter (lambda (x)
                                                      (and (= px (punto-c-x x))
                                                           (= py (punto-c-y x))))
                                                    lista-p)
                                            lista) n-c-alt n-c-anch))))

(provide ordenar-punto-rev)

(define (s-vector-ref vector n)
  (cond
    ((or
      (< n 0)
      (>= n (vector-length vector)))
     empty)
    (else
     (vector-ref vector (inexact->exact (round n))))))

(provide s-vector-ref)

(define (calc-apart-vect pc-y pc-x n-c-anch)
  (cond
    ((or
      (< pc-x 0)
      (>= pc-x n-c-anch))
     -1)
    (else
     (+ (* pc-y n-c-anch) pc-x))))

(provide calc-apart-vect)

(define (coller-puntos-en-caida p-x p-y vect vel-caida sumando l-cadrad n-c-anch)
  (cond
    ((>= vel-caida l-cadrad)
     (append
      (append 
       (s-vector-ref vect (calc-apart-vect (+ p-y sumando) p-x n-c-anch))
       (if (not (<= p-x 0)) 
           (s-vector-ref vect (calc-apart-vect (+ p-y sumando) (- p-x 1) n-c-anch)) empty)
       (if (< p-x n-c-anch) 
           (s-vector-ref vect (calc-apart-vect (+ p-y sumando) (+ p-x 1) n-c-anch)) empty))
      (coller-puntos-en-caida p-x p-y vect (- vel-caida l-cadrad) (+ sumando 1) l-cadrad n-c-anch)))
    (else
     empty)))

(provide coller-puntos-en-caida)

(define (coller-puntos-circundantes x pc-x pc-y vector n-c-anch estad-s ve-c l-cadrad)
  (append
   (s-vector-ref vector (calc-apart-vect pc-y  pc-x n-c-anch)) ;;puntos donde esta o pj
   (s-vector-ref vector (calc-apart-vect (- pc-y 1)  pc-x n-c-anch))
   (s-vector-ref vector (calc-apart-vect (- pc-y 2)  pc-x n-c-anch)) ;; punto arriba
   (s-vector-ref vector (calc-apart-vect (+ pc-y 1)  pc-x n-c-anch)) ;; punto abaixo
   (s-vector-ref vector (calc-apart-vect (+ pc-y 2)  pc-x n-c-anch)) ;;punto abaixo x2
   (if (not (<= pc-x 0))
       (append
        (s-vector-ref vector (calc-apart-vect pc-y  (- pc-x 1) n-c-anch))       ;;puntos anteriores en x
        (s-vector-ref vector (calc-apart-vect (- pc-y 1)  (- pc-x 1) n-c-anch))
        (s-vector-ref vector (calc-apart-vect (- pc-y 2)  (- pc-x 1) n-c-anch))
        (s-vector-ref vector (calc-apart-vect (+ pc-y 1)  (- pc-x 1) n-c-anch))
        (s-vector-ref vector (calc-apart-vect (+ pc-y 2)  (- pc-x 1) n-c-anch)))
       empty)
   (if (not (>= pc-x n-c-anch))
       (append
        (s-vector-ref vector (calc-apart-vect pc-y  (+ pc-x 1) n-c-anch)) ;;puntos posteriores en x
        (s-vector-ref vector (calc-apart-vect (- pc-y 1)  (+ pc-x 1) n-c-anch))
        (s-vector-ref vector (calc-apart-vect (- pc-y 2)  (+ pc-x 1) n-c-anch))
        (s-vector-ref vector (calc-apart-vect (+ pc-y 1)  (+ pc-x 1) n-c-anch))
        (s-vector-ref vector (calc-apart-vect (+ pc-y 2)  (+ pc-x 1) n-c-anch)))
       empty)
   (if (equal? estad-s "down")
       (append 
        (s-vector-ref vector (calc-apart-vect (+ pc-y 3) pc-x n-c-anch))
        (if (not (<= pc-x 0)) 
            (s-vector-ref vector (calc-apart-vect (+ pc-y 3) (- pc-x 1) n-c-anch)) empty)
        (if (< pc-x n-c-anch) 
            (s-vector-ref vector (calc-apart-vect (+ pc-y 3) (+ pc-x 1) n-c-anch)) empty))
       empty)
   (coller-puntos-en-caida pc-x pc-y vector ve-c 3 l-cadrad n-c-anch)
   (if (equal? estad-s "up")
       (append
        (s-vector-ref vector (calc-apart-vect (- pc-y 3) pc-x n-c-anch))
        (if (not (<= pc-x 0))
            (s-vector-ref vector (calc-apart-vect (- pc-y 3) (- pc-x 1) n-c-anch)) empty)
        (if (< pc-x n-c-anch)
            (s-vector-ref vector (calc-apart-vect (- pc-y 3) (+ pc-x 1) n-c-anch)) empty))
       empty)
   ))

(provide coller-puntos-circundantes)

(define (elemento-en-lista? e lista)
  (cond
    ((empty? lista) #f)
    ((equal? e (car lista)) #t)
    (else (elemento-en-lista? e (cdr lista)))))

(provide elemento-en-lista?)

;; volve un número positivo

(define (positivo n)
  (cond
    ((< n 0)
     (- n))
    (else n)))

(provide positivo)

;elemento + lista -> lista
;pon un elemento nunha lista

(define tecla-agacharse (list #\s "down"))

(define (por-elemento-lista x e lista)
  (define estado-s (pj-estado-s (xogo-pj x)))
  (cond
    ((empty? lista)
     (cons e lista))
    ((equal? e (car lista))
     lista)
    ((member (car lista) tecla-agacharse)
     (cons e (remove #\s lista)))
    (else
     (cons (car lista) (list e)))))

(provide por-elemento-lista)

;;;


;punto-c -> punto

(define (punto-cadro->punto p-c l-cadrad)
  (punto
   (+ (* (punto-c-x p-c) l-cadrad) (/ l-cadrad 2))
   (+ (* (punto-c-y p-c) l-cadrad) (/ l-cadrad 2))))

(provide punto-cadro->punto)

;;punto -> punto-c

(define (punto->punto-cadro p l-cadrad)
  (punto-c
   (floor (/ (punto-x p) l-cadrad))
   (floor (/ (punto-y p) l-cadrad))))

(provide punto->punto-cadro)

;numero + punto -> punto

(define (sumar-x-punto n punto-pj)
  (punto
   (+ (punto-x punto-pj) n)
   (punto-y punto-pj)))

(provide sumar-x-punto)

(define (sumar-y-punto n punto-pj)
  (cond
    ((punto? punto-pj)
     (punto
      (punto-x punto-pj)
      (+ (punto-y punto-pj) n)))
    (else
     (punto-c
      (punto-c-x punto-pj)
      (+ (punto-c-y punto-pj) n)))))

(provide sumar-y-punto)

;quitar ultimo elemento de unha lista
;lista + empty -> lista

(define (quitar-ultimo lista lista2)
  (cond
    ((empty? lista)
     lista)
    ((empty? (cdr lista))
     (reverse lista2))
    (else
     (quitar-ultimo (cdr lista) (cons (car lista) lista2)))))

(provide quitar-ultimo)

;elevar número ao cadrado
(define (cadrado n) (* n n))

(provide cadrado)

;distancia 2 puntos-c
(define (dist-puntos-c p1 p2)
  (sqrt (+ (cadrado (- (punto-c-x p2) (punto-c-x p1)))
           (cadrado (- (punto-c-y p2) (punto-c-y p1))))))

(provide dist-puntos-c)

;distancia 2 puntos
(define (dist-puntos p1 p2)
  (sqrt (+ (cadrado (- (punto-x p2) (punto-x p1)))
           (cadrado (- (punto-y p2) (punto-y p1))))))

(provide dist-puntos)