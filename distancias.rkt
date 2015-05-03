#lang racket

(require "structs.rkt")
(require "funcions-varias.rkt")

;; distancia bloque de abaixo
;punto-pj + lista-bloques + lista (empty) -> número

(define (distancia-bloque-abaixo punto-pj lista lista-e l-cadrad)
  (cond
    ((empty? lista)
     (cond
       ((empty? lista-e)
        #f)
       (else
        (- (- (punto-y (punto-cadro->punto (argmin punto-c-y lista-e) l-cadrad)) (/ l-cadrad 2))
           (+ (punto-y punto-pj) l-cadrad)))))
    (else
     (distancia-bloque-abaixo 
      punto-pj empty
      (filter (lambda (n) 
                (and
                 (or
                  (= (punto-c-x (punto->punto-cadro punto-pj l-cadrad)) (punto-c-x n))
                  (= (punto-c-x (punto->punto-cadro (sumar-x-punto (- 1 (/ l-cadrad 2)) punto-pj) l-cadrad)) (punto-c-x n))
                  (= (punto-c-x (punto->punto-cadro (sumar-x-punto (- (/ l-cadrad 2) 1) punto-pj) l-cadrad)) (punto-c-x n)))
                 (>= (- (- (punto-y (punto-cadro->punto n l-cadrad)) (/ l-cadrad 2)) 
                        (+ (punto-y punto-pj) l-cadrad)) 
                     (- (/ l-cadrad 2)))
                 ))
              lista) l-cadrad))))

(provide distancia-bloque-abaixo)

;; distancia bloque de arriba
;punto-pj + lista-bloques + lista (empty) -> número

(define (distancia-bloque-enriba x punto-pj lista lista-e l-cadrad)
  (define estado-m (pj-estado-m (xogo-pj x)))
  (define estado-s (pj-estado-s (xogo-pj x)))
  (cond
    ((empty? lista)
     (cond
       ((empty? lista-e)
        #f)
       (else
        (- (if (and
                (member #\s estado-m)
                (equal? estado-s "floor"))
               (punto-y punto-pj)
               (- (punto-y punto-pj) l-cadrad))
           (+ (punto-y (punto-cadro->punto (argmax punto-c-y lista-e) l-cadrad)) (/ l-cadrad 2))))))
    (else
     (distancia-bloque-enriba
      x punto-pj empty
      (filter (lambda (n)
                (and
                 (or
                  (= (punto-c-x (punto->punto-cadro punto-pj l-cadrad)) (punto-c-x n))
                  (= (punto-c-x (punto->punto-cadro (sumar-x-punto (- 1 (/ l-cadrad 2)) punto-pj) l-cadrad)) (punto-c-x n))
                  (= (punto-c-x (punto->punto-cadro (sumar-x-punto (- (/ l-cadrad 2) 1) punto-pj) l-cadrad)) (punto-c-x n)))
                 (>= (- (if (and
                             (member #\s estado-m)
                             (equal? estado-s "floor"))
                            (punto-y punto-pj)
                            (- (punto-y punto-pj) l-cadrad))
                        (- (punto-y (punto-cadro->punto n l-cadrad)) (/ l-cadrad 2))) 
                     (- (/ l-cadrad 2)))
                 ))
              lista) l-cadrad))))

(provide distancia-bloque-enriba)

;; distancia bloque da esquerda
;punto-pj + lista-bloques + lista (empty) -> número

(define (distancia-bloque-esquerda x punto-pj lista lista-e l-cadrad)
  (define estado-m (pj-estado-m (xogo-pj x)))
  (define estado-s (pj-estado-s (xogo-pj x)))
  (cond
    ((empty? lista)
     (cond
       ((empty? lista-e)
        #f)
       (else
        (- (- (punto-x punto-pj) (/ l-cadrad 2))
           (+ (punto-x (punto-cadro->punto (argmax punto-c-x lista-e) l-cadrad)) (/ l-cadrad 2))))))
    (else
     (distancia-bloque-esquerda 
      x punto-pj empty
      (filter (lambda (n)
                (and
                 (or
                  (= (punto-c-y (punto->punto-cadro punto-pj l-cadrad)) (punto-c-y n))
                  (if (and
                       (member #\s estado-m)
                       (equal? estado-s "floor"))
                      #f
                      (= (punto-c-y (punto->punto-cadro (sumar-y-punto (- 1  l-cadrad) punto-pj) l-cadrad)) (punto-c-y n)))
                  (= (punto-c-y (punto->punto-cadro (sumar-y-punto (- l-cadrad 1) punto-pj) l-cadrad)) (punto-c-y n)))
                 (>=  (- (- (punto-x punto-pj) (/ l-cadrad 2))
                         (- (punto-x (punto-cadro->punto n l-cadrad)) (/ l-cadrad 2))) 
                      0)
                 ))
              lista
              ) l-cadrad))))

(provide distancia-bloque-esquerda)

;; distancia bloque da dereita
;punto-pj + lista-bloques + lista (empty) -> número

(define (distancia-bloque-dereita x punto-pj lista lista-e l-cadrad)
  (define estado-m (pj-estado-m (xogo-pj x)))
  (define estado-s (pj-estado-s (xogo-pj x)))
  (cond
    ((empty? lista)
     (cond
       ((empty? lista-e)
        #f)
       (else
        (- (- (punto-x (punto-cadro->punto (argmin punto-c-x lista-e) l-cadrad)) (/ l-cadrad 2))
           (+ (punto-x punto-pj) (/ l-cadrad 2))))))
    (else
     (distancia-bloque-dereita
      x punto-pj empty 
      (filter (lambda (n)
                (and
                 (or
                  (= (punto-c-y (punto->punto-cadro punto-pj l-cadrad)) (punto-c-y n))
                  (if (and
                       (member #\s estado-m)
                       (equal? estado-s "floor"))
                      #f
                      (= (punto-c-y (punto->punto-cadro (sumar-y-punto (- 1  l-cadrad) punto-pj) l-cadrad)) (punto-c-y n)))
                  (= (punto-c-y (punto->punto-cadro (sumar-y-punto (- l-cadrad 1) punto-pj) l-cadrad)) (punto-c-y n)))
                 (<=  (- (- (punto-x punto-pj) (/ l-cadrad 2))
                         (- (punto-x (punto-cadro->punto n l-cadrad)) (/ l-cadrad 2))) 
                      0)
                 ))
              lista
              ) l-cadrad))))

(provide distancia-bloque-dereita)