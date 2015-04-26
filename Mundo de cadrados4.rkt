#lang racket

;CONSTANTES XOGO

(define l-cadrado 15)

(define l-cadrado-a l-cadrado)

(define n-c-ancho 50)
;;relacion -> n-c-alto = (/ n-c-ancho 2)
(define n-c-alto 25)

(define ancho-inicial (* l-cadrado n-c-ancho))
(define alto-inicial (* l-cadrado n-c-alto))

(define cambiando-tamaño #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;; MODULO BIG-BONG ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ancho-frame #f)
(define alto-frame #f)

(require (except-in racket/gui make-color make-pen))
(require 2htdp/image)
(require (only-in mrlib/image-core render-image))

(define (pairs l)
  (cond ((null? l) '())
        ((null? (cdr l)) (list l))
        (else
         (cons (list (car l) (cadr l))
               (pairs (cddr l))))))

(define (parse-handlers handlers)
  (define h (make-hasheq '((on-draw . #f)
                           (on-key-press . #f)
                           (on-key-release . #f)
                           (on-mouse-press . #f)
                           (on-mouse-release . #f)
                           (on-mouse-move . #f)
                           (on-wheel-down . #f)
                           (on-wheel-up . #f)
                           (on-tick . #f)
                           (tick-interval . #f)
                           (stop-when . #f))))
  (define not-found (list))
  (for ((pair (pairs handlers)))
    (let ((existing (hash-ref h (car pair) not-found)))
      (cond ((eq? existing not-found)
             (error "nom existe esse evento:" (car pair)))
            (existing
             (error "ja havia um manejador para esse evento:" (car pair)))
            (else
             (hash-set! h (car pair) (cadr pair))))))
  h)

(define (big-bong state . rest)
  (define handlers (parse-handlers rest))
  (define on-draw (hash-ref handlers 'on-draw))
  (when (not on-draw)
    (error "E necessario um manejador de on-draw!"))
  (define frame
    (let* ((image (on-draw state))
           (initial-width (image-width image))
           (initial-height (image-height image)))
      (new frame%
           (label "Mundo de Cadrados")
           (width initial-width)
           (height initial-height)
           (min-width ancho-inicial)
           (min-height alto-inicial)
           )))
  (define tela%
    (let ((pulsadas (mutable-seteq)))
      (class canvas%
        (field [rematou #f])
        (define/override (on-event event)
          (unless (get-field rematou this)
            (case (send event get-event-type)
              ((left-down) (handle-user-event 'on-mouse-press (send event get-x) (send event get-y) 'left))
              ((left-up) (handle-user-event 'on-mouse-release (send event get-x) (send event get-y) 'left))
              ((middle-down) (handle-user-event 'on-mouse-press (send event get-x) (send event get-y) 'middle))
              ((middle-up) (handle-user-event 'on-mouse-release (send event get-x) (send event get-y) 'middle))
              ((right-down) (handle-user-event 'on-mouse-press (send event get-x) (send event get-y) 'right))
              ((right-up) (handle-user-event 'on-mouse-release (send event get-x) (send event get-y) 'right))
              ((motion) (handle-user-event 'on-mouse-move (send event get-x) (send event get-y))))))
        (define/public (handle-user-event name . rest)
          (let ((h (hash-ref handlers name)))
            (when h
              (let ((estado-novo (apply h state rest)))
                (when (not (equal? estado-novo state))
                  (set! state estado-novo)
                  (let ((stop-when (hash-ref handlers 'stop-when)))
                    (when (and stop-when (stop-when state))
                      (set-field! rematou this #t)))
                  (send this refresh))))))
        (define/override (on-char event)
          (unless (get-field rematou this)
            (let ((pulsando (send event get-key-code)))
              (case pulsando
                ((release) (let ((soltando (send event get-key-release-code)))
                             (set-remove! pulsadas soltando)
                             (handle-user-event 'on-key-release soltando)))
                ((wheel-down) (handle-user-event 'on-wheel-down))
                ((wheel-up) (handle-user-event 'on-wheel-up))
                (else
                 (unless (set-member? pulsadas pulsando)
                   (set-add! pulsadas pulsando)
                   (handle-user-event 'on-key-press pulsando)))))))
        (super-new))))
  (define tela
    (new tela% (parent frame)
         (style '(no-autoclear))
         (paint-callback
          (λ (canvas dc)
            (let* ((image (on-draw state))
                   (width (image-width image))
                   (height (image-height image)))
              (when (or (not (= width (send tela get-width)))
                        (not (= height (send tela get-height))))
                ;(send frame resize width height)
                ;(send frame on-size width height)
                (set! ancho-frame (send frame get-width))
                (set! alto-frame (send frame get-height))
                ;(send frame min-width width)
                ;(send frame min-height height)
                )
              (render-image
               (on-draw state)
               dc
               0
               0))))))
  (let ((on-tick (hash-ref handlers 'on-tick)))
    (when on-tick
      (letrec ((timer (new timer%
                           (notify-callback (λ ()
                                              (if (and (send frame is-shown?) (not (get-field rematou tela)))
                                                  (send tela handle-user-event 'on-tick)
                                                  (send timer stop))))
                           (interval (inexact->exact (floor (* 1000 (or (hash-ref handlers 'tick-interval)
                                                                        1/30))))))))
        #f)))
  (send frame show #t)
  (send tela focus))

(define (di . args)
  (cond ((null? args)
         (newline)
         (flush-output))
        (else
         (print (car args))
         (display " ")
         (apply di (cdr args)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;; XOGO            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; CONSTANTES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define movemento-lateral (/ l-cadrado 4))

(define movemento-salto (/ l-cadrado 4))

;;;

(define ancho-fondo (* l-cadrado n-c-ancho))

(define n-c-ancho-aumentado 25)

(define n-c-ancho-real (+ n-c-ancho n-c-ancho-aumentado))

(define ancho-fondo-real (* l-cadrado (+ n-c-ancho n-c-ancho-aumentado)))

(define alto-fondo (* l-cadrado n-c-alto))

(define transparente (color 0 0 0 0))

(define fondo-0 (freeze (empty-scene ancho-fondo-real alto-fondo transparente)))


;;teclas de movemento:

(define tecla-salto '(#\w up #\space))

(define tecla-agacharse '(#\s down))

(define tecla-dereita '(#\d right))

(define tecla-esquerda '(#\a left))

;; LISTA PUNTOS ;;

(struct punto-c (x y) #:transparent)

;; LISTA DE PUNTOS QUE SE REPRESENTAN POR DEFECTO

(define (lista-suelo n e)
  (cond
    ;((< n (+ n-c-ancho n-c-ancho-aumentado))
    ((< (+ n 10) (+ n-c-ancho n-c-ancho-aumentado))
     (lista-suelo (+ n 1) (cons (punto-c n (- n-c-alto 5)) e)))
    (else
     e)))

(define lista-puntos 
  (append (lista-suelo 10 empty)
          (list
           (punto-c 19 5)
           (punto-c 19 6) (punto-c 19 7) (punto-c 19 8)
           (punto-c 15 9) (punto-c 16 8) (punto-c 17 8)
           (punto-c 49 9) (punto-c 49 8) (punto-c 49 7) (punto-c 49 6) (punto-c 46 9) (punto-c 46 8) (punto-c 46 7) 
           (punto-c 46 6) (punto-c 44 5) (punto-c 43 5) (punto-c 49 16) (punto-c 49 15) (punto-c 49 14) (punto-c 49 13)
           (punto-c 49 12) (punto-c 49 11) (punto-c 49 10) (punto-c 49 17) (punto-c 48 17) (punto-c 47 17) (punto-c 46 17)
           (punto-c 46 16) (punto-c 46 15) (punto-c 46 14) (punto-c 46 13) (punto-c 46 12) (punto-c 46 11) (punto-c 46 10)
           (punto-c 42 8) (punto-c 41 8) (punto-c 38 8) (punto-c 37 8) (punto-c 35 17) (punto-c 35 16) (punto-c 35 15)
           (punto-c 35 14) (punto-c 35 13) (punto-c 35 12) (punto-c 35 11) (punto-c 35 10) (punto-c 34 17) (punto-c 34 16)
           (punto-c 34 15) (punto-c 34 14) (punto-c 34 13) (punto-c 34 12) (punto-c 34 11) (punto-c 34 10) (punto-c 33 17)
           (punto-c 33 16) (punto-c 33 15) (punto-c 33 14) (punto-c 33 13) (punto-c 33 12) (punto-c 32 14) (punto-c 32 17)
           (punto-c 32 16) (punto-c 32 15) (punto-c 31 17) (punto-c 31 16) (punto-c 30 17) (punto-c 30 16) (punto-c 26 16)
           (punto-c 26 17) (punto-c 25 17) (punto-c 25 16) (punto-c 24 15) (punto-c 23 14) (punto-c 22 13) (punto-c 21 12)
           (punto-c 24 17) (punto-c 24 16) (punto-c 23 17) (punto-c 23 16) (punto-c 23 15) (punto-c 22 17) (punto-c 22 16)
           (punto-c 22 15) (punto-c 22 14) (punto-c 21 16) (punto-c 21 17) (punto-c 21 15) (punto-c 21 14) (punto-c 21 13)
           (punto-c 20 12) (punto-c 20 13) (punto-c 20 14) (punto-c 20 15) (punto-c 20 16) (punto-c 20 17) (punto-c 19 17)
           (punto-c 19 16) (punto-c 19 15) (punto-c 19 14) (punto-c 19 13) (punto-c 19 12) (punto-c 18 12) (punto-c 17 12) 
           (punto-c 16 12) (punto-c 15 17) (punto-c 14 17) (punto-c 15 16) (punto-c 14 16) (punto-c 15 15) (punto-c 14 15) 
           (punto-c 15 14) (punto-c 14 14) (punto-c 15 13) (punto-c 14 13) (punto-c 15 12) (punto-c 14 12) (punto-c 13 17)
           (punto-c 13 15) (punto-c 13 16) (punto-c 13 14) (punto-c 13 13) (punto-c 13 12) (punto-c 12 13) (punto-c 12 17) 
           (punto-c 12 16) (punto-c 12 15) (punto-c 12 14) (punto-c 11 14) (punto-c 11 17) (punto-c 11 16) (punto-c 11 15) 
           (punto-c 10 15) (punto-c 10 17) (punto-c 10 16) (punto-c 9 16) (punto-c 9 17) (punto-c 8 17) (punto-c 7 17) 
           (punto-c 6 17) (punto-c 5 17) (punto-c 4 16) (punto-c 4 17) (punto-c 3 17) (punto-c 3 16) (punto-c 3 15) 
           (punto-c 2 14) (punto-c 2 15) (punto-c 2 16) (punto-c 2 17) (punto-c 1 17) (punto-c 0 17) (punto-c 1 16) 
           (punto-c 0 16) (punto-c 1 15) (punto-c 0 15) (punto-c 1 14) (punto-c 0 14) (punto-c 1 13) (punto-c 0 13)
           )))

(define lista-puntos-creados empty)

(define lista-puntos-ganar (list (punto-c 48 16) (punto-c 47 16)))

;; FUNCIÓNS VARIAS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Función para cando se modifica o tamaño ou zoom da ventana

(define (redefinir-tamaños x)
  (define punto-pj-x (punto-x (pj-punto (xogo-pj x))))
  (set! ancho-fondo (* l-cadrado n-c-ancho))
  (set! ancho-fondo-real (* l-cadrado (+ n-c-ancho n-c-ancho-aumentado)))
  (set! alto-fondo (* l-cadrado n-c-alto))
  (set! fondo-0 (freeze (empty-scene ancho-fondo-real alto-fondo transparente)))
  (set! fondo-1 (freeze (cuadricula-filas 0 (cuadricula-columnas 0 fondo-0))))
  (set! ancho-pj-i (image-width (rectangle l-cadrado (* l-cadrado 2) "solid" "black")))
  (set! alto-pj-i (image-height (rectangle l-cadrado (* l-cadrado 2) "solid" "black")))
  (set! empty-s (freeze (escalar (* ancho-fondo 1.2) alto-fondo (bitmap "fondo-c3.jpg"))))
  (set! cadrado-s (rectangle (+ l-cadrado 1) (+ l-cadrado 1) "solid" (make-color 50 50 50)))
  (set! cadrado-s-2 (freeze 
                     (overlay
                      (circle (/ l-cadrado 4) "solid" "darkblue")
                      (rectangle (+ l-cadrado 1) (+ l-cadrado 1) "solid" "lightblue"))))
  (set! cadrado-fin (rectangle (+ l-cadrado 1) (+ l-cadrado 1) "solid" "lightgreen"))
  (set! linea-alto (freeze (line 0 alto-fondo "lightgray")))
  (set! linea-ancho (freeze (line ancho-fondo-real 0 "lightgray")))
  (set! cadro-r (freeze
                 (overlay
                  (rectangle (* l-cadrado 0.75) (* l-cadrado 0.75) "outline" "red")
                  (rectangle l-cadrado l-cadrado "solid" transparente))))
  (set! movemento-lateral (/ l-cadrado 4))
  (set! movemento-salto (/ l-cadrado 4))
  (set! rectangulo-pj-normal
        (overlay
         (rectangle l-cadrado (* l-cadrado 2) "outline" "darkred")
         (rectangle l-cadrado (* l-cadrado 2) "solid" "black")))
  (set! rectangulo-pj-agachado
        (overlay
         (rectangle l-cadrado l-cadrado "outline" "darkred")
         (rectangle l-cadrado l-cadrado "solid" "black")))
  (set! fondo-estatico
        (freeze  (por-cadros #t cadrado-s-2 lista-puntos-creados 
                             (pantalla-con-cadros cadrado-s lista-puntos
                                                  (pantalla-con-cadros cadrado-fin lista-puntos-ganar fondo-0)))))
  (set! fondo-estatico-cuadricula
        (freeze (por-cuadricula #t fondo-estatico)))
  (set! cambiando-tamaño #t)
  )

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

;;voltear lista
;lista + lista(empty) -> lista

(define (voltear lista emp)
  (cond
    ((empty? lista)
     emp)
    (else
     (voltear (cdr lista) (cons (car lista) emp)))))

;;FUNCIÓNS PARA ACELERAR COLISIÓNS:
;;COLLÉN DIRECTAMENTE OS PUNTOS ADECUADOS:

;;ORDENA PUNTO A PUNTO
; (punto 0 0) -> (punto 0 1) -> (punto 0 2)

(define (ordenar-punto-rev px py lista-p lista)
  (cond
    ((>= py n-c-alto)
     (list->vector (reverse lista)))
    ((< px (- n-c-ancho-real 1))
     (ordenar-punto-rev (+ px 1) py lista-p (cons
                                             (filter (lambda (x)
                                                       (and (= px (punto-c-x x))
                                                            (= py (punto-c-y x))))
                                                     lista-p)
                                             lista)))
    (else
     (ordenar-punto-rev 0 (+ py 1) lista-p (cons
                                            (filter (lambda (x)
                                                      (and (= px (punto-c-x x))
                                                           (= py (punto-c-y x))))
                                                    lista-p)
                                            lista)))))

(define VECTOR-PUNTOS-ORDENADOS (ordenar-punto-rev 0 0 lista-puntos empty))

(define (s-vector-ref vector n)
  (cond
    ((or
      (< n 0)
      (>= n (vector-length vector)))
     empty)
    (else
     (vector-ref vector (inexact->exact (round n))))))

(define (calc-apart-vect pc-y pc-x)
  (cond
    ((or
      (< pc-x 0)
      (>= pc-x n-c-ancho-real))
     -1)
    (else
     (+ (* pc-y n-c-ancho-real) pc-x))))


(define (coller-puntos-en-caida x p-x p-y vect vel-caida sumando)
  (cond
    ((>= vel-caida l-cadrado)
     (append
      (append 
       (s-vector-ref vect (calc-apart-vect (+ p-y sumando) p-x))
       (if (not (<= p-x 0)) 
           (s-vector-ref vect (calc-apart-vect (+ p-y sumando) (- p-x 1))) empty)
       (if (< p-x n-c-ancho-real) 
           (s-vector-ref vect (calc-apart-vect (+ p-y sumando) (+ p-x 1))) empty))
      (coller-puntos-en-caida x p-x p-y vect (- vel-caida l-cadrado) (+ sumando 1))))
    (else
     empty)))


(define (coller-puntos-circundantes x pc-x pc-y vector)
  (append
   (s-vector-ref vector (calc-apart-vect pc-y  pc-x)) ;;puntos donde esta o pj
   (s-vector-ref vector (calc-apart-vect (- pc-y 1)  pc-x))
   (s-vector-ref vector (calc-apart-vect (- pc-y 2)  pc-x)) ;; punto arriba
   (s-vector-ref vector (calc-apart-vect (+ pc-y 1)  pc-x)) ;; punto abaixo
   (s-vector-ref vector (calc-apart-vect (+ pc-y 2)  pc-x)) ;;punto abaixo x2
   (if (not (<= pc-x 0))
       (append
        (s-vector-ref vector (calc-apart-vect pc-y  (- pc-x 1)))       ;;puntos anteriores en x
        (s-vector-ref vector (calc-apart-vect (- pc-y 1)  (- pc-x 1)))
        (s-vector-ref vector (calc-apart-vect (- pc-y 2)  (- pc-x 1)))
        (s-vector-ref vector (calc-apart-vect (+ pc-y 1)  (- pc-x 1)))
        (s-vector-ref vector (calc-apart-vect (+ pc-y 2)  (- pc-x 1))))
       empty)
   (if (not (>= pc-x n-c-ancho-real))
       (append
        (s-vector-ref vector (calc-apart-vect pc-y  (+ pc-x 1))) ;;puntos posteriores en x
        (s-vector-ref vector (calc-apart-vect (- pc-y 1)  (+ pc-x 1)))
        (s-vector-ref vector (calc-apart-vect (- pc-y 2)  (+ pc-x 1)))
        (s-vector-ref vector (calc-apart-vect (+ pc-y 1)  (+ pc-x 1)))
        (s-vector-ref vector (calc-apart-vect (+ pc-y 2)  (+ pc-x 1))))
       empty)
   (if (equal? (pj-estado-s (xogo-pj x)) "down")
       (append 
        (s-vector-ref vector (calc-apart-vect (+ pc-y 3) pc-x))
        (if (not (<= pc-x 0)) 
            (s-vector-ref vector (calc-apart-vect (+ pc-y 3) (- pc-x 1))) empty)
        (if (< pc-x n-c-ancho-real) 
            (s-vector-ref vector (calc-apart-vect (+ pc-y 3) (+ pc-x 1))) empty))
       empty)
   (coller-puntos-en-caida x pc-x pc-y vector (xogo-vel-c x) 3)
   (if (equal? (pj-estado-s (xogo-pj x)) "up")
       (append
        (s-vector-ref vector (calc-apart-vect (- pc-y 3) pc-x))
        (if (not (<= pc-x 0))
            (s-vector-ref vector (calc-apart-vect (- pc-y 3) (- pc-x 1))) empty)
        (if (< pc-x n-c-ancho-real)
            (s-vector-ref vector (calc-apart-vect (- pc-y 3) (+ pc-x 1))) empty))
       empty)
   ))

;;FUNCIÓNS PARA COLLER PASAR OS PUNTOS ADECUADOS DEPENDENDO DO PUNTO ACTUAL DO PJ.

;;coller puntos adecuados en x

(define (coller-puntos-adecuados-x P-O punto-c-x-pj)
  (cond
    ((< punto-c-x-pj -1)
     empty)
    ((< punto-c-x-pj 0)
     (vector-ref P-O (inexact->exact (+ punto-c-x-pj 1))))
    ((= punto-c-x-pj 0)
     (append (vector-ref P-O (inexact->exact punto-c-x-pj)) (vector-ref P-O (inexact->exact (+ punto-c-x-pj 1)))))
    ((= punto-c-x-pj (- (+ n-c-ancho n-c-ancho-aumentado) 1))
     (append (vector-ref P-O punto-c-x-pj) (vector-ref P-O (inexact->exact (- punto-c-x-pj 1)))))
    ((= punto-c-x-pj (+ n-c-ancho n-c-ancho-aumentado))
     (vector-ref P-O (inexact->exact (- punto-c-x-pj 1))))
    ((> punto-c-x-pj (+ n-c-ancho n-c-ancho-aumentado))
     empty)
    (else
     (append (vector-ref P-O (inexact->exact punto-c-x-pj))
             (vector-ref P-O (inexact->exact (- punto-c-x-pj 1))) (vector-ref P-O (inexact->exact (+ punto-c-x-pj 1)))))))

;;coller puntos adecuados en y

(define (coller-puntos-adecuados-y P-O punto-c-y-pj)
  (cond
    ((< punto-c-y-pj -1)
     empty)
    ((= punto-c-y-pj -1)
     (vector-ref P-O (inexact->exact (+ punto-c-y-pj 1))))
    ((= punto-c-y-pj 0)
     (append (vector-ref P-O (inexact->exact punto-c-y-pj)) (vector-ref P-O (inexact->exact (+ punto-c-y-pj 1)))))
    ((= punto-c-y-pj  (- n-c-alto 1))
     (append (vector-ref P-O (inexact->exact punto-c-y-pj)) (vector-ref P-O (inexact->exact (- punto-c-y-pj 1)))))
    ((= punto-c-y-pj  n-c-alto)
     (vector-ref P-O (inexact->exact (- punto-c-y-pj 1))))
    ((> punto-c-y-pj n-c-alto)
     empty)
    (else
     (append (vector-ref P-O (inexact->exact punto-c-y-pj)) 
             (vector-ref P-O (inexact->exact (- punto-c-y-pj 1))) (vector-ref P-O (inexact->exact (+ punto-c-y-pj 1)))))))

;; -----------------

;elemento + lista -> booleano

; (define (elemento-en-lista? e lista)
;   (not (false? (member e lista))))
(define (elemento-en-lista? e lista)
  (cond
    ((empty? lista) #f)
    ((equal? e (car lista)) #t)
    (else (elemento-en-lista? e (cdr lista)))))

;; volve un número positivo

(define (positivo n)
  (cond
    ((< n 0)
     (- n))
    (else n)))

;elemento + lista -> lista
;pon un elemento nunha lista

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

;; Creación da cuadricula

;;número(0) + imagen -> imagen

(define linea-alto (freeze (line 0 alto-fondo "lightgray")))
(define linea-ancho (freeze (line ancho-fondo-real 0 "lightgray")))

(define (cuadricula-filas py fondo)
  (place-image
   linea-ancho
   (/ ancho-fondo-real 2) py
   (cond
     ((>= py alto-fondo)
      fondo)
     (else
      (cuadricula-filas (+ py l-cadrado) fondo)))))

(define (cuadricula-columnas px fondo)
  (place-image
   linea-alto
   px (/ alto-fondo 2)
   (cond
     ((>= px ancho-fondo-real)
      fondo)
     (else
      (cuadricula-columnas (+ px l-cadrado) fondo)))))

(define (por-cuadricula bool fondo)
  (cond 
    (bool
     (cuadricula-filas 0 
                       (cuadricula-columnas 0 fondo)))
    (else fondo)))

; fondo con cuadricula

(define fondo-1 (cuadricula-filas 0 (cuadricula-columnas 0 fondo-0)))

;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;PUNTOS ORDENADOS ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;---- X ----

;(define PUNTOS-ORDENADOS-X (ordenar-puntos-x 0 lista-puntos empty))

;---- Y ----

;(define PUNTOS-ORDENADOS-Y (ordenar-puntos-y 0 lista-puntos empty))

;; PERSONAJE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; imagenes

(define rectangulo-pj-normal
  (freeze
   (overlay
   (rectangle (- l-cadrado 1) (* l-cadrado 2) "outline" "darkred")
   (rectangle l-cadrado (* l-cadrado 2) "solid" "black"))))

(define rectangulo-pj-agachado
  (freeze
  (overlay
   (rectangle (- l-cadrado 1) l-cadrado "outline" "darkred")
   (freeze (rectangle l-cadrado l-cadrado "solid" "black")))))

(define (img-pj x)
  (define estado-m (pj-estado-m (xogo-pj x)))
  (define estado-s (pj-estado-s (xogo-pj x)))
  (cond
    ((and
      (member #\s estado-m)
      (equal? estado-s "floor"))
     rectangulo-pj-agachado)
    (else
     rectangulo-pj-normal)))

; ancho e alto

(define ancho-pj-i (image-width (rectangle l-cadrado (* l-cadrado 2) "solid" "black")))

(define alto-pj-i (image-height (rectangle l-cadrado (* l-cadrado 2) "solid" "black")))

(define (ancho-pj x) (image-width (img-pj x)))

(define (alto-pj x) (image-height (img-pj x)))

;; CADROS (PLATAFORMAS) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;punto-c -> punto

(define (punto-cadro->punto p-c)
  (punto
   (+ (* (punto-c-x p-c) l-cadrado) (/ l-cadrado 2))
   (+ (* (punto-c-y p-c) l-cadrado) (/ l-cadrado 2))))

;;punto -> punto-c

(define (punto->punto-cadro p)
  (punto-c
   (floor (/ (punto-x p) l-cadrado))
   (floor (/ (punto-y p) l-cadrado))))

;numero + punto -> punto

(define (sumar-x-punto n punto-pj)
  (punto
   (+ (punto-x punto-pj) n)
   (punto-y punto-pj)))

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

;; distancia bloque de abaixo
;punto-pj + lista-bloques + lista (empty) -> número

(define (distancia-bloque-abaixo punto-pj lista lista-e)
  (cond
    ((empty? lista)
     (cond
       ((empty? lista-e)
        #f)
       (else
        (- (- (punto-y (punto-cadro->punto (argmin punto-c-y lista-e))) (/ l-cadrado 2))
           (+ (punto-y punto-pj) l-cadrado)))))
    (else
     (distancia-bloque-abaixo 
      punto-pj empty
      (filter (lambda (n) 
                (and
                 (or
                  (= (punto-c-x (punto->punto-cadro punto-pj)) (punto-c-x n))
                  (= (punto-c-x (punto->punto-cadro (sumar-x-punto (- 1 (/ l-cadrado 2)) punto-pj))) (punto-c-x n))
                  (= (punto-c-x (punto->punto-cadro (sumar-x-punto (- (/ l-cadrado 2) 1) punto-pj))) (punto-c-x n)))
                 (>= (- (- (punto-y (punto-cadro->punto n)) (/ l-cadrado 2)) 
                        (+ (punto-y punto-pj) l-cadrado)) 
                     (- (/ l-cadrado 2)))
                 ))
              lista)))))

;; distancia bloque de arriba
;punto-pj + lista-bloques + lista (empty) -> número

(define (distancia-bloque-enriba x punto-pj lista lista-e)
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
               (- (punto-y punto-pj) l-cadrado))
           (+ (punto-y (punto-cadro->punto (argmax punto-c-y lista-e))) (/ l-cadrado 2))))))
    (else
     (distancia-bloque-enriba
      x punto-pj empty
      (filter (lambda (n)
                (and
                 (or
                  (= (punto-c-x (punto->punto-cadro punto-pj)) (punto-c-x n))
                  (= (punto-c-x (punto->punto-cadro (sumar-x-punto (- 1 (/ l-cadrado 2)) punto-pj))) (punto-c-x n))
                  (= (punto-c-x (punto->punto-cadro (sumar-x-punto (- (/ l-cadrado 2) 1) punto-pj))) (punto-c-x n)))
                 (>= (- (if (and
                             (member #\s estado-m)
                             (equal? estado-s "floor"))
                            (punto-y punto-pj)
                            (- (punto-y punto-pj) l-cadrado))
                        (- (punto-y (punto-cadro->punto n)) (/ l-cadrado 2))) 
                     (- (/ l-cadrado 2)))
                 ))
              lista)))))


;; distancia bloque da esquerda
;punto-pj + lista-bloques + lista (empty) -> número

(define (distancia-bloque-esquerda x punto-pj lista lista-e)
  (define estado-m (pj-estado-m (xogo-pj x)))
  (define estado-s (pj-estado-s (xogo-pj x)))
  (cond
    ((empty? lista)
     (cond
       ((empty? lista-e)
        #f)
       (else
        (- (- (punto-x punto-pj) (/ l-cadrado 2))
           (+ (punto-x (punto-cadro->punto (argmax punto-c-x lista-e))) (/ l-cadrado 2))))))
    (else
     (distancia-bloque-esquerda 
      x punto-pj empty
      (filter (lambda (n)
                (and
                 (or
                  (= (punto-c-y (punto->punto-cadro punto-pj)) (punto-c-y n))
                  (if (and
                       (member #\s estado-m)
                       (equal? estado-s "floor"))
                      #f
                      (= (punto-c-y (punto->punto-cadro (sumar-y-punto (- 1  l-cadrado) punto-pj))) (punto-c-y n)))
                  (= (punto-c-y (punto->punto-cadro (sumar-y-punto (- l-cadrado 1) punto-pj))) (punto-c-y n)))
                 (>=  (- (- (punto-x punto-pj) (/ l-cadrado 2))
                         (- (punto-x (punto-cadro->punto n)) (/ l-cadrado 2))) 
                      0)
                 ))
              lista
              )))))


;; distancia bloque da dereita
;punto-pj + lista-bloques + lista (empty) -> número

(define (distancia-bloque-dereita x punto-pj lista lista-e)
  (define estado-m (pj-estado-m (xogo-pj x)))
  (define estado-s (pj-estado-s (xogo-pj x)))
  (cond
    ((empty? lista)
     (cond
       ((empty? lista-e)
        #f)
       (else
        (- (- (punto-x (punto-cadro->punto (argmin punto-c-x lista-e))) (/ l-cadrado 2))
           (+ (punto-x punto-pj) (/ l-cadrado 2))))))
    (else
     (distancia-bloque-dereita
      x punto-pj empty 
      (filter (lambda (n)
                (and
                 (or
                  (= (punto-c-y (punto->punto-cadro punto-pj)) (punto-c-y n))
                  (if (and
                       (member #\s estado-m)
                       (equal? estado-s "floor"))
                      #f
                      (= (punto-c-y (punto->punto-cadro (sumar-y-punto (- 1  l-cadrado) punto-pj))) (punto-c-y n)))
                  (= (punto-c-y (punto->punto-cadro (sumar-y-punto (- l-cadrado 1) punto-pj))) (punto-c-y n)))
                 (<=  (- (- (punto-x punto-pj) (/ l-cadrado 2))
                         (- (punto-x (punto-cadro->punto n)) (/ l-cadrado 2))) 
                      0)
                 ))
              lista
              )))))

;imagenes cadrados

(define cadrado-s (rectangle (+ l-cadrado 1) (+ l-cadrado 1) "solid" (make-color 50 50 50)))

(define cadrado-s-2 (freeze 
                     (overlay
                      (circle (/ l-cadrado 4) "solid" "darkblue")
                      (rectangle (+ l-cadrado 1) (+ l-cadrado 1) "solid" "lightblue"))))

(define cadrado-fin (rectangle (+ l-cadrado 1) (+ l-cadrado 1) "solid" "lightgreen"))

(define cadro-r (freeze
                 (overlay
                  (rectangle (* l-cadrado 0.75) (* l-cadrado 0.75) "outline" "red")
                  (rectangle l-cadrado l-cadrado "solid" transparente))))

;;; STRUCT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;estructuras - objectos

(struct punto (x y) #:transparent)

(struct pj (estado-m estado-s punto punto-pantalla) #:transparent)

(struct xogo (pj temp vel-c cuadricula cadros-restantes estado carga conm) #:transparent)

;; 

;; to-draw ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;funcións necesarias:

;lista + imagen -> imagen

(define (pantalla-con-cadros cadrado lista fondo)
  (place-image cadrado
               (punto-x (punto-cadro->punto (car lista)))
               (punto-y (punto-cadro->punto (car lista)))
               (cond
                 ((empty? (cdr lista))
                  fondo)
                 (else
                  (pantalla-con-cadros cadrado (cdr lista) fondo)))))

;; pon os cadros que van no estado do xogo

(define (por-cadros bool tipo-cadro lista fondo)
  (cond 
    ((not bool)
     fondo)
    (else
     (cond
       ((empty? lista)
        fondo)
       (else
        (place-image tipo-cadro
                     (punto-x (punto-cadro->punto (car lista)))
                     (punto-y (punto-cadro->punto (car lista)))
                     (cond
                       ((empty? (cdr lista))
                        fondo)
                       (else
                        (por-cadros bool tipo-cadro (cdr lista) fondo)))))))))

; pon os cadros das colisións (pulsar a tecla "C" mentras se xoga)

(define (por-cadros-e bool x tipo-cadro  lista fondo)
  (cond 
    ((not bool)
     fondo)
    (else
     (cond
       ((empty? lista)
        fondo)
       (else
        (place-image tipo-cadro
                     (cond
                       ((< (punto-x (pj-punto (xogo-pj x))) (/ ancho-fondo 2))
                        (punto-x (punto-cadro->punto (car lista))))
                       ((> (punto-x (pj-punto (xogo-pj x))) (- ancho-fondo-real (/ ancho-fondo 2)))
                        (- (punto-x (punto-cadro->punto (car lista))) (- ancho-fondo-real ancho-fondo)))
                       (else
                        (- (punto-x (punto-cadro->punto (car lista))) (- (punto-x (pj-punto (xogo-pj x)))
                                                                         (/ ancho-fondo 2)))))
                     (punto-y (punto-cadro->punto (car lista)))
                     (cond
                       ((empty? (cdr lista))
                        fondo)
                       (else
                        (por-cadros-e bool x tipo-cadro (cdr lista) fondo)))))))))

; fondo de cadrados do xogo

(define fondo-estatico
  (freeze (por-cadros #t cadrado-s-2 lista-puntos-creados 
                      (pantalla-con-cadros cadrado-s lista-puntos
                                           (pantalla-con-cadros cadrado-fin lista-puntos-ganar fondo-0)))))

; fondo de cadrados cuadriculado

(define fondo-estatico-cuadricula
  (freeze (por-cuadricula #t fondo-estatico)))

; botón da pantalla inicial no que pon: "XOGAR"

(define (boton-xogar x)
  (overlay (text "XOGAR" (floor (* l-cadrado (/ n-c-ancho 35))) 
                 (if (equal? (xogo-estado x) "xogo->pantallas") "darkgray" "black"))
           (overlay
            (rectangle (* (* l-cadrado 5) (/ n-c-ancho 35)) 
                       (* (* l-cadrado 2) (/ n-c-ancho 35)) 
                       "outline" "black")
            (rectangle (* (* l-cadrado 5) (/ n-c-ancho 35))
                       (* (* l-cadrado 2) (/ n-c-ancho 35)) 
                       "solid" (if (equal? (xogo-estado x) "xogo->pantallas")
                                   "lightblue" "lightgray")))))

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

; barra de "carga" da pantalla inicial. (non se usa a carga)

(define (barra-carga x)
  (rectangle (* ancho-fondo (* (xogo-carga x) 0.01)) (* l-cadrado 4) "solid" "lightblue"))

; fondo da pantalla inicia

(define (fondo-menu)
  (rectangle ancho-fondo (* l-cadrado 4) "solid" (make-color 230 230 230)))

; pantalla inicial

(define (pantalla-inicio x)
  (place-image (text "MUNDO DE CADRADOS" (floor (* (* l-cadrado 1.5) (/ n-c-ancho 35))) "black")
               (/ ancho-fondo 2) (* l-cadrado 2)
               (place-image 
                (line ancho-fondo 0 "black")
                (/ ancho-fondo 2) (* l-cadrado 4)
                (place-image 
                 (line ancho-fondo 0 "black")
                 (/ ancho-fondo 2) (- alto-fondo (* l-cadrado 4))
                 (cond
                   ((equal? (xogo-estado x) "cargando")
                    (overlay 
                     (rectangle (- ancho-fondo 1) (- alto-fondo 1) "outline" "black")
                     (place-image
                      (barra-carga x)
                      (/ ancho-fondo 2) (/ (* l-cadrado 4) 2)
                      (empty-scene ancho-fondo alto-fondo))))
                   (else
                    (place-image (boton-xogar x)
                                 (/ ancho-fondo 2) 
                                 (- (/ alto-fondo 2.5) (* (/ n-c-ancho 35) (* l-cadrado 2)))
                                 (overlay (rectangle (- ancho-fondo 1) (- alto-fondo 1) "outline" "black")
                                          (place-image
                                           (barra-carga x)
                                           (/ ancho-fondo 2) (/ (* l-cadrado 4) 2)
                                           (place-image 
                                            (fondo-menu)
                                            (/ ancho-fondo 2) 
                                            (- alto-fondo (* l-cadrado 2))
                                            (empty-scene ancho-fondo alto-fondo)))))))))))

; pantalla do xogo (incluindo a inicial)

(define (pantalla-xogo x)
  (cond
    ((member (xogo-estado x) (list "inicio" "xogo->pantallas" "cargando"))
     (pantalla-inicio x)) ; pantalla-inicial
    (else
     (define estado-m (pj-estado-m (xogo-pj x)))
     (define estado-s (pj-estado-s (xogo-pj x)))
     (define punto-pj-x (punto-x (pj-punto (xogo-pj x))))
     (overlay (text (cond
                      ((equal? (xogo-estado x) "game over")
                       "GAME OVER")
                      ((equal? (xogo-estado x) "completado")
                       "COMPLETADO")
                      (else 
                       ""))
                    (floor (/ ancho-fondo 15)) (cond
                                              ((equal? (xogo-estado x) "game over")
                                               "red")
                                              ((equal? (xogo-estado x) "completado")
                                               "green")
                                              (else "black")))
              (por-cadros-e ; pantalla do xogo en sí
               (if (xogo-cuadricula x) #t #f)
               x cadro-r lista-pasable
                            (place-image (img-pj x)
                                         (punto-x (pj-punto-pantalla (xogo-pj x)))
                                         (cond
                                           ((and
                                             (member #\s estado-m)
                                             (equal? estado-s "floor"))
                                            (+ (punto-y (pj-punto (xogo-pj x))) (/ l-cadrado 2)))
                                           (else
                                            (punto-y (pj-punto (xogo-pj x)))))      
                                         (place-image
                                          (if (xogo-cuadricula x) fondo-estatico-cuadricula fondo-estatico)
                                          (cond 
                                            ((<= punto-pj-x (/ ancho-fondo 2))
                                             (+ (/ ancho-fondo 2) (/ (- ancho-fondo-real ancho-fondo) 2)))
                                            ((>= punto-pj-x (- ancho-fondo-real (/ ancho-fondo 2)))
                                             (- (/ ancho-fondo 2) (/ (- ancho-fondo-real ancho-fondo) 2)))
                                            (else
                                             (- (+ (/ ancho-fondo 2) (/ (- ancho-fondo-real ancho-fondo) 2)) 
                                                (- (punto-x (pj-punto (xogo-pj x)))
                                                   (punto-x (pj-punto-pantalla (xogo-pj x)))))
                                             ))
                                          (/ alto-fondo 2) 
                                          (place-image empty-s
                                                       (cond
                                                         ((> punto-pj-x (- ancho-fondo-real (/ ancho-fondo 2)))
                                                          (- (/ ancho-fondo 2)
                                                             (/ (- (- ancho-fondo-real (/ ancho-fondo 2)) (/ ancho-fondo 2)) 30)))
                                                         ((> punto-pj-x (/ ancho-fondo 2))
                                                          (- (/ ancho-fondo 2)
                                                             (/ (- punto-pj-x (/ ancho-fondo 2)) 30)))
                                                         (else
                                                          (/ ancho-fondo 2)))
                                                       (/ alto-fondo 2)
                                                       (empty-scene ancho-fondo alto-fondo)))))))))

; funcíon para escalar unha imagen

(define (escalar x y imagen)
  (scale/xy (/ x (image-width imagen)) (/ y (image-height imagen)) imagen))

;(define empty-s (freeze (empty-scene ancho-fondo alto-fondo (make-color 230 230 230))))

(define empty-s (freeze (escalar (* ancho-fondo 1.2) alto-fondo (bitmap "fondo-c3.jpg"))))

;; on-key ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; PRESIONAR 

(define (tecla x t)
  (define distancia-bloque-enriba-c (distancia-bloque-enriba x (pj-punto (xogo-pj x)) lista-pasable empty))
  (cond
    ((member (xogo-estado x) (list "game over" "completado"))
     (cond
       ((equal? t #\return)
        (xogo 
         (pj 
          empty "floor"
          punto-inicial
          punto-inicial)
         0 0 #f 500 "pantallas" 100 0))
       (else
        x)))
    ((member (xogo-estado x) (list "inicio" "xogo->pantallas"))
     x)
    (else
     (define punto-pj-x (punto-x (pj-punto (xogo-pj x))))
     (define punto-pj-y (punto-y (pj-punto (xogo-pj x))))
     (define estado-m (pj-estado-m (xogo-pj x)))
     (define estado-s (pj-estado-s (xogo-pj x)))
     ;(when (equal? t #\return) (display (puntos->texto lista-puntos-creados)))
     (when (member t tecla-agacharse) (set! agacharse #t))
     (xogo
      (pj
       (cond ;;movemento-m
         ((member t (append tecla-esquerda tecla-dereita))
          (por-elemento-lista x (cond
                                  ((member t tecla-dereita) #\d)
                                  ((member t tecla-esquerda) #\a))
                              estado-m))
         (else
          estado-m))
       (cond ;;movemento-s
         ((and
           (not (elemento-en-lista? #\s estado-m))
           (member t tecla-salto)
           (if 
            distancia-bloque-enriba-c
            (>  distancia-bloque-enriba-c
                1)
            #t)
           (equal? estado-s "floor"))
          (play-sound "salto.wav" #t)
          "up")
         (else 
          estado-s))
       (pj-punto (xogo-pj x))   ;; punto-pj)
       (pj-punto-pantalla (xogo-pj x))) ;;punto-pantalla
      (cond ;;temp
        ((and
          (not (elemento-en-lista? #\s estado-m))
          (member t tecla-salto)
          (equal? estado-s "floor"))
         l-cadrado)
        (else
         (xogo-temp x)))
      (xogo-vel-c x)
      (cond    ;; xogo-cuadricula
        ((and
          (equal? t #\c)
          (equal? (xogo-cuadricula x) #f))
         #t)
        ((and
          (equal? t #\c)
          (equal? (xogo-cuadricula x) #t))
         #f)
        (else
         (xogo-cuadricula x)))
      (xogo-cadros-restantes x)
      (xogo-estado x)
      (xogo-carga x)
      (xogo-conm x)))))

;; on-release ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define levantarse #f)
(define agacharse #f)

;; SOLTAR

(define (soltar-tecla x t)
  (cond
    ((member (xogo-estado x) (list "inicio" "xogo->pantallas" "cargando"))
     x)
    (else
     (define estado-m (pj-estado-m (xogo-pj x)))
     (define estado-s (pj-estado-s (xogo-pj x)))
     (when (member t tecla-agacharse) (set! levantarse #t))
     (xogo
      (pj
       (cond ;;estado-m
         ((not (member t tecla-agacharse))
          (remove (cond
                    ((member t tecla-dereita) #\d)
                    ((member t tecla-esquerda) #\a)
                    (else
                     ""))
                  estado-m))
         (else
          estado-m))
       estado-s
       (pj-punto (xogo-pj x))
       (pj-punto-pantalla (xogo-pj x)))
      (cond ;;temp
        ((member t tecla-salto)
         (round (/ (xogo-temp x) 1.5)))
        (else
         (xogo-temp x)))
      (xogo-vel-c x)
      (xogo-cuadricula x)
      (xogo-cadros-restantes x)
      (xogo-estado x)
      (xogo-carga x)
      (xogo-conm x)))))

;; on-tick ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define lista-pasable empty)

(define (tick x)
  (when (not cambiando-tamaño) (set! l-cadrado-a l-cadrado))
  (when (and 
         ancho-frame
         (> (positivo (- ancho-frame ancho-fondo)) l-cadrado))
    (when (not cambiando-tamaño) (set! l-cadrado-a l-cadrado))
    (set! l-cadrado (/ ancho-frame n-c-ancho))
    (redefinir-tamaños x))
  ;(display cambiando-tamaño)
  (cond
    ((equal? (xogo-estado x) "cargando")
     (xogo
      (xogo-pj x)       
      (xogo-temp x)
      (xogo-vel-c x)
      (xogo-cuadricula x)
      (xogo-cadros-restantes x)
      (cond
        ((>= (xogo-carga x) 100)
         "inicio")
        (else
         (xogo-estado x)))
      (cond
        ((< (xogo-carga x) 100)
         (+ (xogo-carga x) 4))
        (else
         (xogo-carga x)))
      (cond
        (cambiando-tamaño
         (set! cambiando-tamaño #f)
         3)
        ((> (xogo-conm x) 0)
         (- (xogo-conm x) 1))
        (else
         (xogo-conm x)))
      ))
    ((member (xogo-estado x) (list "inicio" "xogo->pantallas"))
     (struct-copy xogo x [conm (cond
                                 (cambiando-tamaño
                                  (set! cambiando-tamaño #f)
                                  3)
                                 ((> (xogo-conm x) 0)
                                  (- (xogo-conm x) 1))
                                 (else
                                  (xogo-conm x)))]))
    ((member (xogo-estado x) (list "game over" "completado"))
     x)
    (else
     (define punto-pj-x (punto-x (pj-punto (xogo-pj x))))
     (define punto-pj-y (punto-y (pj-punto (xogo-pj x))))
     (define estado-m (pj-estado-m (xogo-pj x)))
     (define estado-s (pj-estado-s (xogo-pj x)))
     (set! lista-pasable
           (coller-puntos-circundantes x 
                                       (punto-c-x (punto->punto-cadro (pj-punto (xogo-pj x))))
                                       (punto-c-y (punto->punto-cadro (pj-punto (xogo-pj x))))
                                       VECTOR-PUNTOS-ORDENADOS))
     (define distancia-bloque-enriba-c (distancia-bloque-enriba x (pj-punto (xogo-pj x)) lista-pasable empty))
     (define distancia-bloque-abaixo-c (distancia-bloque-abaixo (pj-punto (xogo-pj x))  lista-pasable empty))
     (define distancia-bloque-esquerda-c (distancia-bloque-esquerda x (pj-punto (xogo-pj x)) lista-pasable empty))
     (define distancia-bloque-dereita-c (distancia-bloque-dereita x (pj-punto (xogo-pj x)) lista-pasable empty))
     (xogo
      (pj
       (cond ;; estado-m
         ((equal? levantarse #t)
          (set! levantarse #f)
          (remove #\s estado-m))
         ((equal? agacharse #t)
          (set! agacharse #f)
          (por-elemento-lista x #\s estado-m))
         (else 
          estado-m))
       (cond ;; estado-s
         ((and
           (<= (xogo-temp x) 0)
           distancia-bloque-abaixo-c
           (<= distancia-bloque-abaixo-c 0))
          "floor")
         ((or
           (and
            (<= (xogo-temp x) 0)
            (not (equal? estado-s "floor")))
           (and
            (not distancia-bloque-abaixo-c)
            (<= (xogo-temp x) 0))
           (and
            (<= (xogo-temp x) 0)
            distancia-bloque-abaixo-c
            (>= distancia-bloque-abaixo-c 0))
           (and
            distancia-bloque-enriba-c
            (<= distancia-bloque-enriba-c 0)))
          "down")
         (else
          estado-s))
       (punto
        (cond ;;punto-pj-x
          (cambiando-tamaño
           (* (/ punto-pj-x l-cadrado-a) l-cadrado))
          ((and
            (not (empty? estado-m))
            (equal? (car estado-m) #\a)
            distancia-bloque-esquerda-c
            distancia-bloque-enriba-c         ;;
            (< distancia-bloque-enriba-c 1)   ;;
            distancia-bloque-abaixo-c         ;;
            (< distancia-bloque-abaixo-c 1)   ;;
            (< distancia-bloque-esquerda-c movemento-lateral))
           (- punto-pj-x distancia-bloque-esquerda-c))
          ((and
            (not (empty? estado-m))
            (equal? (car estado-m) #\d)
            distancia-bloque-dereita-c
            distancia-bloque-enriba-c         ;;
            (< distancia-bloque-enriba-c 1)   ;;
            distancia-bloque-abaixo-c         ;;
            (< distancia-bloque-abaixo-c 1)   ;;
            (< distancia-bloque-dereita-c movemento-lateral))
           (+ punto-pj-x distancia-bloque-dereita-c))
          ((and
            (not (empty? estado-m))
            (equal? (car estado-m) #\a)
            (not (and (member #\s estado-m)
                      (equal? estado-s "floor")))
            (if distancia-bloque-esquerda-c
                (> distancia-bloque-esquerda-c 0)
                #t))
           (- punto-pj-x movemento-lateral))
          ((and
            (not (empty? estado-m))
            (equal? (car estado-m) #\d)
            (not (and (member #\s estado-m)
                      (equal? estado-s "floor")))   
            (if distancia-bloque-dereita-c
                (> distancia-bloque-dereita-c 0)
                #t))
           (+ punto-pj-x movemento-lateral))
          ((and
            (if distancia-bloque-abaixo-c
                (< distancia-bloque-abaixo-c 1)
                #f)
            distancia-bloque-esquerda-c
            (< distancia-bloque-esquerda-c 0))
           (+ punto-pj-x (positivo distancia-bloque-esquerda-c)))
          ((and
            (if distancia-bloque-abaixo-c
                (< distancia-bloque-abaixo-c 1)
                #f)
            distancia-bloque-dereita-c
            (< distancia-bloque-dereita-c 0))
           (- punto-pj-x (positivo distancia-bloque-dereita-c)))
          (else
           punto-pj-x))
        (cond ;;punto-pj-y
          (cambiando-tamaño
           (* (/ punto-pj-y l-cadrado-a) l-cadrado))
          ((and
            (not (empty? estado-m))
            (equal? (car estado-m) #\d)
            distancia-bloque-abaixo-c
            (equal? estado-s "down")
            (< distancia-bloque-abaixo-c 0)
            distancia-bloque-dereita-c
            (< distancia-bloque-dereita-c 0))
           punto-pj-y) ;;para quitar meneo
          ((and
            (not (empty? estado-m))
            (equal? (car estado-m) #\a)
            distancia-bloque-abaixo-c
            (equal? estado-s "down")
            (< distancia-bloque-abaixo-c 0)
            distancia-bloque-esquerda-c
            (< distancia-bloque-esquerda-c 0))
           punto-pj-y) ;;para quitar meneo
          ((and
            (or 
             (not distancia-bloque-abaixo-c)
             (> distancia-bloque-abaixo-c (+ (xogo-vel-c x) 1)))
            (equal? estado-s "down"))
           (+ punto-pj-y (xogo-vel-c x)))
          ((and
            (equal? estado-s "down")
            distancia-bloque-abaixo-c
            (<= distancia-bloque-abaixo-c (+ (xogo-vel-c x) 1)))
           (+ punto-pj-y distancia-bloque-abaixo-c))
          ((and
            distancia-bloque-enriba-c
            (< distancia-bloque-enriba-c 0)
            distancia-bloque-abaixo-c
            (<= distancia-bloque-abaixo-c 0))
           punto-pj-y)
          ((and
            distancia-bloque-enriba-c
            (< distancia-bloque-enriba-c 0))
           (+ punto-pj-y (positivo distancia-bloque-enriba-c)))                  
          ((and
            (if distancia-bloque-abaixo-c
                (>= distancia-bloque-abaixo-c 1)
                #t)
            distancia-bloque-enriba-c
            (< distancia-bloque-enriba-c 0))
           (+ punto-pj-y (positivo distancia-bloque-enriba-c)))
          ((and
            (equal? estado-s "up")
            distancia-bloque-enriba-c
            (>= distancia-bloque-enriba-c 0)
            (<= distancia-bloque-enriba-c
                (+ movemento-salto (round (/ (xogo-temp x) 2)))))
           (- punto-pj-y distancia-bloque-enriba-c))
          ((and
            (equal? estado-s "up")
            (> (xogo-temp x) 0))
           (- punto-pj-y (+ movemento-salto (round (/ (xogo-temp x) 2)))))
          (else
           punto-pj-y)))
       (cond ;; punto-pj-pantalla
         ((and
           (not cambiando-tamaño)
           (or
            (and distancia-bloque-enriba-c
                 (< distancia-bloque-enriba-c -1))
            (and distancia-bloque-abaixo-c
                 (< distancia-bloque-abaixo-c -1))
            (and distancia-bloque-esquerda-c
                 (< distancia-bloque-esquerda-c -1))
            (and distancia-bloque-dereita-c
                 (< distancia-bloque-dereita-c -1))))
          (pj-punto-pantalla (xogo-pj x)))
         (else
          (punto
           (cond
             ((<= punto-pj-x (/ ancho-fondo 2))
              (if cambiando-tamaño 
                  (* (/ punto-pj-x l-cadrado-a) l-cadrado)
                  punto-pj-x))
             ((>= punto-pj-x (- ancho-fondo-real (/ ancho-fondo 2)))
              (- (if cambiando-tamaño
                     (* (/ punto-pj-x l-cadrado-a) l-cadrado)
                     punto-pj-x)
                 (- ancho-fondo-real ancho-fondo)))
             (else
              (/ ancho-fondo 2)))
           (cond
             (#t
              punto-pj-y))))
         ))
      (cond ;; temp
        ((and
          distancia-bloque-enriba-c
          (<= distancia-bloque-enriba-c 0))
         0)
        ((> (xogo-temp x) 0)
         (- (xogo-temp x) (/ l-cadrado 8)))
        ((< (xogo-temp x) 0)
         0)
        (else
         (xogo-temp x)))
      (cond ;; vel-c
        ((equal? estado-s "down")
         (+ (xogo-vel-c x) (/ l-cadrado 10)))
        ((and
          (equal? estado-s "up")
          distancia-bloque-enriba-c
          (<= distancia-bloque-enriba-c 0))
         (/ l-cadrado 6))
        (else
         0))
      (xogo-cuadricula x)
      (xogo-cadros-restantes x)
      (cond ;;xogo-estado
        ((fora? x)
         "game over")
        ((ganar? x)
         "completado")
        (else
         (xogo-estado x)))
      (xogo-carga x)
      (cond
        ((equal? cambiando-tamaño #t)
         (set! cambiando-tamaño #f)
         3)
        ((> (xogo-conm x) 0)
         (- (xogo-conm x) 1))
        (else
         (xogo-conm x)))))))

;; stop-when

(define (fora? x)
  (cond
    ((or
      (> (xogo-conm x) 0)
      cambiando-tamaño)
     #f)
    (else
     (define punto-pj-y (punto-y (pj-punto (xogo-pj x))))
     (> (- punto-pj-y (* l-cadrado 3)) alto-fondo))))

(define (ganar? x)
  (cond
    ((or
      (> (xogo-conm x) 0)
      cambiando-tamaño)
     #f)
    (else
     (or
      (and
       (distancia-bloque-enriba x (pj-punto (xogo-pj x)) lista-puntos-ganar empty)
       (< (distancia-bloque-enriba x (pj-punto (xogo-pj x)) lista-puntos-ganar empty) 0))
      (and
       (distancia-bloque-abaixo (pj-punto (xogo-pj x)) lista-puntos-ganar empty)
       (< (distancia-bloque-abaixo (pj-punto (xogo-pj x)) lista-puntos-ganar empty) 0))
      (and
       (distancia-bloque-esquerda x (pj-punto (xogo-pj x)) lista-puntos-ganar empty)
       (< (distancia-bloque-esquerda x (pj-punto (xogo-pj x)) lista-puntos-ganar empty) 0))
      (and
       (distancia-bloque-dereita x (pj-punto (xogo-pj x)) lista-puntos-ganar empty)
       (< (distancia-bloque-dereita x (pj-punto (xogo-pj x)) lista-puntos-ganar empty) 0))))))

;;on-mouse-press

;cadrado
(define (cadrado n) (* n n))

;distancia 2 puntos-c
(define (dist-puntos-c p1 p2)
  (sqrt (+ (cadrado (- (punto-c-x p2) (punto-c-x p1)))
           (cadrado (- (punto-c-y p2) (punto-c-y p1))))))

;distancia 2 puntos
(define (dist-puntos p1 p2)
  (sqrt (+ (cadrado (- (punto-x p2) (punto-x p1)))
           (cadrado (- (punto-y p2) (punto-y p1))))))

;click en boton xogo?
(define (clic-xogo? px py)
  (and
   (>= px (- (/ ancho-fondo 2) (/ (* (* l-cadrado 5) (/ n-c-ancho 35)) 2)))
   (<= px (+ (/ ancho-fondo 2) (/ (* (* l-cadrado 5) (/ n-c-ancho 35)) 2)))
   (>= py (- (- (/ alto-fondo 2.5) (* (/ n-c-ancho 35) (* l-cadrado 2)))
             (/ (* (* l-cadrado 2) (/ n-c-ancho 35)) 2)))
   (<= py (+ (- (/ alto-fondo 2.5) (* (/ n-c-ancho 35) (* l-cadrado 2)))
             (/ (* (* l-cadrado 2) (/ n-c-ancho 35)) 2)))))

(define (mouse x px py boton)
  (cond
    ((equal? (xogo-estado x) "cargando")
     x)
    ((member (xogo-estado x) (list "inicio" "xogo->pantallas"))
     (cond
       ((clic-xogo? px py)
        (xogo
         (xogo-pj x)
         (xogo-temp x)
         (xogo-vel-c x)
         (xogo-cuadricula x)
         (xogo-cadros-restantes x)
         "xogo->pantallas"
         (xogo-carga x)
         (xogo-conm x)))
       (else
        x)))
    ((member (xogo-estado x) (list "game over" "completado"))
     x)
    (else
     (define punto-pj-x (punto-x (pj-punto (xogo-pj x))))
     (define px-v 
       (cond 
         ((<= punto-pj-x (/ ancho-fondo 2))
          px)
         ((>= punto-pj-x (- ancho-fondo-real (/ ancho-fondo 2)))
          (+ px (- ancho-fondo-real ancho-fondo)))
         (else
          (+ px (- punto-pj-x (/ ancho-fondo 2))))))
     (cond 
       ((and
         (xogo-cuadricula x)
         (> (dist-puntos (punto-cadro->punto (punto->punto-cadro (punto px-v py))) (pj-punto (xogo-pj x))) l-cadrado)
         (not (member (punto->punto-cadro (punto px-v py)) lista-puntos))
         (not (member (punto->punto-cadro (punto px-v py)) lista-puntos-creados))
         (> (xogo-cadros-restantes x) 0))
        (set! lista-puntos-creados (cons (punto->punto-cadro (punto px-v py)) lista-puntos-creados))
        (set! VECTOR-PUNTOS-ORDENADOS (ordenar-punto-rev 0 0 (append lista-puntos lista-puntos-creados) empty))
        (set! fondo-estatico
              (freeze  (por-cadros #t cadrado-s-2 lista-puntos-creados 
                                   (pantalla-con-cadros cadrado-s lista-puntos
                                                        (pantalla-con-cadros cadrado-fin lista-puntos-ganar fondo-0)))))
        (set! fondo-estatico-cuadricula
              (freeze (por-cuadricula #t fondo-estatico)))
        (xogo
         (xogo-pj x)
         (xogo-temp x)
         (xogo-vel-c x)
         (xogo-cuadricula x)
         (- (xogo-cadros-restantes x) 1)
         (xogo-estado x)
         (xogo-carga x)
         (xogo-conm x)))
       (else
        x)))))

(define (soltar-mouse x px py boton)
  (cond
    ((member (xogo-estado x) (list "game over" "completado"))
     (xogo 
      (pj 
       empty "floor"
       punto-inicial
       punto-inicial)
      0 0 #f 100 "pantallas" 100 0))
    (else
     (xogo
      (xogo-pj x)       
      (xogo-temp x)
      (xogo-vel-c x)
      (xogo-cuadricula x)
      (xogo-cadros-restantes x)
      (cond
        ((and
          (member (xogo-estado x) (list "xogo->pantallas"))
          (clic-xogo? px py))
         "pantallas")
        ((member (xogo-estado x) (list "xogo->pantallas"))
         "inicio")
        (else
         (xogo-estado x)))
      (xogo-carga x)
      (xogo-conm x)))))

(define zoom 0)

(define (roda-adiante x)
  (cond
    ((and
      (< (xogo-conm x) 3)
      (> n-c-ancho 45)
      (not (member (xogo-estado x) (list "inicio" "cargando" "xogo->pantallas"))))
     (set! zoom (+ zoom 1))
     (set! l-cadrado-a l-cadrado)
     (set! l-cadrado (+ l-cadrado 1))
     (set! n-c-ancho-aumentado (inexact->exact (round (- (/ ancho-fondo-real l-cadrado) (/ ancho-fondo l-cadrado)))))
     (set! n-c-ancho (round (/ ancho-fondo l-cadrado)))
     (set! n-c-ancho-real (round (+ n-c-ancho n-c-ancho-aumentado)))
     (set! n-c-alto (/ alto-fondo l-cadrado))
     (set! VECTOR-PUNTOS-ORDENADOS (ordenar-punto-rev 0 0 (append lista-puntos lista-puntos-creados) empty))
     (redefinir-tamaños x)
     x)
    (else
     x)))

(define (roda-atras x)
  (cond
    ((and
      (< (xogo-conm x) 3)
      (< n-c-ancho 200)
      (not (member (xogo-estado x) (list "inicio" "cargando" "xogo->pantallas"))))
     (set! zoom (- zoom 1))
     (set! l-cadrado-a l-cadrado)
     (set! l-cadrado (- l-cadrado 1))
     (set! n-c-ancho-aumentado (inexact->exact (round (- (/ ancho-fondo-real l-cadrado) (/ ancho-fondo l-cadrado)))))
     (set! n-c-ancho (round (/ ancho-fondo l-cadrado)))
     (set! n-c-ancho-real (round (+ n-c-ancho n-c-ancho-aumentado)))
     (set! n-c-alto (/ alto-fondo l-cadrado))
     (set! VECTOR-PUNTOS-ORDENADOS (ordenar-punto-rev 0 0 (append lista-puntos lista-puntos-creados) empty))
     (redefinir-tamaños x)
     x)
    (else
     x)))

;;BIG-BONG ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define punto-inicial (punto (- l-cadrado (/ ancho-pj-i 2))
                             1))

(big-bong (xogo 
           (pj 
            empty "floor"
            punto-inicial
            punto-inicial)
           0 0 #f 500 "cargando" 100 0)
          'on-draw pantalla-xogo
          'on-key-press tecla
          'on-key-release soltar-tecla
          'on-mouse-press mouse
          'on-mouse-release soltar-mouse
          ;'on-mouse-move
          'on-wheel-down roda-atras
          'on-wheel-up roda-adiante
          'on-tick tick
          ;'tick-interval (/ 1 60)
          ;'stop-when
          )


;;;;FUNCIÓNS PARA GARDAR UN ARQUIVO

;(require racket/file)

;(file->string path [#:mode mode-flag]) → string?
;  path : path-string?
;  mode-flag : (or/c 'binary 'text) = 'binary

