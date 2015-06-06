#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;; XOGO            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Módulos importados

(require (except-in racket/gui make-color make-pen))
(require 2htdp/image)
(require racket/vector)

(require "structs.rkt")
(require "funcions-varias.rkt")
(require "distancias.rkt")
(require "big-bong.rkt")
(require "func-ficheiros.rkt")

;;;;CONSTANTES XOGO ::::::::::::::::

(define l-cadrado 8)

(define l-cadrado-a l-cadrado)

(define n-c-ancho-ventana 80)

(define n-c-alto-ventana (quotient n-c-ancho-ventana 2))

(define ancho-inicial (* l-cadrado n-c-ancho-ventana))

(define alto-inicial (* l-cadrado n-c-alto-ventana))

(define cambiando-tamaño #f)

(define movemento-lateral (/ l-cadrado 4))

(define movemento-salto (/ l-cadrado 4))

(define ancho-fondo-ventana (* l-cadrado n-c-ancho-ventana))

;(define n-c-ancho-ventana-aumentado 25)

;(define n-c-ancho-ventana-real (+ n-c-ancho-ventana n-c-ancho-ventana-aumentado))

(define n-c-ancho 200)

(define n-c-alto 100)

(define ancho-fondo (* n-c-ancho l-cadrado))

(define alto-fondo (* n-c-alto l-cadrado))

;(define ancho-fondo-ventana-real (* l-cadrado (+ n-c-ancho-ventana n-c-ancho-ventana-aumentado)))

(define alto-fondo-ventana (* l-cadrado n-c-alto-ventana))

(define transparente (color 0 0 0 0))

(define fondo-0 (freeze (empty-scene ancho-fondo alto-fondo transparente)))

;;teclas de movemento:

(define tecla-salto '(#\w #\W up #\space))

(define tecla-agacharse '(#\s #\S down))

(define tecla-dereita '(#\d #\D right))

(define tecla-esquerda '(#\a #\A left))

;; LISTA PUNTOS ;;

;; LISTA DE PUNTOS QUE SE REPRESENTAN POR DEFECTO

(define (lista-suelo n e)
  (cond
    ;((< n (+ n-c-ancho-ventana n-c-ancho-ventana-aumentado))
    ((< (+ n 10) n-c-ancho)
     (lista-suelo (+ n 1) (cons (punto-c n (- n-c-alto 5)) e)))
    (else
     e)))

(define lista-puntos empty)

#;(append (lista-suelo 10 empty)
          (list
           (punto-c 19 5)
           (punto-c 200 100) (punto-c 199 100) (punto-c 199 99) (punto-c 200 0) (punto-c 0 100)
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
           ))

(define lista-puntos-creados empty)

(define lista-puntos-ganar empty)

;(list (punto-c 48 16) (punto-c 47 16)))

;; VECTOR CREADO A PARTIR DOS PUNTOS :::

(define VECTOR-PUNTOS-ORDENADOS (ordenar-punto-rev 0 0 lista-puntos empty n-c-alto n-c-ancho))

;; FUNCIÓN PARA REDEFINIR TAMAÑOS E VARIABLES ::::::::::::

;;Función para cando se modifica o tamaño ou zoom da ventana

(define (redefinir-tamaños x)
  (define punto-pj-x (punto-x (pj-punto (xogo-pj x))))
  (set! ancho-fondo (* n-c-ancho l-cadrado))
  (set! alto-fondo (* n-c-alto l-cadrado))
  (set! ancho-fondo-ventana (* l-cadrado n-c-ancho-ventana))
  (set! alto-fondo-ventana (* l-cadrado n-c-alto-ventana))
  (set! fondo-0 (freeze (empty-scene ancho-fondo alto-fondo transparente)))
  (set! ancho-pj-i (image-width (rectangle l-cadrado (* l-cadrado 2) "solid" "black")))
  (set! alto-pj-i (image-height (rectangle l-cadrado (* l-cadrado 2) "solid" "black")))
  (set! empty-s 
        (cond
          ((file-exists? "imaxes/fondo-c3.jpg")
           (freeze (escalar (+ (* ancho-fondo-ventana 1.2) (- n-c-ancho n-c-ancho-ventana))
                            (+ (* ancho-fondo-ventana 1.2) (- n-c-ancho n-c-ancho-ventana))
                            (bitmap "imaxes/fondo-c3.jpg"))))
          (else
           (freeze (rectangle (* ancho-fondo-ventana 1.2) (* alto-fondo-ventana 1.2) "solid" "white")))))
  (set! cadrado-s (freeze (rectangle (+ l-cadrado 1) (+ l-cadrado 1) "solid" (make-color 50 50 50))))
  (set! cadrado-s-2 (freeze (rectangle (+ l-cadrado 1) (+ l-cadrado 1) "solid" "darkblue")))
  (set! cadrado-fin (freeze (rectangle (+ l-cadrado 1) (+ l-cadrado 1) "solid" "lightgreen")))
  (set! linea-alto (freeze (line 0 alto-fondo "lightgray")))
  (set! linea-ancho (freeze (line ancho-fondo 0 "lightgray")))
  (set! cadro-r (freeze
                 (overlay
                  (rectangle (* l-cadrado 0.75) (* l-cadrado 0.75) "outline" "red")
                  (rectangle l-cadrado l-cadrado "solid" transparente))))
  (set! movemento-lateral (/ l-cadrado 4))
  (set! movemento-salto (/ l-cadrado 4))
  (set! rectangulo-pj-normal
        (freeze
         (overlay
          (rectangle (- l-cadrado 1) (* l-cadrado 2) "outline" "darkred")
          (rectangle (- l-cadrado 1) (* l-cadrado 2) "solid" "black"))))
  (set! rectangulo-pj-agachado
        (freeze
         (overlay
          (rectangle (- l-cadrado 1) l-cadrado "outline" "darkred")
          (rectangle (- l-cadrado 1) l-cadrado "solid" "black"))))
  (set! fondo-estatico
        (freeze (por-cadros #t cadrado-s-2 lista-puntos-creados 
                            (pantalla-con-cadros cadrado-s lista-puntos
                                                 (pantalla-con-cadros cadrado-fin lista-puntos-ganar fondo-0)))))
  (set! fondo-estatico-cuadricula
        (freeze (por-cuadricula #t fondo-estatico)))
  (set! cambiando-tamaño #t)
  )

;; FUNCIÓNS PARA CREAR A CUADRICULA :::::

;; Creación da cuadricula

(define linea-alto (freeze (line 0 alto-fondo "lightgray")))

(define linea-ancho (freeze (line ancho-fondo 0 "lightgray")))

;;número(0) + imagen -> imagen

(define (cuadricula-filas py fondo)
  (place-image
   linea-ancho
   (/ ancho-fondo 2) py
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
     ((>= px ancho-fondo)
      fondo)
     (else
      (cuadricula-columnas (+ px l-cadrado) fondo)))))

(define (por-cuadricula bool fondo)
  (cond 
    (bool
     (cuadricula-filas 0 
                       (cuadricula-columnas 0 fondo)))
    (else fondo)))

; IMAGENES :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

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

; ancho e alto do personaje

(define ancho-pj-i (image-width (rectangle l-cadrado (* l-cadrado 2) "solid" "black")))

(define alto-pj-i (image-height (rectangle l-cadrado (* l-cadrado 2) "solid" "black")))

;; CADROS (PLATAFORMAS) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define cadrado-s (freeze (rectangle (+ l-cadrado 1) (+ l-cadrado 1) "solid" (make-color 50 50 50))))

(define cadrado-s-2 (freeze (rectangle (+ l-cadrado 1) (+ l-cadrado 1) "solid" "darkblue")))

(define cadrado-fin (freeze (rectangle (+ l-cadrado 1) (+ l-cadrado 1) "solid" "lightgreen")))

(define cadro-r (freeze
                 (overlay
                  (rectangle (* l-cadrado 0.75) (* l-cadrado 0.75) "outline" "red")
                  (rectangle l-cadrado l-cadrado "solid" transparente))))

;; TO-DRAW :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

;funcións necesarias:

;lista + imagen -> imagen

(define (pantalla-con-cadros cadrado lista fondo)
  (cond
    ((empty? lista) fondo)
    (else
     (place-image cadrado
                  (punto-x (punto-cadro->punto (car lista) l-cadrado))
                  (punto-y (punto-cadro->punto (car lista) l-cadrado))
                  (cond
                    ((empty? (cdr lista))
                     fondo)
                    (else
                     (pantalla-con-cadros cadrado (cdr lista) fondo)))))))

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
                     (punto-x (punto-cadro->punto (car lista) l-cadrado))
                     (punto-y (punto-cadro->punto (car lista) l-cadrado))
                     (cond
                       ((empty? (cdr lista))
                        fondo)
                       (else
                        (por-cadros bool tipo-cadro (cdr lista) fondo)))))))))

; pon os cadros das colisións (pulsar a tecla "C" mentras se xoga)

(define (por-cadros-e bool x tipo-cadro lista fondo)
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
                       ((< (punto-x (pj-punto (xogo-pj x))) (/ ancho-fondo-ventana 2))
                        (punto-x (punto-cadro->punto (car lista) l-cadrado)))
                       ((> (punto-x (pj-punto (xogo-pj x))) (- ancho-fondo (/ ancho-fondo-ventana 2)))
                        (- (punto-x (punto-cadro->punto (car lista) l-cadrado)) (- ancho-fondo ancho-fondo-ventana)))
                       (else
                        (- (punto-x (punto-cadro->punto (car lista) l-cadrado)) (- (punto-x (pj-punto (xogo-pj x)))
                                                                                   (/ ancho-fondo-ventana 2)))))
                     (cond
                       ((< (punto-y (pj-punto (xogo-pj x))) (/ alto-fondo-ventana 2))
                        (punto-y (punto-cadro->punto (car lista) l-cadrado)))
                       ((> (punto-y (pj-punto (xogo-pj x))) (- alto-fondo (/ alto-fondo-ventana 2)))
                        (- (punto-y (punto-cadro->punto (car lista) l-cadrado)) (- alto-fondo alto-fondo-ventana)))
                       (else
                        (- (punto-y (punto-cadro->punto (car lista) l-cadrado)) (- (punto-y (pj-punto (xogo-pj x)))
                                                                                   (/ alto-fondo-ventana 2)))))
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
  (overlay (text "XOGAR" (floor (* l-cadrado (/ n-c-ancho-ventana 35))) 
                 (if (equal? (xogo-estado x) "xogo->pantallas") "darkgray" "black"))
           (overlay
            (rectangle (* (* l-cadrado 5) (/ n-c-ancho-ventana 35)) 
                       (* (* l-cadrado 2) (/ n-c-ancho-ventana 35)) 
                       "outline" "black")
            (rectangle (* (* l-cadrado 5) (/ n-c-ancho-ventana 35))
                       (* (* l-cadrado 2) (/ n-c-ancho-ventana 35)) 
                       "solid" (if (equal? (xogo-estado x) "xogo->pantallas")
                                   "lightblue" "lightgray")))))

;; botón da pantalla de inicio donde pon: "EDITAR"

(define (boton-edicion x)
  (overlay (text "EDITAR" (floor (* l-cadrado (/ n-c-ancho-ventana 35))) 
                 (if (equal? (xogo-estado x) "xogo->edicion") "darkgray" "black"))
           (overlay
            (rectangle (* (* l-cadrado 5) (/ n-c-ancho-ventana 35)) 
                       (* (* l-cadrado 2) (/ n-c-ancho-ventana 35)) 
                       "outline" "black")
            (rectangle (* (* l-cadrado 5) (/ n-c-ancho-ventana 35))
                       (* (* l-cadrado 2) (/ n-c-ancho-ventana 35)) 
                       "solid" (if (equal? (xogo-estado x) "xogo->edicion")
                                   "lightblue" "lightgray")))))

;botón "ATRÁS"

(define (boton-atras x)
  (overlay (text "ATRÁS" (floor (* l-cadrado (/ n-c-ancho-ventana 40))) 
                 (if (equal? (xogo-estado x) "edicion->atras") "darkgray" "black"))
           (overlay
            (rectangle (* (* l-cadrado 5) (/ n-c-ancho-ventana 40)) 
                       (* (* l-cadrado 2) (/ n-c-ancho-ventana 40)) 
                       "outline" "black")
            (rectangle (* (* l-cadrado 5) (/ n-c-ancho-ventana 40))
                       (* (* l-cadrado 2) (/ n-c-ancho-ventana 40)) 
                       "solid" (if (equal? (xogo-estado x) "edicion->atras")
                                   "lightblue" "lightgray")))))

;botón "NUEVO"

(define (boton-nuevo x)
  (overlay (text "NUEVO" (floor (* l-cadrado (/ n-c-ancho-ventana 40))) 
                 (if (equal? (xogo-estado x) "edicion->nuevo") "darkgray" "black"))
           (overlay
            (rectangle (* (* l-cadrado 5) (/ n-c-ancho-ventana 40)) 
                       (* (* l-cadrado 2) (/ n-c-ancho-ventana 40)) 
                       "outline" "black")
            (rectangle (* (* l-cadrado 5) (/ n-c-ancho-ventana 40))
                       (* (* l-cadrado 2) (/ n-c-ancho-ventana 40)) 
                       "solid" (if (equal? (xogo-estado x) "edicion->nuevo")
                                   "lightblue" "lightgray")))))

; barra de "carga" da pantalla inicial. (non se usa)

(define (barra-carga x)
  (rectangle (* ancho-fondo-ventana (* (xogo-carga x) 0.01)) (/ alto-fondo-ventana 7) "solid" "lightblue"))

; fondo da pantalla inicial

(define (fondo-menu)
  (rectangle ancho-fondo-ventana (/ alto-fondo-ventana 7) "solid" (make-color 230 230 230)))

; detectar 'fases'
;número(0) + lista(empty)

(define (crear-lista-fases n lista)
  (cond
    ((file-exists? (string-append "fases/" (number->string n) "/nombre.txt"))
     (crear-lista-fases (+ n 1) (cons 
                                 (string-append "--  " (file->value (string-append "fases/" (number->string n) "/nombre.txt")) "  --")
                                 lista)))
    (else
     (reverse lista))))

; mostrar as 'fases' nas pantallas
;número + lista + número + imagen -> imagen

(define (mostrar-fases x n lista py fondo)
  (cond
    ((empty? lista) fondo)
    (else
     (place-image (overlay
                   (text (car lista) (round (/ alto-fondo-ventana 22)) (if  (= (xogo-seleccionado x) n) "darkred" "darkgray"))
                   (rectangle (- ancho-fondo-ventana 2) (/ alto-fondo-ventana 15) "solid" (if  (= (xogo-seleccionado x) n) "lightgray" "white")))
                  (/ (image-width fondo) 2)
                  py
                  (cond
                    ((empty? (cdr lista))
                     fondo)
                    (else
                     (mostrar-fases x (+ n 1) (cdr lista) (+ py (/ alto-fondo-ventana 10.2)) fondo)))))))

; pantalla inicial

(define (pantalla-inicio x)
  (place-image (text "MUNDO DE CADRADOS" (round (/ alto-fondo-ventana 15)) "black")
               (/ ancho-fondo-ventana 2) (/ alto-fondo-ventana 15)
               (place-image 
                (line ancho-fondo-ventana 0 "black")
                (/ ancho-fondo-ventana 2) (/ alto-fondo-ventana 7)
                (place-image 
                 (line ancho-fondo-ventana 0 "black")
                 (/ ancho-fondo-ventana 2) (- alto-fondo-ventana (/ alto-fondo-ventana 7))
                 (cond
                   ((equal? (xogo-estado x) "cargando")
                    (overlay 
                     (rectangle (- ancho-fondo-ventana 1) (- alto-fondo-ventana 1) "outline" "red")
                     (place-image
                      (barra-carga x)
                      (/ ancho-fondo-ventana 2) (/ (/ alto-fondo-ventana 7) 2)
                      (empty-scene ancho-fondo-ventana alto-fondo-ventana))))
                   (else
                    (place-image (boton-xogar x)
                                 (/ ancho-fondo-ventana 2) 
                                 (/ alto-fondo-ventana 3)
                                 (place-image (boton-edicion x)
                                              (/ ancho-fondo-ventana 2)
                                              (/ alto-fondo-ventana 1.8)
                                              (overlay (rectangle (- ancho-fondo-ventana 1) (- alto-fondo-ventana 1) "outline" "black")
                                                       (place-image
                                                        (barra-carga x)
                                                        (/ ancho-fondo-ventana 2) (/ (/ alto-fondo-ventana 7) 2)
                                                        (place-image
                                                         (fondo-menu)
                                                         (/ ancho-fondo-ventana 2) 
                                                         (- alto-fondo-ventana (/ (/ alto-fondo-ventana 7) 2))
                                                         (empty-scene ancho-fondo-ventana alto-fondo-ventana))))))))))))

;pantalla de selección

(define (pantalla-seleccion-e x)
  (place-image (text "MUNDO DE CADRADOS" (round (/ alto-fondo-ventana 15)) "black")
               (/ ancho-fondo-ventana 2) (/ alto-fondo-ventana 15)
               (place-image 
                (line ancho-fondo-ventana 0 "black")
                (/ ancho-fondo-ventana 2) (/ alto-fondo-ventana 7)
                (place-image 
                 (line ancho-fondo-ventana 0 "black")
                 (/ ancho-fondo-ventana 2) (- alto-fondo-ventana (/ alto-fondo-ventana 7))
                 (place-image (mostrar-fases x 1 (crear-lista-fases 1 empty) (/ alto-fondo-ventana 20)
                                             (rectangle (- ancho-fondo-ventana 5) 
                                                        (- alto-fondo-ventana (/ alto-fondo-ventana 3.2)) "solid" "white"))
                              (/ ancho-fondo-ventana 2)
                              (/ alto-fondo-ventana 2)
                              (overlay (rectangle (- ancho-fondo-ventana 1) (- alto-fondo-ventana 1) "outline" "black")
                                       (place-image
                                        (barra-carga x)
                                        (/ ancho-fondo-ventana 2) (/ (/ alto-fondo-ventana 7) 2)
                                        (place-image 
                                         (place-image (boton-atras x)
                                                      (/ ancho-fondo-ventana 13)
                                                      (/ (/ alto-fondo-ventana 7) 2)
                                                      (place-image (boton-nuevo x)
                                                                   (- ancho-fondo-ventana (/ ancho-fondo-ventana 13))
                                                                   (/ (/ alto-fondo-ventana 7) 2)
                                                                   (fondo-menu)))
                                         (/ ancho-fondo-ventana 2) 
                                         (- alto-fondo-ventana (/ (/ alto-fondo-ventana 7) 2))
                                         (empty-scene ancho-fondo-ventana alto-fondo-ventana)))))))))

; pantalla edición

(define (pantalla-edicion x)
  (define punto-pj-x (punto-x (pj-punto (xogo-pj x))))
  (define punto-pj-y (punto-y (pj-punto (xogo-pj x))))
  (place-image
   (if (xogo-cuadricula x) fondo-estatico-cuadricula fondo-estatico)
   (cond
     ((<= punto-pj-x (/ ancho-fondo-ventana 2))
      (+ (/ ancho-fondo-ventana 2) (/ (- ancho-fondo ancho-fondo-ventana) 2)))
     ((>= punto-pj-x (- ancho-fondo (/ ancho-fondo-ventana 2)))
      (- (/ ancho-fondo-ventana 2) (/ (- ancho-fondo ancho-fondo-ventana) 2)))
     (else
      (- (+ (/ ancho-fondo-ventana 2) (/ (- ancho-fondo ancho-fondo-ventana) 2)) 
         (- (punto-x (pj-punto (xogo-pj x)))
            (punto-x (pj-punto-pantalla (xogo-pj x)))))))
   (cond 
     ((<= punto-pj-y (/ alto-fondo-ventana 2))
      (+ (/ alto-fondo-ventana 2) (/ (- alto-fondo alto-fondo-ventana) 2)))
     ((>= punto-pj-y (- alto-fondo (/ alto-fondo-ventana 2)))
      (- (/ alto-fondo-ventana 2) (/ (- alto-fondo alto-fondo-ventana) 2)))
     (else
      (- (+ (/ alto-fondo-ventana 2) (/ (- alto-fondo alto-fondo-ventana) 2)) 
         (- (punto-y (pj-punto (xogo-pj x)))
            (punto-y (pj-punto-pantalla (xogo-pj x)))))
      ))
   (empty-scene ancho-fondo-ventana alto-fondo-ventana "white")))

; pantalla do xogo (incluindo a inicial)

(define (pantalla-xogo x)
  (cond
    ((member (xogo-estado x) (list "inicio" "xogo->pantallas" "xogo->edicion" "cargando"))
     (pantalla-inicio x)) ; pantalla-inicial
    ((member (xogo-estado x) (list "sel-pantallas-e" "edicion->atras" "edicion->nuevo"))
     (pantalla-seleccion-e x))
    ((equal? (xogo-estado x) "edicion")
     (pantalla-edicion x)) ;;pantalla-edición
    (else
     (define estado-m (pj-estado-m (xogo-pj x)))
     (define estado-s (pj-estado-s (xogo-pj x)))
     (define punto-pj-x (punto-x (pj-punto (xogo-pj x))))
     (define punto-pj-y (punto-y (pj-punto (xogo-pj x))))
     (overlay (text (cond
                      ((equal? (xogo-estado x) "game over")
                       "GAME OVER")
                      ((equal? (xogo-estado x) "completado")
                       "COMPLETADO")
                      (else 
                       ""))
                    (floor (/ ancho-fondo-ventana 15)) (cond
                                                         ((equal? (xogo-estado x) "game over")
                                                          "red")
                                                         ((equal? (xogo-estado x) "completado")
                                                          "green")
                                                         (else "black")))
              (por-cadros-e ; pantalla do xogo en sí
               (if (xogo-cuadricula x) #t #f) x
               cadro-r lista-pasable
               (place-image (img-pj x)
                            (punto-x (pj-punto-pantalla (xogo-pj x)))
                            (cond
                              ((and
                                (member #\s estado-m)
                                (equal? estado-s "floor"))
                               (+ (punto-y (pj-punto-pantalla (xogo-pj x))) (/ l-cadrado 2)))
                              (else
                               (punto-y (pj-punto-pantalla (xogo-pj x)))))      
                            (place-image
                             (if (and (xogo-cuadricula x) (equal? (xogo-estado x) "edicion")) fondo-estatico-cuadricula fondo-estatico)
                             (cond 
                               ((<= punto-pj-x (/ ancho-fondo-ventana 2))
                                (+ (/ ancho-fondo-ventana 2) (/ (- ancho-fondo ancho-fondo-ventana) 2)))
                               ((>= punto-pj-x (- ancho-fondo (/ ancho-fondo-ventana 2)))
                                (- (/ ancho-fondo-ventana 2) (/ (- ancho-fondo ancho-fondo-ventana) 2)))
                               (else
                                (- (+ (/ ancho-fondo-ventana 2) (/ (- ancho-fondo ancho-fondo-ventana) 2)) 
                                   (- (punto-x (pj-punto (xogo-pj x)))
                                      (punto-x (pj-punto-pantalla (xogo-pj x)))))
                                ))
                             (cond 
                               ((<= punto-pj-y (/ alto-fondo-ventana 2))
                                (+ (/ alto-fondo-ventana 2) (/ (- alto-fondo alto-fondo-ventana) 2)))
                               ((>= punto-pj-y (- alto-fondo (/ alto-fondo-ventana 2)))
                                (- (/ alto-fondo-ventana 2) (/ (- alto-fondo alto-fondo-ventana) 2)))
                               (else
                                (- (+ (/ alto-fondo-ventana 2) (/ (- alto-fondo alto-fondo-ventana) 2)) 
                                   (- (punto-y (pj-punto (xogo-pj x)))
                                      (punto-y (pj-punto-pantalla (xogo-pj x)))))
                                ))
                             (place-image 
                              empty-s
                              (cond
                                ((> punto-pj-x (- ancho-fondo (/ ancho-fondo-ventana 2)))
                                 (- (/ ancho-fondo-ventana 2)
                                    (/ (- (- ancho-fondo (/ ancho-fondo-ventana 2)) (/ ancho-fondo-ventana 2)) 30)))
                                ((> punto-pj-x (/ ancho-fondo-ventana 2))
                                 (- (/ ancho-fondo-ventana 2)
                                    (/ (- punto-pj-x (/ ancho-fondo-ventana 2)) 30)))
                                (else
                                 (/ ancho-fondo-ventana 2)))
                              (cond
                                ((> punto-pj-y (- alto-fondo (/ alto-fondo-ventana 2)))
                                 (- (/ alto-fondo-ventana 2)
                                    (/ (- (- alto-fondo (/ alto-fondo-ventana 2)) (/ alto-fondo-ventana 2)) 30)))
                                ((> punto-pj-y (/ alto-fondo-ventana 2))
                                 (- (/ alto-fondo-ventana 2)
                                    (/ (- punto-pj-y (/ alto-fondo-ventana 2)) 30)))
                                (else
                                 (/ alto-fondo-ventana 2)))
                              (empty-scene ancho-fondo-ventana alto-fondo-ventana)))))))))

; funcíon para escalar unha imagen

(define (escalar x y imagen)
  (scale/xy (/ x (image-width imagen)) (/ y (image-height imagen)) imagen))

(define empty-s 
  (cond
    ((file-exists? "imaxes/fondo-c3.jpg")
     (freeze (escalar (+ (* ancho-fondo-ventana 1.2) (- n-c-ancho n-c-ancho-ventana))
                      (+ (* ancho-fondo-ventana 1.2) (- n-c-ancho n-c-ancho-ventana))
                      (bitmap "imaxes/fondo-c3.jpg"))))
    (else
     (freeze (rectangle (* ancho-fondo-ventana 1.2) (* alto-fondo-ventana 1.2) "solid" "white")))))

;; ON-KEY :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

(define punto-pj-c #f)
(define punto-pj-pantalla-c #f)
(define punto-y-comeza-salto #f)
(define l-cadrado-edi 0)

;; PRESIONAR ::::::::::::::::::::::::::::::::::::::::::::::::::::

(define (tecla x t)
  (define distancia-bloque-enriba-c (distancia-bloque-enriba x (pj-punto (xogo-pj x)) lista-pasable empty l-cadrado))
  (when (and (equal? (xogo-estado x) "edicion")
             (equal? t #\return)
             (file-exists? "mapa-edicion.txt"))
    (delete-file "mapa-edicion.txt"))
  (when (and (equal? (xogo-estado x) "edicion") (equal? t #\return))
    (cond ((> (xogo-seleccionado x) 0)
           (delete-file (string-append "fases/" (number->string (xogo-seleccionado x)) "/puntos.txt"))
           (write-to-file (puntos->texto (append lista-puntos-creados lista-puntos))
                          (string-append "fases/" (number->string (xogo-seleccionado x)) "/puntos.txt")))
          (else
           (write-to-file (puntos->texto lista-puntos-creados) "fases/0/puntos0.txt"))))
  (cond
    ((member (xogo-estado x) (list "game over" "completado"))
     (cond
       ((equal? t #\return)
        (xogo 
         (pj 
          empty "floor"
          punto-inicial
          punto-inicial)
         0 0 #t 500 "inicio" 100 0 0))
       (else
        x)))
    ((member (xogo-estado x) (list "inicio" "xogo->pantallas"  "xogo->edicion" "sel-pantallas-e" "edicion->atras" "edicion->nuevo"))
     x)
    (else
     (define punto-pj-x (punto-x (pj-punto (xogo-pj x))))
     (define punto-pj-y (punto-y (pj-punto (xogo-pj x))))
     (define estado-m (pj-estado-m (xogo-pj x)))
     (define estado-s (pj-estado-s (xogo-pj x)))
     ;(when (equal? t #\return) (display (puntos->texto lista-puntos-creados)))
     (when (and (member t tecla-agacharse)
                (not (equal? (xogo-estado x) "edicion")))
       (set! agacharse #t))
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
           (not (equal? (xogo-estado x) "edicion"))
           (not (elemento-en-lista? #\s estado-m))
           (member t tecla-salto)
           (if 
            distancia-bloque-enriba-c
            (>  distancia-bloque-enriba-c
                1)
            #t)
           (equal? estado-s "floor"))
          (when (file-exists? "sonidos/salto.wav") (play-sound "sonidos/salto.wav" #t))
          "up")
         (else 
          estado-s))
       (pj-punto (xogo-pj x))
       (pj-punto-pantalla (xogo-pj x)))
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
          (equal? (xogo-estado x) "edicion")
          (equal? t #\c)
          (equal? (xogo-cuadricula x) #f))
         #t)
        ((and
          (equal? (xogo-estado x) "edicion")
          (equal? t #\c)
          (equal? (xogo-cuadricula x) #t))
         #f)
        (else
         (xogo-cuadricula x)))
      (xogo-cadros-restantes x)
      (xogo-estado x)
      (xogo-carga x)
      (xogo-conm x)
      (xogo-seleccionado x)))))

;;;;;

(define levantarse #f)
(define agacharse #f)

;; SOLTAR  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

(define (soltar-tecla x t)
  (cond
    ((member (xogo-estado x) (list "inicio" "xogo->pantallas" "xogo->edicion" "cargando"  "sel-pantallas-e"))
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
      (xogo-conm x)
      (xogo-seleccionado x)))))

;; ON-TICK ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

(define lista-pasable empty)

(define (tick x)
  (when (not cambiando-tamaño) (set! l-cadrado-a l-cadrado))
  (when (and 
         ancho-frame
         (> (positivo (- ancho-frame ancho-fondo-ventana)) l-cadrado))
    (when (not cambiando-tamaño) (set! l-cadrado-a l-cadrado))
    (set! l-cadrado (/ ancho-frame n-c-ancho-ventana))
    (redefinir-tamaños x))
  (when (and (equal? (xogo-estado x) "inicio")
             (not (empty? lista-puntos)))
    (set! lista-puntos empty)
    (set! VECTOR-PUNTOS-ORDENADOS (ordenar-punto-rev 0 0 lista-puntos empty n-c-alto n-c-ancho))
    (set! fondo-estatico
          (freeze (por-cadros #t cadrado-s-2 lista-puntos-creados 
                              (pantalla-con-cadros cadrado-s lista-puntos
                                                   (pantalla-con-cadros cadrado-fin lista-puntos-ganar fondo-0)))))
    (set! fondo-estatico-cuadricula
          (freeze (por-cuadricula #t fondo-estatico))))
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
      (xogo-seleccionado x)
      ))
    ((equal? (xogo-estado x) "edicion")
     (define punto-pj-x (punto-x (pj-punto (xogo-pj x))))
     (define punto-pj-y (punto-y (pj-punto (xogo-pj x))))
     (struct-copy xogo x
                  [pj 
                   [struct-copy pj (xogo-pj x)
                                [punto
                                 (punto
                                  (cond
                                    ((and
                                      (> (punto-x p-mouse) (+ (/ ancho-fondo-ventana 2) (/ ancho-fondo-ventana 4)))
                                      (< punto-pj-x (- ancho-fondo (/ ancho-fondo-ventana 2))))
                                     (+ punto-pj-x (/ (punto-x p-mouse) 50)))
                                    ((and
                                      (< (punto-x p-mouse) (- (/ ancho-fondo-ventana 2) (/ ancho-fondo-ventana 4)))
                                      (> punto-pj-x (/ ancho-fondo-ventana 2)))
                                     (- punto-pj-x (/ (- ancho-fondo-ventana (punto-x p-mouse)) 50)))
                                    (else
                                     punto-pj-x))
                                  (cond
                                    ((and
                                      (> (punto-y p-mouse) (+ (/ alto-fondo-ventana 2) (/ alto-fondo-ventana 5)))
                                      (< punto-pj-y (- alto-fondo (/ alto-fondo-ventana 2))))
                                     (+ punto-pj-y (/ (punto-y p-mouse) 50)))
                                    ((and
                                      (< (punto-y p-mouse) (- (/ alto-fondo-ventana 2) (/ alto-fondo-ventana 5)))
                                      (> punto-pj-y (/ alto-fondo-ventana 2)))
                                     (- punto-pj-y (/ (- alto-fondo-ventana (punto-y p-mouse)) 50)))
                                    (else
                                     punto-pj-y)))]
                                [punto-pantalla
                                 (punto
                                  (cond
                                    ((<= punto-pj-x (/ ancho-fondo-ventana 2))
                                     (if cambiando-tamaño 
                                         (* (/ punto-pj-x l-cadrado-a) l-cadrado)
                                         punto-pj-x))
                                    ((>= punto-pj-x (- ancho-fondo (/ ancho-fondo-ventana 2)))
                                     (- (if cambiando-tamaño
                                            (* (/ punto-pj-x l-cadrado-a) l-cadrado)
                                            punto-pj-x)
                                        (- ancho-fondo ancho-fondo-ventana)))
                                    (else
                                     (/ ancho-fondo-ventana 2)))
                                  (cond
                                    ((<= punto-pj-y (/ alto-fondo-ventana 2))
                                     (if cambiando-tamaño 
                                         (* (/ punto-pj-y l-cadrado-a) l-cadrado)
                                         punto-pj-y))
                                    ((>= punto-pj-y (- alto-fondo (/ alto-fondo-ventana 2)))
                                     (- (if cambiando-tamaño
                                            (* (/ punto-pj-y l-cadrado-a) l-cadrado)
                                            punto-pj-y)
                                        (- alto-fondo alto-fondo-ventana)))
                                    (else
                                     (/ alto-fondo-ventana 2))))]]]
                  [conm 
                   (cond
                     (cambiando-tamaño
                      (set! cambiando-tamaño #f)
                      3)
                     ((> (xogo-conm x) 0)
                      (- (xogo-conm x) 1))
                     (else
                      (xogo-conm x)))]))
    ((member (xogo-estado x) (list "inicio" "xogo->pantallas" "xogo->edicion"  "sel-pantallas-e" "edicion->atras" "edicion->nuevo"))
     (struct-copy xogo x [conm (cond
                                 (cambiando-tamaño
                                  (set! cambiando-tamaño #f)
                                  3)
                                 ((> (xogo-conm x) 0)
                                  (- (xogo-conm x) 1))
                                 (else
                                  (xogo-conm x)))]))
    ((member (xogo-estado x) (list "game over" "completado"))
     (struct-copy xogo x [conm (cond
                                 (cambiando-tamaño
                                  (set! cambiando-tamaño #f)
                                  3)
                                 ((> (xogo-conm x) 0)
                                  (- (xogo-conm x) 1))
                                 (else
                                  (xogo-conm x)))]))
    (else
     (define punto-pj-x (punto-x (pj-punto (xogo-pj x))))
     (define punto-pj-y (punto-y (pj-punto (xogo-pj x))))
     (define estado-m (pj-estado-m (xogo-pj x)))
     (define estado-s (pj-estado-s (xogo-pj x)))
     (set! lista-pasable
           (coller-puntos-circundantes x 
                                       (punto-c-x (punto->punto-cadro (pj-punto (xogo-pj x)) l-cadrado))
                                       (punto-c-y (punto->punto-cadro (pj-punto (xogo-pj x)) l-cadrado))
                                       VECTOR-PUNTOS-ORDENADOS n-c-ancho (pj-estado-s (xogo-pj x)) (xogo-vel-c x) l-cadrado))
     (define distancia-bloque-enriba-c (distancia-bloque-enriba x (pj-punto (xogo-pj x)) lista-pasable empty l-cadrado))
     (define distancia-bloque-abaixo-c (distancia-bloque-abaixo (pj-punto (xogo-pj x))  lista-pasable empty l-cadrado))
     (define distancia-bloque-esquerda-c (distancia-bloque-esquerda x (pj-punto (xogo-pj x)) lista-pasable empty l-cadrado))
     (define distancia-bloque-dereita-c (distancia-bloque-dereita x (pj-punto (xogo-pj x)) lista-pasable empty l-cadrado))
     (xogo
      (pj
       (cond ;; estado-m
         (cambiando-tamaño
          empty)
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
          (cond
            ((and ;;evitar a colision error con bloque dereito ao saltar
              (not (empty? estado-m))
              (equal? (car estado-m) #\d)
              (equal? estado-s "up")
              distancia-bloque-dereita-c
              (< distancia-bloque-dereita-c 0))
             estado-s)
            ((and ;;evitar a colision error con bloque esquerdo ao saltar
              (not (empty? estado-m))
              (equal? (car estado-m) #\a)
              (equal? estado-s "up")
              distancia-bloque-esquerda-c
              (< distancia-bloque-esquerda-c 0))
             estado-s)
            ((and
              distancia-bloque-abaixo-c
              (and
               (< distancia-bloque-abaixo-c 1)
               (> distancia-bloque-abaixo-c -1)))
             "floor")
            (else
             "down")))
         (else
          estado-s))
       (punto
        (cond ;;punto-pj-x
          (cambiando-tamaño
           (* (/ punto-pj-x l-cadrado-a) l-cadrado))
          ((and ;;volver a un punto correcto en x
            distancia-bloque-dereita-c
            (< distancia-bloque-dereita-c 0))
           (+ punto-pj-x distancia-bloque-dereita-c))
          ((and ;;volver a un punto correcto en x
            distancia-bloque-esquerda-c
            (< distancia-bloque-esquerda-c 0))
           (- punto-pj-x distancia-bloque-esquerda-c))
          ((and ;;chocar contra a parede dereita
            (not (empty? estado-m))
            (equal? (car estado-m) #\d)
            distancia-bloque-dereita-c
            (<= distancia-bloque-dereita-c movemento-lateral)
            (> distancia-bloque-dereita-c 0))
           (+ punto-pj-x distancia-bloque-dereita-c))
          ((and ;;chocar contra a parede esquerda
            (not (empty? estado-m))
            (equal? (car estado-m) #\a)
            distancia-bloque-esquerda-c
            (<= distancia-bloque-esquerda-c movemento-lateral)
            (> distancia-bloque-esquerda-c 0))
           (- punto-pj-x distancia-bloque-esquerda-c))
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
          ((and ;;evitar a colision error ao saltar contra un bloque a dereita
            (equal? estado-s "up")
            (not (empty? estado-m))
            (equal? (car estado-m) #\d)
            distancia-bloque-enriba-c
            distancia-bloque-dereita-c
            (< distancia-bloque-enriba-c 0)
            (< distancia-bloque-dereita-c 0))
           (- punto-pj-y (+ movemento-salto (round (/ (xogo-temp x) 2)))))
          ((and ;;evitar a colision error ao saltar contra un bloque a esquerda
            (equal? estado-s "up")
            (not (empty? estado-m))
            (equal? (car estado-m) #\a)
            distancia-bloque-enriba-c
            distancia-bloque-esquerda-c
            (< distancia-bloque-enriba-c 0)
            (< distancia-bloque-esquerda-c 0))
           (- punto-pj-y (+ movemento-salto (round (/ (xogo-temp x) 2)))))
          ((and
            (equal? estado-s "down")
            distancia-bloque-abaixo-c
            (<= distancia-bloque-abaixo-c (+ (xogo-vel-c x) 3)))
           (+ punto-pj-y distancia-bloque-abaixo-c))
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
             ((<= punto-pj-x (/ ancho-fondo-ventana 2))
              (if cambiando-tamaño 
                  (* (/ punto-pj-x l-cadrado-a) l-cadrado)
                  punto-pj-x))
             ((>= punto-pj-x (- ancho-fondo (/ ancho-fondo-ventana 2)))
              (- (if cambiando-tamaño
                     (* (/ punto-pj-x l-cadrado-a) l-cadrado)
                     punto-pj-x)
                 (- ancho-fondo ancho-fondo-ventana)))
             (else
              (/ ancho-fondo-ventana 2)))
           (cond
             ((<= punto-pj-y (/ alto-fondo-ventana 2))
              (if cambiando-tamaño 
                  (* (/ punto-pj-y l-cadrado-a) l-cadrado)
                  punto-pj-y))
             ((and
               (>= (* (/ punto-pj-y l-cadrado-a) l-cadrado) (- alto-fondo (/ alto-fondo-ventana 2)))
               cambiando-tamaño)
              (- (* (/ punto-pj-y l-cadrado-a) l-cadrado) 
                 (- alto-fondo alto-fondo-ventana)))
             ((>= punto-pj-y (- alto-fondo (/ alto-fondo-ventana 2)))
              (- punto-pj-y
                 (- alto-fondo alto-fondo-ventana)))
             (else
              (/ alto-fondo-ventana 2)))))
         ))
      (cond ;; temp
        ((and
          distancia-bloque-enriba-c
          (<= distancia-bloque-enriba-c 0))
         (cond
           ((and ;;evitar a colision error con bloque dereito ao saltar
             (not (empty? estado-m))
             (equal? (car estado-m) #\d)
             (equal? estado-s "up")
             distancia-bloque-dereita-c
             (< distancia-bloque-dereita-c 0))
            (- (xogo-temp x) (/ l-cadrado 8)))
           ((and ;;evitar a colision error con bloque esquerdo ao saltar
             (not (empty? estado-m))
             (equal? (car estado-m) #\a)
             (equal? estado-s "up")
             distancia-bloque-esquerda-c
             (< distancia-bloque-esquerda-c 0))
            (- (xogo-temp x) (/ l-cadrado 8)))
           (else
            0)))
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
         (xogo-conm x)))
      (xogo-seleccionado x)))))

;;; FUNCIÓNS DE FINALIZADO ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

(define (fora? x)
  (cond
    ((or
      (> (xogo-conm x) 0)
      cambiando-tamaño)
     #f)
    (else
     (define punto-pj (pj-punto (xogo-pj x)))
     (> (- (punto-c-y (punto->punto-cadro punto-pj l-cadrado)) 2) n-c-alto))))

(define (ganar? x)
  (cond
    ((or
      (> (xogo-conm x) 0)
      cambiando-tamaño)
     #f)
    (else
     (or
      (and
       (distancia-bloque-enriba x (pj-punto (xogo-pj x)) lista-puntos-ganar empty l-cadrado)
       (< (distancia-bloque-enriba x (pj-punto (xogo-pj x)) lista-puntos-ganar empty l-cadrado) 0))
      (and
       (distancia-bloque-abaixo (pj-punto (xogo-pj x)) lista-puntos-ganar empty l-cadrado)
       (< (distancia-bloque-abaixo (pj-punto (xogo-pj x)) lista-puntos-ganar empty l-cadrado) 0))
      (and
       (distancia-bloque-esquerda x (pj-punto (xogo-pj x)) lista-puntos-ganar empty l-cadrado)
       (< (distancia-bloque-esquerda x (pj-punto (xogo-pj x)) lista-puntos-ganar empty l-cadrado) 0))
      (and
       (distancia-bloque-dereita x (pj-punto (xogo-pj x)) lista-puntos-ganar empty l-cadrado)
       (< (distancia-bloque-dereita x (pj-punto (xogo-pj x)) lista-puntos-ganar empty l-cadrado) 0))))))

;;; ON-MOUSE :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

;click en boton xogo?
(define (clic-xogo? px py)
  (and
   (>= px (- (/ ancho-fondo-ventana 2) (/ (* (* l-cadrado 5) (/ n-c-ancho-ventana 35)) 2)))
   (<= px (+ (/ ancho-fondo-ventana 2) (/ (* (* l-cadrado 5) (/ n-c-ancho-ventana 35)) 2)))
   (>= py (- (/ alto-fondo-ventana 3)
             (/ (* (* l-cadrado 2) (/ n-c-ancho-ventana 35)) 2)))
   (<= py (+ (/ alto-fondo-ventana 3)
             (/ (* (* l-cadrado 2) (/ n-c-ancho-ventana 35)) 2)))))

;click en boton editar?
(define (clic-edicion? px py)
  (and
   (>= px (- (/ ancho-fondo-ventana 2) (/ (* (* l-cadrado 5) (/ n-c-ancho-ventana 35)) 2)))
   (<= px (+ (/ ancho-fondo-ventana 2) (/ (* (* l-cadrado 5) (/ n-c-ancho-ventana 35)) 2)))
   (>= py (- (/ alto-fondo-ventana 1.8)
             (/ (* (* l-cadrado 2) (/ n-c-ancho-ventana 35)) 2)))
   (<= py (+ (/ alto-fondo-ventana 1.8)
             (/ (* (* l-cadrado 2) (/ n-c-ancho-ventana 35)) 2)))))

;click en boton "atras"
(define (clic-atras? px py)
  (and
   (>= px (- (/ ancho-fondo-ventana 13) (/ (* (* l-cadrado 5) (/ n-c-ancho-ventana 40)) 2)))
   (<= px (+ (/ ancho-fondo-ventana 13) (/ (* (* l-cadrado 5) (/ n-c-ancho-ventana 40)) 2)))
   (>= py (- (- alto-fondo-ventana (/ (/ alto-fondo-ventana 7) 2))
             (/ (* (* l-cadrado 2) (/ n-c-ancho-ventana 40)) 2)))
   (<= py (+ (- alto-fondo-ventana (/ (/ alto-fondo-ventana 7) 2))
             (/ (* (* l-cadrado 2) (/ n-c-ancho-ventana 40)) 2)))))

;click en boton "nuevo"
(define (clic-nuevo? px py)
  (and
   (>= px (- (- ancho-fondo-ventana (/ ancho-fondo-ventana 13)) (/ (* (* l-cadrado 5) (/ n-c-ancho-ventana 40)) 2)))
   (<= px (+ (- ancho-fondo-ventana (/ ancho-fondo-ventana 13)) (/ (* (* l-cadrado 5) (/ n-c-ancho-ventana 40)) 2)))
   (>= py (- (- alto-fondo-ventana (/ (/ alto-fondo-ventana 7) 2))
             (/ (* (* l-cadrado 2) (/ n-c-ancho-ventana 40)) 2)))
   (<= py (+ (- alto-fondo-ventana (/ (/ alto-fondo-ventana 7) 2))
             (/ (* (* l-cadrado 2) (/ n-c-ancho-ventana 40)) 2)))))

;;; PULSAR :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


(define (mouse x px py boton)
  (cond
    ((equal? (xogo-estado x) "cargando")
     x)
    ((member (xogo-estado x) (list "inicio" "xogo->pantallas" "xogo->edicion"))
     (struct-copy xogo x [estado (cond
                                   ((clic-xogo? px py) "xogo->pantallas")
                                   ((clic-edicion? px py) "xogo->edicion")
                                   (else (xogo-estado x)))]))
    ((equal? (xogo-estado x) "sel-pantallas-e")
     (when (and (> (xogo-seleccionado x) 0)
                (file-exists? (string-append "fases/" (number->string (xogo-seleccionado x)) "/nombre.txt")))
       (set! lista-puntos (texto->l-puntos (file->value (string-append "fases/" (number->string (xogo-seleccionado x)) "/" "puntos.txt"))))
       (set! VECTOR-PUNTOS-ORDENADOS (ordenar-punto-rev 0 0 lista-puntos empty n-c-alto n-c-ancho))
       (set! fondo-estatico
             (freeze (por-cadros #t cadrado-s-2 lista-puntos-creados 
                                 (pantalla-con-cadros cadrado-s lista-puntos
                                                      (pantalla-con-cadros cadrado-fin lista-puntos-ganar fondo-0)))))
       (set! fondo-estatico-cuadricula
             (freeze (por-cuadricula #t fondo-estatico))))
     (struct-copy xogo x [estado (cond
                                   ((clic-atras? px py) "edicion->atras")
                                   ((clic-nuevo? px py) "edicion->nuevo")
                                   ((and (> (xogo-seleccionado x) 0)
                                         (file-exists? (string-append "fases/" (number->string (xogo-seleccionado x)) "/nombre.txt")))
                                    "edicion")
                                   (else (xogo-estado x)))]))
    ((member (xogo-estado x) (list "game over" "completado"))
     x)
    (else
     (define punto-pj-x (punto-x (pj-punto (xogo-pj x))))
     (define px-v 
       (cond 
         ((<= punto-pj-x (/ ancho-fondo-ventana 2))
          px)
         ((>= punto-pj-x (- ancho-fondo (/ ancho-fondo-ventana 2)))
          (+ px (- ancho-fondo ancho-fondo-ventana)))
         (else
          (+ px (- punto-pj-x (/ ancho-fondo-ventana 2))))))
     (define punto-pj-y (punto-y (pj-punto (xogo-pj x))))
     (define py-v 
       (cond 
         ((<= punto-pj-y (/ alto-fondo-ventana 2))
          py)
         ((>= punto-pj-y (- alto-fondo (/ alto-fondo-ventana 2)))
          (+ py (- alto-fondo alto-fondo-ventana)))
         (else
          (+ py (- punto-pj-y (/ alto-fondo-ventana 2))))))
     (cond 
       ((and
         (xogo-cuadricula x)
         ;(> (dist-puntos (punto-cadro->punto (punto->punto-cadro (punto px-v py-v) l-cadrado) l-cadrado) (pj-punto (xogo-pj x))) l-cadrado)
         (not (member (punto->punto-cadro (punto px-v py-v) l-cadrado) lista-puntos))
         (not (member (punto->punto-cadro (punto px-v py-v) l-cadrado) lista-puntos-creados))
         (> (xogo-cadros-restantes x) 0))
        (set! lista-puntos-creados (cons (punto->punto-cadro (punto px-v py-v) l-cadrado) lista-puntos-creados))
        (set! VECTOR-PUNTOS-ORDENADOS 
              (por-e-vector (list (punto->punto-cadro (punto px-v py-v) l-cadrado)) 
                            (inexact->exact (+ (floor (/ px-v l-cadrado)) 
                                               (* (floor (/ py-v l-cadrado)) n-c-ancho)))
                            VECTOR-PUNTOS-ORDENADOS))
        (set! fondo-estatico
              (freeze  (por-cadros #t cadrado-s-2 (list (punto->punto-cadro (punto px-v py-v) l-cadrado)) fondo-estatico))) 
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
         (xogo-conm x)
         (xogo-seleccionado x))
        (struct-copy xogo x [cadros-restantes (- (xogo-cadros-restantes x) 1)]))
       (else
        x)))))

;; SOLTAR ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

(define (soltar-mouse x px py boton)
  (sleep 0.10)
  (cond
    ((member (xogo-estado x) (list "game over" "completado"))
     (xogo 
      (pj 
       empty "floor"
       punto-inicial
       punto-inicial)
      0 0 #t 100 "inicio" 100 0 0))
    (else
     (xogo
      (struct-copy pj (xogo-pj x) [punto (cond
                                           ((and
                                             (member (xogo-estado x) (list "edicion->nuevo" "sel-pantalla-e"))
                                             (clic-nuevo? px py))
                                            (punto (/ ancho-fondo-ventana 2) (/ alto-fondo-ventana 2)))
                                           (else (pj-punto (xogo-pj x))))])       
      (xogo-temp x)
      (xogo-vel-c x)
      (cond
        ((and
          (member (xogo-estado x) (list "edicion->nuevo" "sel-pantalla-e"))
          (clic-nuevo? px py))
         #t)
        (else
         (xogo-cuadricula x)))
      (xogo-cadros-restantes x)
      (cond
        ((and
          (member (xogo-estado x) (list "xogo->pantallas"))
          (clic-xogo? px py))
         #;(when (file-exists? "mapa-edicion.txt")
           (set! lista-puntos (texto->l-puntos (file->value "mapa-edicion.txt")))) ;;;;;;;;;;;;;;;;;;;;; cargar arquivos
         #;(set! VECTOR-PUNTOS-ORDENADOS (ordenar-punto-rev 0 0 lista-puntos empty n-c-alto n-c-ancho))
         #;(set! fondo-estatico
               (freeze (por-cadros #t cadrado-s-2 lista-puntos-creados 
                                   (pantalla-con-cadros cadrado-s lista-puntos
                                                        (pantalla-con-cadros cadrado-fin lista-puntos-ganar fondo-0)))))
         #;(set! fondo-estatico-cuadricula
               (freeze (por-cuadricula #t fondo-estatico)))
         "inicio") ;sel-pantallas-x
        ((and
          (member (xogo-estado x) (list "edicion->atras"))
          (clic-edicion? px py))
         "inicio")
        ((and
          (member (xogo-estado x) (list "xogo->edicion"))
          (clic-edicion? px py))
         "sel-pantallas-e")
        ((member (xogo-estado x) (list "xogo->pantallas" "xogo->edicion"))
         "inicio")
        ((and
          (member (xogo-estado x) (list "edicion->atras"))
          (clic-atras? px py))
         "inicio")
        ((member (xogo-estado x) (list "edicion->atras"))
         "sel-pantallas-e")
        ((and
          (member (xogo-estado x) (list "edicion->nuevo"))
          (clic-nuevo? px py))
         (set! lista-puntos empty)
         (set! VECTOR-PUNTOS-ORDENADOS (ordenar-punto-rev 0 0 lista-puntos empty n-c-alto n-c-ancho))
         (set! fondo-estatico
               (freeze (por-cadros #t cadrado-s-2 lista-puntos-creados 
                                   (pantalla-con-cadros cadrado-s lista-puntos
                                                        (pantalla-con-cadros cadrado-fin lista-puntos-ganar fondo-0)))))
         (set! fondo-estatico-cuadricula
               (freeze (por-cuadricula #t fondo-estatico)))
         "edicion")
        ((member (xogo-estado x) (list "edicion->nuevo"))
         "sel-pantallas-e")
        (else
         (xogo-estado x)))
      (xogo-carga x)
      (xogo-conm x)
      (xogo-seleccionado x)))))

;; RODA DO RATÓN CARA ADIANTE  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

(define zoom 0)

(define (roda-adiante x)
  (cond
    ((and
      (< (xogo-conm x) 3)
      (> n-c-ancho-ventana 40)
      (member (xogo-estado x) (list "edicion"))
      )
     (set! zoom (+ zoom 1))
     (set! l-cadrado-a l-cadrado)
     (set! l-cadrado (+ l-cadrado 1))
     (set! n-c-ancho-ventana (round (/ ancho-fondo-ventana l-cadrado)))
     (set! n-c-alto-ventana (/ alto-fondo-ventana l-cadrado))
     (redefinir-tamaños x)
     x)
    (else
     x)))

;; RODA DO RATÓN CARA ATRÁS  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

(define (roda-atras x)
  (cond
    ((and
      (< (xogo-conm x) 3)
      (< n-c-ancho-ventana n-c-ancho)
      (< n-c-alto-ventana n-c-alto)
      (member (xogo-estado x) (list "edicion"))
      )
     (cond
       ((or
         (> (round (/ ancho-fondo-ventana (- l-cadrado 1))) n-c-ancho)
         (> (round (/ alto-fondo-ventana (- l-cadrado 1))) n-c-alto))
        (set! zoom (- zoom 1))
        (set! l-cadrado-a l-cadrado)
        (set! n-c-ancho-ventana n-c-ancho)
        (set! n-c-alto-ventana n-c-alto)
        (set! l-cadrado (/ n-c-ancho l-cadrado))
        (redefinir-tamaños x)
        x)
       (else
        (set! zoom (- zoom 1))
        (set! l-cadrado-a l-cadrado)
        (set! l-cadrado (- l-cadrado 1))
        (set! n-c-ancho-ventana (round (/ ancho-fondo-ventana l-cadrado)))
        (set! n-c-alto-ventana (/ alto-fondo-ventana l-cadrado))
        (redefinir-tamaños x)
        x)))
    (else
     x)))

;; MOVEMENTO DO RATÓN  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

(define p-mouse (punto 0 0))

(define (mover-mouse x px py)
  (when
      (equal? (xogo-estado x) "edicion")
    (set! p-mouse (punto px py)))
  (cond
    ((equal? (xogo-estado x) "sel-pantallas-e")
     (struct-copy xogo x [seleccionado (cond
                                         ((< py (/ alto-fondo-ventana 7)) 0)
                                         ((> py (- alto-fondo-ventana (/ alto-fondo-ventana 7))) 0)
                                         (else
                                          (inexact->exact (ceiling (/ (- py (/ alto-fondo-ventana 7)) 
                                                                      (/ (- alto-fondo-ventana (* (/ alto-fondo-ventana 7) 2)) 7))))))
                                       ]))
    (else
     x)))

;;BIG-BONG ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define punto-inicial (punto (- l-cadrado (/ ancho-pj-i 2)) 1))

(define (hai-que-repintar? velho novo)
  (or
      (xogo-cuadricula novo)
      (not (equal? (xogo-pj velho) (xogo-pj novo)))))

(big-bong (xogo 
           (pj 
            empty "floor"
            punto-inicial
            punto-inicial)
           0 0 #t 5000 "cargando" 100 0 0)
          'on-draw pantalla-xogo
          'on-key-press tecla
          'on-key-release soltar-tecla
          'on-mouse-press mouse
          'on-mouse-release soltar-mouse
          'on-mouse-move mover-mouse
          'on-wheel-down roda-atras
          'on-wheel-up roda-adiante
          'on-tick tick
          'needs-repaint? hai-que-repintar?
          ;'tick-interval (/ 1 40)
          ;'stop-when
          )
