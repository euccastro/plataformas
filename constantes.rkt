#lang racket

;;;;CONSTANTES XOGO ::::::::::::::::

(define l-cadrado 8)

(define l-cadrado-a l-cadrado)

(define n-c-ancho-ventana 80)

;;relacion -> n-c-alto-ventana = (/ n-c-ancho 2)

(define n-c-alto-ventana 40)

(define ancho-inicial (* l-cadrado n-c-ancho-ventana))

(define alto-inicial (* l-cadrado n-c-alto-ventana))

(provide l-cadrado)
(provide l-cadrado-a)
(provide n-c-ancho-ventana)
(provide n-c-alto-ventana)
(provide ancho-inicial)
(provide alto-inicial)
