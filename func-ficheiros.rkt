#lang racket

;;;;FUNCIÓNS PARA GARDAR UN ARQUIVO

(require racket/file)
(require "structs.rkt")

;;convertir puntos en texto

;lista -> texto

(define (puntos->texto lista-puntos)
  (cond 
    ((empty? lista-puntos)
     "")
    (else
     (string-append 
      (number->string (punto-c-x (car lista-puntos))) "_" (number->string (punto-c-y (car lista-puntos))) ":"
      (puntos->texto (cdr lista-puntos))))))

;detectar punto
;texto + número(0) -> número

(define (detectar-guion texto cont)
  (cond
    ((equal? (substring texto 0 1) "_")
     cont)
    (else
     (detectar-guion (substring texto 1 (string-length texto)) (+ cont 1)))))

;detectar dous puntos
;texto + número(0) -> número

(define (detectar-dous-puntos texto cont)
  (cond
    ((equal? (substring texto 0 1) ":")
     cont)
    (else
     (detectar-dous-puntos (substring texto 1 (string-length texto)) (+ cont 1)))))

;texto -> punto-c

(define (texto->punto-c texto)
  (punto-c (string->number (substring texto 0 (detectar-guion texto 0)))
           (string->number (substring texto (+ (detectar-guion texto 0) 1) (detectar-dous-puntos texto 0)))))

;convertir texto en lista de puntos
;texto + empty -> lista

(define (texto->l-puntos texto)
  (cons (texto->punto-c texto)
        (cond
          ((= (detectar-dous-puntos texto 0) 
              (- (string-length texto) 1))
           empty)
          (else
           (texto->l-puntos (substring texto 
                                       (+ (detectar-dous-puntos texto 0) 1)
                                       (string-length texto)))))))

;(file->string path [#:mode mode-flag]) → string?
;  path : path-string?
;  mode-flag : (or/c 'binary 'text) = 'binary

;gardar texto en un arquivo:

;(write-to-file (puntos->texto l) "proba.txt")

;cargar texto de un arquivo:

;(texto->l-puntos (file->value "proba.txt"))

;;exportar

(provide puntos->texto)

(provide texto->l-puntos)


