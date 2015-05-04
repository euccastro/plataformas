#lang racket

(struct punto (x y))
(struct punto-c (x y))


(struct pj (estado-m estado-s punto punto-pantalla) #:transparent)

(struct xogo (pj temp vel-c cuadricula cadros-restantes estado carga conm p-mouse) #:transparent)


(provide (struct-out punto))
(provide (struct-out punto-c))
(provide (struct-out pj))
(provide (struct-out xogo))

