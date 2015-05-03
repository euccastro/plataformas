#lang racket

(struct punto (x y))
(struct punto-c (x y))


(struct pj (estado-m estado-s punto punto-pantalla) #:transparent)

(struct xogo (pj temp vel-c cuadricula cadros-restantes estado carga conm p-mouse) #:transparent)


(provide punto)
(provide punto-c)

(provide punto-x)
(provide punto-c-x)

(provide punto-y)
(provide punto-c-y)

(provide punto?)
(provide punto-c?)

(provide pj)

(provide pj-estado-m)
(provide pj-estado-s)
(provide pj-punto)
(provide pj-punto-pantalla)

(provide xogo)
(provide xogo-pj)
(provide xogo-temp)
(provide xogo-vel-c)
(provide xogo-cuadricula)
(provide xogo-cadros-restantes)
(provide xogo-estado)
(provide xogo-carga)
(provide xogo-conm)
(provide xogo-p-mouse)


