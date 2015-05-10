#lang racket

(require 2htdp/image)
(require (except-in racket/gui make-color make-pen))
(require (only-in mrlib/image-core render-image))
(require "constantes.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;; MODULO BIG-BONG ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ancho-frame #f)
(define alto-frame #f)

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
                           (stop-when . #f)
                           (needs-repaint? . #f))))
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
  (define needs-repaint? (or (hash-ref handlers 'needs-repaint?) equal?))
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
              (let ((estado-velho state)
                    (estado-novo (apply h state rest)))
                (set! state estado-novo)
                (when (needs-repaint? estado-velho estado-novo)
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


(provide big-bong)
(provide ancho-frame)
(provide alto-frame)