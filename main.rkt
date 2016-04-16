#lang racket
(require racket/gui/base)
(require racket/draw)

(require "./gui.rkt")

(define mouse-d-pos (list 0 0))

(define tstE (λ(obj e)(begin (send canv-bitmap-dc clear)
                             (set! obj-list (list '(0 0 0 0)))
                             (send m-wnd-canv refresh-now))))


(define tst (λ(obj e) 'a))
(define p-callback (λ (canvas dc) (send dc draw-bitmap canv-bitmap 0 0)))

(define obj-list (list '(0 0 0 0)))
(define (del-obj obj olist) (filter (λ(x)(not(equal? x obj))) olist))
(define draw-object (λ(x y x2 y2)(send canv-bitmap-dc draw-line
                                        x
                                        y
                                        x2
                                        y2)))
(define (draw-object-all o-list)
  (map (λ(x)(apply draw-object x)) o-list))


;Canvas% class override for event handling
(define s-canvas%
  (class canvas% 
    (define/override (on-event event)
      (begin
        (send canv-bitmap-dc clear)
        (draw-object-all obj-list)
        (cond
          ((send event button-down?)
           (set! mouse-d-pos (list (send event get-x)
                                         (send event get-y))))
          ((send event button-up?)
           (set! obj-list (append obj-list
                                 (list (append mouse-d-pos
                                               (list (send event get-x)
                                                     (send event get-y)))))))
        #|  ((send event dragging?)
           ;(begin
             (send canv-bitmap-dc draw-line
                   (car mouse-d-pos)
                   (cadr mouse-d-pos)
                   (send event get-y)
                   (send event get-x))
             ;)
           )
|#

                         ((send event dragging?)

;(define (shape-to-draw var-if-needed)

(let (( y1-coorid (cadr mouse-d-pos))
       (x1-coorid (car mouse-d-pos)))
(define x2-coorid ((λ () (send event get-x))))
(define y2-coorid ((λ () (send event get-y))))
(define y-run     ((λ () (abs (- y2-coorid  y1-coorid)))))
(define x-run     ((λ () (abs (- x2-coorid x1-coorid)))))
(define x-start   ((λ () (min x1-coorid x2-coorid))))
(define y-start   ((λ () (min y1-coorid y2-coorid))))



(define (make-perfect ) 

(define (dispatch m )
  (define same-run ((λ () (begin (set! x-run (max x-run y-run)) x-run))))
(define startx ((λ () ( if (> x1-coorid x2-coorid) (begin (set! x-start (- x1-coorid same-run )) x-start) x-start))))
(define starty ((λ () ( if (> y1-coorid y2-coorid) (begin (set! y-start (- y1-coorid same-run )) y-start) y-start))))

  ( cond ((equal? m 'x ) startx)
         ((equal? m 'y ) starty)
        ((equal? m 'run ) same-run)
         (else (error "something bad happend" ))))
        dispatch )



  

                          (define line
(send canv-bitmap-dc draw-line
                   x-start
                  y-start
                   x2-coorid
                   y2-coorid))

  (define rectangle 

         
  (send canv-bitmap-dc draw-rectangle
                    x-start
                    y-start
                    x-run
                    y-run))
(define square  

         
  (send canv-bitmap-dc draw-rectangle
                    ((make-perfect) 'x)
                    ((make-perfect) 'y)

                    ((make-perfect) 'run)
                  ((make-perfect) 'run)
                    ))
  

  (define ellipse

         
  (send canv-bitmap-dc draw-ellipse
                    x-start
                    y-start
                   x-run
                    y-run))


  (define circle

         
  (send canv-bitmap-dc draw-ellipse
                 ((make-perfect) 'x)
                ((make-perfect) 'y)
                   ((make-perfect) 'run)
                    ((make-perfect) 'run)
                    ))


  


(define (dispatch m )

  ( cond ((equal? m 'rectangle ) 1)
         ((equal? m 'ellipse ) 2)
        ((equal? m 'circle ) 3)
        ((equal? m 'line ) 1)
        ((equal? m 'square ) square)
         (else (error "something bad happend" ))))

dispatch 

  )

 
);))
          );one to close cond
        (send m-wnd-canv refresh-now)))
    (define/override (on-char event)
      'a)
    (super-new))
  )


;;;Main Window Canvas, and canvas dc
;Canvas is placed in a main window pane defined in GUI.rkt
(define m-wnd-canv (new s-canvas%
                        [parent m-wnd-pane]
                        [paint-callback p-callback]))

(define m-wnd-canv-dc (send m-wnd-canv get-dc))

;; Bitmap for drawing, and bitmap-dc
(provide canv-bitmap)
(provide canv-bitmap-dc)

(define canv-bitmap (make-object bitmap% 1 1))
(define canv-bitmap-dc (new bitmap-dc% [bitmap canv-bitmap]))

;Set bitmap width-height to canvas size
(set! canv-bitmap (make-object bitmap%
                    (send m-wnd-canv get-width)
                    (send m-wnd-canv get-height)))
(send canv-bitmap-dc set-bitmap canv-bitmap)

;;define demo color and set pen for canvas-dc
(define black (make-object color% 10 10 10))
(send canv-bitmap-dc set-pen black 5 'solid)
