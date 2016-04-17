#lang racket
(require racket/gui/base)
(require racket/draw)

(require "./gui.rkt")

(define nil '())

(define obj-list '())


;; Draw class
(define (drawing maingui)
  (let ((mouse-start-p '())
        (mouse-current-p '())
        (mouse-square '())
        (current-tool '())
        (current-pen '())
        (current-brush '()))
         

    ; Set initial pointer coordinates,
    ; set current drawing  tool.
    (define (d-begin event)
      (set! mouse-start-p
            (list (send event get-x)
                  (send event get-y)))
      (set-current-tool (mk-current-tool
                         (maingui 'get-current-tool))))


    ; Current tool selector
    (define (mk-current-tool type)
      (cond ((eq? type 'line) (list 'line
                                    line))
            ((eq? type 'circle) (list 'circle
                                      circle))
            (else (list 'nothing
                        (λ([var #f]) 'emptylambda)))))

    ;Set current tool
    (define (set-current-tool tool-pair)
      (set! current-tool tool-pair))
      

    ; Make pen
    (define (mk-pen)
      (set! current-pen (new pen%
                          [color "red"]
                          [width 5]
                          [style 'solid])))
    ; Make brush
    (define (mk-brush)
      (set! current-brush (new brush%
                               [color (make-object
                                          color%
                                        10
                                        10
                                        10
                                        0.0)]))) ; alpha
            

    ; Set current pen
    (define (set-dc-pen)
      (send (maingui 'get-bmp-dc) set-pen current-pen))
    ; Set current brush
    (define (set-dc-brush)
      (send (maingui 'get-bmp-dc) set-brush current-brush))


    ; Set current pointer coord
    (define (set-mouse-current event)
      (if (null? event) #f
          (set! mouse-current-p
                (list (send event get-x)
                      (send event get-y)))))

    ; mk pointer start-end "square"
    (define (mk-mouse-square [params #f])
      (set! mouse-square (if (not params)
                            (list (car mouse-start-p)
                                  (cadr mouse-start-p)
                                  (car mouse-current-p)
                                  (cadr mouse-current-p))
                            params)))
    
    
    ; Shape accessor
    (define (d-draw event [type #f])
      (mk-pen)
      (set-dc-pen)
      (mk-brush)
      (set-dc-brush)
      (set-mouse-current event)
      (cond ((null? current-tool)
             (error "current-tool not initialized"))
            ((not type) (cadr current-tool))
            (else  (cadr (mk-current-tool type)))))
      
    ;sqr
    (define (sqr x) (* x x))
    
    ;; Shape-specific procedures
    ;Line - coords x1, y1, x2, y2
    (define (line [params #f])
      (mk-mouse-square params)
      (send (maingui 'get-bmp-dc) draw-line
              (car mouse-square)
              (cadr mouse-square)
              (caddr mouse-square)
              (cadddr mouse-square)))

    ;Circle - coords cx, cy, r
    (define (circle [params #f])
      (mk-mouse-square params)
      (let ((cx (car mouse-square))
            (cy (cadr mouse-square))
            (r  (sqrt (+ (sqr (- (car mouse-square)
                                 (caddr mouse-square)))
                         (sqr (- (cadr mouse-square)
                                 (cadddr mouse-square)))))))
      (send (maingui 'get-bmp-dc) draw-ellipse
            cx
            cy
            r
            r)))
      
    ;; -------------------------

    
    ; dispatch
    (define (dispatch msg)
      (cond ((eq? msg 'begin) d-begin)
            ((eq? msg 'draw)  d-draw)
            ((eq? msg 'get-mg) maingui)
            ((eq? msg 'get-mouse) (append mouse-start-p
                                          mouse-current-p))
            ((eq? msg 'get-tool-type) (car current-tool))))
            ;((eq? msg 'end) )))
    dispatch))

; Elements constructor
(define (element type param)
  (let ((t type)
        (p param))
    (define (dispatch msg)
      (cond ((eq? msg 'get-param) p)
            ((eq? msg 'get-type) t)))
    dispatch))
; ===========================================================



;; Canvas% class override for event handling
(define s-canvas%
  (class canvas% 
    (define/override (on-event event)
      (begin
        (main-gui 'clear-bmp)
        (draw-all-elements obj-list)
        (cond
          ((send event button-down?)
           ((main-draw 'begin) event))
                  ;((main-draw 'draw) event)))
          
          ((send event button-up?)
           (set! obj-list (append obj-list (list (element
                                                  (main-draw 'get-tool-type)
                                                  (main-draw 'get-mouse))))))
          
          ((send event dragging?)
           [((main-draw 'draw) event)]))
        (main-gui 'refresh-canvas)))
;      (define/override (on-char event)
;        'a)
    
    (super-new)
    ))
  
; =====================================================================


; Init main-gui object
(define main-gui (mk-gui))

; Init drawing object
(define main-draw (drawing main-gui))

; GUI prep and display
(define (gui-init)
  ((main-gui 'set-canvas) s-canvas%)
  (main-gui 'show)
  (main-gui 'bmp-resize))  ; Set bitmap to canvas size)

(gui-init)


;(define (del-obj obj olist) (filter (λ(x)(not(equal? x obj))) olist))
(define (draw-element obj) ([(main-draw 'draw) nil (obj 'get-type)] (obj 'get-param)))
  
(define (draw-all-elements o-list)
    (map (λ(x)(draw-element x)) o-list))
