#lang racket
(require racket/gui/base)
(require racket/draw)

(require "./gui.rkt")
(require "./svg.rkt")

(define nil '())

;(define mouse-d-pos (list 0 0))
;(define obj-list '())


;; Draw class
(define (drawing maingui)
  (let ((mouse-start-p '())
        (mouse-current-p '())
        (mouse-square '())
        (current-tool '())
        (current-pen '())
        (current-brush '()))
         

    ;sqr
    (define (sqr x) (* x x))

    ; Current tool selector
    (define (mk-current-tool type)
      (cond ((eq? type 'line) (list 'line
                                    line))
            ((eq? type 'ellipse) (list 'ellipse
                                      ellipse))
            (else (list 'n
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

    ; Set starting pointer coord
    (define (set-mouse-start event)
      (set! mouse-start-p
            (list (send event get-x)
                  (send event get-y))))

    ; mk pointer start-end "square"
    (define (mk-mouse-square [params #f])
      (begin (set! mouse-square (if (not params)
                                    (append mouse-start-p
                                            mouse-current-p)
                                    params))
             mouse-square))

    
    ; Set initial pointer coordinates,
    ; set current drawing  tool.
    (define (d-begin event)
      (set-mouse-start event)
      (set-current-tool (mk-current-tool
                         (maingui 'get-current-tool))))
    
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
      
    ; -------------------------------------------------
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
    (define (ellipse [params #f])
      (mk-mouse-square params)
      (let ((sx (min (car mouse-square)
                     (caddr mouse-square)))
            (sy (min (cadr mouse-square)
                     (cadddr mouse-square)))
            (w (abs (- (caddr mouse-square)
                       (car mouse-square))))
            (h (abs (- (cadddr mouse-square)
                       (cadr mouse-square)))))
        (mk-mouse-square (list sx sy (+ sx w)  (+ sy h)))
        (send (maingui 'get-bmp-dc) draw-ellipse
              sx
              sy
              w
              h)))

    ;; ---------------------------------------------------

    
    ; dispatch
    (define (dispatch msg)
      (cond ((eq? msg 'begin) d-begin)
            ((eq? msg 'draw)  d-draw)
            ;((eq? msg 'end)  d-end)
            ((eq? msg 'get-mg) maingui)
            ((eq? msg 'get-mouse) (mk-mouse-square))
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
        (draw-all-elements (main-svg 'get-e-list))
        (cond
          ((send event button-down?)
           ((main-draw 'begin) event))
                  ;((main-draw 'draw) event)))
          
          ((send event button-up?)
           ((main-svg 'add-shape) (main-draw 'get-tool-type)
                                  (main-draw 'get-mouse)))
          
          ((send event dragging?)
           [((main-draw 'draw) event)]))
        (main-gui 'refresh-canvas)))
;      (define/override (on-char event)
;        'a)
    
    (super-new)
    ))
  
; =====================================================================



; ==============================Inits==================================
; Init main-gui object
(define main-gui (mk-gui))

; Init drawing object
(define main-draw (drawing main-gui))

; Init svg module
(define main-svg (svg))
  
; GUI prep and display
(define (gui-init)
  ((main-gui 'set-svg) main-svg) ;Set svg object to work with
  ((main-gui 'set-canvas) s-canvas%)
  (main-gui 'show)
  (main-gui 'bmp-resize)  ; Set bitmap to canvas size (also sets svg w/h)
  (main-gui 'svg-resize))

(gui-init)
; ====================================================================


;(define (del-obj obj olist) (filter (λ(x)(not(equal? x obj))) olist))
(define (draw-element element) ([(main-draw 'draw) nil (element 'get-type)]
                                (element 'get-param)))
  
(define (draw-all-elements e-list)
    (map (λ(x)(draw-element x)) e-list))
