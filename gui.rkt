#lang racket
(require racket/gui/base)
(require racket/draw)

(require "./svg.rkt")

;;;;; GUI one-time Constructor.
;;;;; Used in main.rkt
;;
;; Dispatch Messages :
;;
;;	'bmp-resize - stretch bitmap to current canvas dimentions
;;	'clear-bmp - clear bitmap
;;	'refresh-canvas - force immediate canvas update
; 
;;	('set-canvas) - takes in an s-canvas% class as argument,
;;					initializes s-canvas object.
;; 'set-svg - sets "svg" variable. Svg object is used to build
;;            elements list, importing/exporting svg and for
;;            other operations on the list of drawn shapes.
;; 'svg-resize - sets width/height property for svg document.
;;               Invokes procedure from svg object.
;;
;;	'get-bmp-dc - returns bitmap's drawing context
;;	'get-current-tool - returns symbol identifying current shape (current-tool).
;;						Current-tool is set in callback procedure triggered by
;;						toolbox gui controls/buttons.
;;
;;	'get-current-util - returns symbol id'ing current utility/function
;;						(open, save, etc.) (not implemented yet)
;;	'get-util-btns - list of utility box controls,
;;					 each paired with id symbol in a sublist.
;;	'get-tool-btns - list of tool box controls,
;;					 each paired with id symbol in a sublist.
;; 'get-pen   - returns current pen (stroke) set in color/prop window
;; 'get-brush - returns current brush (fill) set in color/prop window
;;
;;----
;;----

;; Main window constructor
(provide mk-gui)
(define (mk-gui)
  
  ; GUI Callbacks
  ; --------------
  
  ; Main canvas callback
  (define p-callback
    (λ (canvas dc)
      (send dc
            draw-bitmap
            bmp
            0 0)))
  
  
  ; Selected util
  (define current-util '())
  ; Set current-util
  (define (set-util util)
    (set! current-util util))
  
  ; Trigger canvas event
  (define (trigger-event)
    (send m-wnd-canvas
          on-event
          (new mouse-event%	 
               [event-type 'enter])))
  ; Common util-box buttons callback
  (define util-callback
    (λ (obj event)
      (begin (set-util 
              (cadr (assq obj util-buttons)))
             (cond ((eq? current-util 'save)
                    [(svg 'save) (put-file)])
                   ((eq? current-util 'undo)
                    (begin (svg 'remove-last)
                           (trigger-event)
                           (refresh-canvas)))
                   ((eq? current-util 'load)
                    (begin
                      [(svg 'load) (get-file)]
                      (trigger-event)
                      (refresh-canvas)))
                   ((eq? current-util 'clear)
                    (let ((clear-dialog
                           (message-box "Clear"
                                        "Clear Canvas?"
                                        m-wnd
                                        (list 'ok-cancel))))
                      (cond ((eq? clear-dialog
                                  'ok)
                             (svg 'clear)
                             (trigger-event)
                             (refresh-canvas)))))))))
  
  
  
  ; Selected tool
  (define current-tool '())
  ; Common tool-box buttons callback
  (define tool-callback
    (λ (obj event)
      (begin (set! current-tool 
                   (cadr (assq obj tool-box-buttons))) ;search tool buttons list for obj, get obj's id
             (set-status (string-append
                          status-msg
                          (send obj get-label))))))
  
  
  ; GUI elements
  ; ------------
  
  ;; Main window frame
  (define m-wnd (new frame%
                     [label "SkraM-SVG"]
                     [x 0]
                     [y 0]
                     [min-width 800]
                     [min-height 600]
                     [stretchable-width #f]	 
                     [stretchable-height #f]))
  
  ; =========================================================
  ;; Color/Properties window
  
  ;; Make current Pen/Brush color, associated procs
  (define c-pen '())
  (define c-brush '())
  
  (define (mk-color clr)

;(display (/ (cadddr clr) 10.0 ))
    (make-object
        color%
      (car clr)
      (cadr clr)
      (caddr clr)
      (/ (cadddr clr) 10.0 )
      )   )
  
  (define (mk-pen clr)
    (set! c-pen (new pen%
                     [color clr]
                     [width 5]
                     [style 'solid])))
  (define (mk-brush clr)
    (set! c-brush (new brush% [color clr])))
  
  (mk-pen (mk-color '(0 0 0 0)))
  (mk-brush (mk-color '(0 0 0 0)))
  (define pcolors (mk-color '(0 0 0 0)));pen colors
  (define bcolors (mk-color '(0 0 0 0)));brush colors
  (define psave (list 0 0 0 1)) ;used to save pen settings
  (define bsave (list 0 0 0 1)) ;used to save brush settings
  (define current-sf-radio 0)
  
  
  ; Color select canvas callback
  (define c-callback
    (λ (canvas dc)
      (send dc
            draw-bitmap
            bmp-c
            0 0)))
  
  ; Color sliders common callback
  
  
  (define color-callback
    (λ (obj event)
      (begin
        (send bmp-c-dc clear )
        (cond 
          ((eq? current-sf-radio 0);true if stroke selected
           (begin
             (set!
              pcolors
              
              ;sets pcolors to the current slider selections
              (map (λ (x)(send (car x) get-value))
                   color-sliders))             
             (mk-pen (mk-color pcolors)) ;sets the current color             
             (set! psave pcolors) ;saves the colors
             ; (mk-brush (mk-color colors ));'transparent
             ))    
          ((eq? current-sf-radio 1) ;true if fill has been selected
           (begin
             (set! bcolors;sets bcolors to the current slider selection
                   (map (λ (x)(send (car x) get-value));problem causer
                        color-sliders))             
             (mk-brush (mk-color bcolors)) ;sets the current color             
             (set! bsave bcolors)))) ;saves the color selection          
        ;        ( if (eq? current-fill-check #f) ;checks if user wants to fill color
        ;             (mk-brush  (mk-color '(0 0 0) 0.0)) ;no color
        ;             (mk-brush  (mk-color  bcolors))) ; color      
        (send bmp-c-dc set-pen c-pen) 
        (send bmp-c-dc set-brush c-brush)
        (send bmp-c-dc draw-ellipse
              30
              50
              140
              50)
        (send p-wnd-canvas refresh-now)
        )))
  
  ; Current radio-box
  ;(define current-sf-radio 0)
  ; Stroke/Fill radio buttons
  ; common callback
  
  (define sf-callback
    (λ (obj event)
      (begin        
        (set! current-sf-radio
              (send sf-radio-box get-selection))      
       ; (display "\n")
       ; (display psave)
        (cond ((eq? current-sf-radio 0)               
               (map (λ (x y)(send (car x) set-value  y ))
                    color-sliders psave))        
              ((eq? current-sf-radio 1)               
               (map (λ (x y)(send (car x) set-value  y ))
                    color-sliders bsave)))                      
        (color-callback #f #f))))
  
  
  ; Properties window frame
  (define p-wnd (new frame%
                     [label "Color"]
                     [x 800]
                     [y 0]
                     [min-width 200]
                     [min-height 370]
                     [style (list 'no-system-menu)]
                     [parent m-wnd]
                     [stretchable-width #f]	 
                     [stretchable-height #f]))
  
  (define p-wnd-slider_pane (new vertical-pane% [parent p-wnd]
                                 [alignment (list 'center 'center)]))
  
  ;; Color/prop window canvas
  (define p-wnd-canvas (new canvas%
                            [parent p-wnd-slider_pane]
                            ;[min-width 50]	 
                            ;[min-height min-height]
                            [paint-callback c-callback]))
  
  
  ;; Bitmap for color selection, and bitmap-dc
  (define bmp-c (make-object bitmap% 200 300))
  (define bmp-c-dc (new bitmap-dc%
                        [bitmap bmp-c]))
  
  ;; Generic make sliders.
  ;; slider_list - (label (range-s range-e) tag)
  (define (mk-sliders gui_parent slider_list callproc)
    (map (λ(x) (list (new slider%
                          [label (car x)]	 
                          [min-value (car (cadr x))]	 
                          [max-value (cadr (cadr x))]	 
                          [parent gui_parent]	 
                          [callback callproc]	 
                          [init-value (caddr x)])
                     (cadddr x)))
         slider_list))
  
  ;; Color sliders list. 
  (define sliders-lst (list '("R" (0 255) 0 'red-s)
                            '("G" (0 255) 0 'green-s)
                            '("B" (0 255) 0 'blue-s)
                            '("O" (0 10) 10 'alpha-s) ))
  ;; Generated color slider list
  ;; of (slider-obj tag) pairs
  (define color-sliders (mk-sliders p-wnd-slider_pane
                                    sliders-lst
                                    color-callback))
  
  
  ;; General purpose make radio-box.
  ;; radio_list - (label select tag)
  ;  (define (mk-radios gui_parent radio_list callproc)
  ;    (map (λ(x) (list (new radio-box%	 
  ;                          [label (car x)]	 
  ;                          [selection (cadr x)]	 
  ;                          [parent gui_parent]	 
  ;                          [callback callproc]
  ;                     (caddr x))))
  ;         radio_list))
  
  ;; Radio-box list
  ;  (define radios-lst (list '("Stroke" 1 'stroke)
  ;                            '("Fill" 0 'fill)))
  ;
  ;  (define sf-radio-box (mk-radios p-wnd-slider_pane
  ;                                radios-lst
  ;                                sf-callback))
  
  (define sf-radio-box (new radio-box%	 
                            [label ""]
                            [choices (list "Stroke" "Fill")]
                            [selection 0]	 
                            [parent p-wnd-slider_pane]	 
                            [callback sf-callback]))
  ; ===========================================================
  
  
  
  ;; Main window pane
  ;(provide m-wnd_pane)
  (define m-wnd_pane (new horizontal-pane% [parent m-wnd]))
  
  
  ; Button constructor.
  ; Requires parent GUI, list of buttons, callback procedsure
  ; and optional common button dimentions in form of (cons width height)
  ; Each element of button list is a sublist of type '("label" 'button_id)
  ; Returns list of type '((button_object 'button_id) ...)
  (define (mk_buttons gui_parent button_list callproc [dim (cons #f #f)])
    (map (λ(x) (list (new button%
                          [parent gui_parent]
                          [label (car x)]
                          [min-width (car dim)]
                          [min-height (cdr dim)]
                          [callback callproc])
                     (cadr x)))
         button_list))
  
  
  ;;; Tool box
  
  ;; Tool-box pane
  ;(provide m-wnd-tool_pane)
  (define m-wnd-tool_pane (new vertical-pane%
                               [parent m-wnd_pane]
                               [min-width 50]	 
                               [min-height 600]	 
                               [stretchable-width #f]	 
                               [stretchable-height #f]))
  
  ;; Tool-box label
  (new message% [parent m-wnd-tool_pane]
       [label "Toolbar"])
  
  ;; Tool-box buttons
  (define btn_lst (list '("Line" line) '("Ellipse" ellipse)
                        '("Rectangle" rect)))
  
  (define tool-box-buttons (mk_buttons m-wnd-tool_pane
                                       btn_lst
                                       tool-callback
                                       (cons 50 40)))
  
  ;;; -----------------------------------------------------------
  
  
  ;;; Utility box
  
  ;; Utility pane
  ;(provide m-wnd-button_pane)
  (define m-wnd-util_pane (new horizontal-pane% [parent m-wnd]
                               [alignment (list 'left 'center)]))
  
  ;; Status msg
  (define status-msg "Selected: ")
  ;; Status bar
  (define status-bar (new message%
                          [label status-msg]
                          [parent m-wnd-util_pane]
                          [min-width 250]
                          [font (make-object font% 11 'default)]))
  ;; Set status
  (define (set-status msg)
    (send status-bar set-label msg))
  
  
  ;; Utility buttons
  (set! btn_lst (list '("Undo" undo)
                      '("Load" load)
                      '("Save" save)
                      '("-n-" n)
                      '("!!Clear!!" clear)))
  (define util-buttons (mk_buttons m-wnd-util_pane
                                   btn_lst
                                   util-callback))
  
  ;;; ----------------
  
  
  ;; Drawing canvas
  (define m-wnd-canvas '())
  
  ;; Bitmap for drawing, and bitmap-dc
  (define bmp (make-object bitmap% 1 1))
  (define bmp-dc (new bitmap-dc%
                      [bitmap bmp]))
  
  
  
  ; mk-gui utility procedures and variables
  ; ---------------------------
  
  ;; dispaly GUI
  (define (show-gui)
    (send m-wnd show #t)
    (send p-wnd show #t))
  
  ;; Resize bitmap to fit canvas
  (define (bmp-resize)
    (set! bmp (make-object bitmap%
                (send m-wnd-canvas get-width)
                (send m-wnd-canvas get-height)))
    (send bmp-dc
          set-bitmap bmp))
  
  ;; Set drawing canvas (also, make init run of
  ;; color-callback
  (define (set-canvas canvas)
    (set! m-wnd-canvas (new canvas
                            [parent m-wnd_pane]
                            [paint-callback p-callback]))
    (color-callback #f #f))
  
  ;; Set drawn shapes list to operate on
  (define svg '())
  (define (set-svg svg-obj)
    (set! svg svg-obj))
  
  ;; Resize SVG
  (define (svg-resize)
    ((svg 'set-wh) (send m-wnd-canvas get-width)
                   (send m-wnd-canvas get-height)))
  
  ;; Clear bitmap
  (define (clear-bmp)
    (send bmp-dc clear))
  
  ;; Force canvas redraw 
  (define (refresh-canvas)
    (send m-wnd-canvas refresh-now))
  
  ;; dispatch with accessor procedures
  (define (dispatch msg)
    (cond ((eq? msg 'show) (show-gui))
          
          ((eq? msg 'svg-resize) (svg-resize))
          
          ((eq? msg 'bmp-resize) (bmp-resize))
          ((eq? msg 'clear-bmp) (clear-bmp))
          ((eq? msg 'refresh-canvas) (refresh-canvas))
          
          ((eq? msg 'set-canvas) set-canvas)
          ((eq? msg 'set-svg) set-svg)
          ((eq? msg 'set-util) set-util)
          
          ((eq? msg 'get-bmp-dc) bmp-dc)
          ((eq? msg 'get-current-tool) current-tool)
          ((eq? msg 'get-current-util) current-util)
          ((eq? msg 'get-util-btns) util-buttons)
          ((eq? msg 'get-tool-btns) tool-box-buttons)
          ((eq? msg 'get-pen) c-pen)
          ((eq? msg 'get-brush) c-brush)
          (else (display "No such thing in here"))))
  dispatch)

