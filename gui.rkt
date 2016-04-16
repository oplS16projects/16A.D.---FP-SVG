#lang racket
(require racket/gui/base)
(require racket/draw)

;;;;; GUI


;; Main window constructor
;(provide m-wnd)
(provide mk-gui)
(define (mk-gui)

  ; GUI Callbacks
  ; --------------
  
  ; Canvas callback
  (define p-callback
    (λ (canvas dc)
      (send dc
            draw-bitmap
            bmp
            0 0)))

  ; Selected util
  (define current-util '())
  ; Common util-box buttons callback
  (define util-callback
    (λ (obj event)
      (set! current-util 
            (cadr (assq obj util-buttons)))))

  
  ; Selected tool
  (define current-tool '())
  ; Common tool-box buttons callback
  (define tool-callback
    (λ (obj event)
      (set! current-tool 
            (cadr (assq obj tool-box-buttons))))) ;search tool buttons list for obj, get obj's id


  ; GUI elements
  ; ------------

  ;; Main window frame
  (define m-wnd (new frame%
                     [label "SVG-Edit"]
                     [x 0]
                     [y 0]
                     [min-width 800]
                     [min-height 600]
                     [stretchable-width #f]	 
                     [stretchable-height #f]))

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
  (define btn_lst (list '("Line" line) '("Circle" circle)
                        '("--n--" nothing) '("--n--" nothing)
                         '("--n--" nothing) '("--n--" nothing)
                         '("--n--" nothing) '("--n--" nothing)
                         '("--n--" nothing) '("--n--" nothing)
                         '("--n--" nothing)))
  
  (define tool-box-buttons (mk_buttons m-wnd-tool_pane
                                       btn_lst
                                       tool-callback
                                       (cons 50 40)))
                
  ;;; -----------------------------------------------------------


  ;;; Utility box

  ;; Utility pane
  ;(provide m-wnd-button_pane)
  (define m-wnd-util_pane (new horizontal-pane% [parent m-wnd]
                               [alignment (list 'center 'center)]))

  ;; Utility buttons
  (set! btn_lst (list '("?" nothing)
                      '("Clear" clear)
                      '("Save" save)
                      '("?" nothing)))
  (define util-buttons (mk_buttons m-wnd-util_pane
                                   btn_lst
                                   util-callback))
  
  ;;; ----------------
  

  ;; Drawing canvas
  (define m-wnd-canvas '())

  ;; Bitmap for drawing, and bitmap-dc
  (define bmp '())
  (define bmp-dc (new bitmap-dc% [bitmap #f]))
 


; mk-gui utility procedures and variables
; ---------------------------

  ;; dispaly GUI
  (define (show-gui) (send m-wnd show #t))

  ;; Resize bitmap to fit canvas
  (define (bmp-resize)
    (set! bmp (make-object bitmap%
                (send m-wnd-canvas get-width)
                (send m-wnd-canvas get-height)))
    (send bmp-dc
          set-bitmap bmp))

  ;; Set drawing canvas
  (define (set-canvas canvas)
    (set! m-wnd-canvas (new canvas
                            [parent m-wnd_pane]
                            [paint-callback p-callback])))

  ;; Clear bitmap
  (define (clear-bmp)
    (send bmp-dc clear))

  ;; Force canvas redraw 
  (define (refresh-canvas)
    (send m-wnd-canvas refresh-now))

  ;; dispatch with accessor procedures
  (define (dispatch msg)
    (cond ((eq? msg 'show) (show-gui))

          ((eq? msg 'bmp-resize) (bmp-resize))
          ((eq? msg 'clear-bmp) (clear-bmp))
          ((eq? msg 'refresh-canvas) (refresh-canvas))

          ((eq? msg 'set-canvas) set-canvas)

          ((eq? msg 'get-bmp-dc) bmp-dc)
          ((eq? msg 'get-current-tool) current-tool)
          ((eq? msg 'get-current-util) current-util)
          ((eq? msg 'get-util-btns) util-buttons)
          ((eq? msg 'get-tool-btns) tool-box-buttons)
          (else (display "No such thing in here"))))
  dispatch)
          
