;Dmitri Kheifets
;Adam Contardo

#lang racket
(require racket/gui/base)
(require racket/draw)

(require "./events.rkt")


;;;;; GUI
;
; Naming convention for GUI elements:
;
;        m-wnd-"type"_"function"-"context"
;        m - window order, e.g. main, toolset, etc.
;        "type"_"function" - type of object placed in the window and it's function
;        "context" - e.g. dc or some other derived type



;; Main window
(provide m-wnd)
(define m-wnd (new frame%
                   [label "SVG-Edit"]
                   [x 0]
                   [y 0]
                   [width 800]
                   [height 600]))

;; Main window pane
(provide m-wnd-pane)
(define m-wnd-pane (new horizontal-pane% [parent m-wnd]))



;; Tool pane
(provide m-wnd-pane_tool)
(define m-wnd-pane_tool (new vertical-pane%
                             [parent m-wnd-pane]
                             [min-width 50]	 
                             [min-height 600]	 
                             [stretchable-width #f]	 
                             [stretchable-height #f]))

;;Tool pane buttons
(new message% [parent m-wnd-pane_tool]
     [label "Toolbar"])

(new button% [parent m-wnd-pane_tool]
     [label "--?--"]
     [min-width 50]
     [min-height 40]
     [callback tool-call0])

(new button% [parent m-wnd-pane_tool]
     [label "--?--"]
     [min-width 50]
     [min-height 40]
     [callback tool-call0])

(new button% [parent m-wnd-pane_tool]
     [label "--?--"]
     [min-width 50]
     [min-height 40]
     [callback tool-call0])

(new button% [parent m-wnd-pane_tool]
     [label "--?--"]
     [min-width 50]
     [min-height 40]
     [callback tool-call0])

(new button% [parent m-wnd-pane_tool]
     [label "--?--"]
     [min-width 50]
     [min-height 40]
     [callback tool-call0])

(new button% [parent m-wnd-pane_tool]
     [label "--?--"]
     [min-width 50]
     [min-height 40]
     [callback tool-call0])

(new button% [parent m-wnd-pane_tool]
     [label "--?--"]
     [min-width 50]
     [min-height 40]
     [callback tool-call0])

(new button% [parent m-wnd-pane_tool]
     [label "--?--"]
     [min-width 50]
     [min-height 40]
     [callback tool-call0])

(new button% [parent m-wnd-pane_tool]
     [label "--?--"]
     [min-width 50]
     [min-height 40]
     [callback tool-call0])

(new button% [parent m-wnd-pane_tool]
     [label "--?--"]
     [min-width 50]
     [min-height 40]
     [callback tool-call0])

(new button% [parent m-wnd-pane_tool]
     [label "--?--"]
     [min-width 50]
     [min-height 40]
     [callback tool-call0])

(new button% [parent m-wnd-pane_tool]
     [label "--?--"]
     [min-width 50]
     [min-height 40]
     [callback tool-call0])

(new button% [parent m-wnd-pane_tool]
     [label "--?--"]
     [min-width 50]
     [min-height 40]
     [callback tool-call0])

;;;Short Info Pane (nothing major, just program instructions? feedback?)
;;(new message% [parent m-wnd]
;;     [label "Information Panel\n"])

; Button pane
(provide m-wnd-button_pane)
(define m-wnd-button_pane (new horizontal-pane% [parent m-wnd]
                         [alignment (list 'center 'center)]))

(new button% [parent m-wnd-button_pane]
     [label "?"]
     [callback button-call0])
(new button% [parent m-wnd-button_pane]
     [label "Clear"]
     [callback button-call0])
(new button% [parent m-wnd-button_pane]
     [label "Save"]
     [callback button-call0])
(new button% [parent m-wnd-button_pane]
     [label "Test Color"]
     [callback button-call0])
(new button% [parent m-wnd-button_pane]
     [label "?"]
     [callback button-call0])


(send m-wnd show #t)