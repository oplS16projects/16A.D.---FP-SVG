;Dmitri Kheifets
;Adam Contardo

#lang racket
(require racket/gui/base)
(require racket/draw)

(require "./events.rkt")


;;;;; GUI
;
; Naming convention for GUI elements:
;              m-wnd-"type"-"context"



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
(provide m-wnd-tool-pane)
(define m-wnd-tool-pane (new vertical-pane%
                             [parent m-wnd-pane]
                             [min-width 50]	 
                             [min-height 600]	 
                             [stretchable-width #f]	 
                             [stretchable-height #f]))

;;Tool pane buttons
(new message% [parent m-wnd-tool-pane]
     [label "Toolbar"])

(new button% [parent m-wnd-tool-pane]
     [label "--?--"]
     [min-width 50]
     [min-height 40]
     [callback tool-call0])

(new button% [parent m-wnd-tool-pane]
     [label "--?--"]
     [min-width 50]
     [min-height 40]
     [callback tool-call0])

(new button% [parent m-wnd-tool-pane]
     [label "--?--"]
     [min-width 50]
     [min-height 40]
     [callback tool-call0])

(new button% [parent m-wnd-tool-pane]
     [label "--?--"]
     [min-width 50]
     [min-height 40]
     [callback tool-call0])

(new button% [parent m-wnd-tool-pane]
     [label "--?--"]
     [min-width 50]
     [min-height 40]
     [callback tool-call0])

(new button% [parent m-wnd-tool-pane]
     [label "--?--"]
     [min-width 50]
     [min-height 40]
     [callback tool-call0])

(new button% [parent m-wnd-tool-pane]
     [label "--?--"]
     [min-width 50]
     [min-height 40]
     [callback tool-call0])

(new button% [parent m-wnd-tool-pane]
     [label "--?--"]
     [min-width 50]
     [min-height 40]
     [callback tool-call0])

(new button% [parent m-wnd-tool-pane]
     [label "--?--"]
     [min-width 50]
     [min-height 40]
     [callback tool-call0])

(new button% [parent m-wnd-tool-pane]
     [label "--?--"]
     [min-width 50]
     [min-height 40]
     [callback tool-call0])

(new button% [parent m-wnd-tool-pane]
     [label "--?--"]
     [min-width 50]
     [min-height 40]
     [callback tool-call0])

(new button% [parent m-wnd-tool-pane]
     [label "--?--"]
     [min-width 50]
     [min-height 40]
     [callback tool-call0])

(new button% [parent m-wnd-tool-pane]
     [label "--?--"]
     [min-width 50]
     [min-height 40]
     [callback tool-call0])

;;;Short Info Pane (nothing major, just program instructions? feedback?)
;;(new message% [parent m-wnd]
;;     [label "Information Panel\n"])

; Button pane
(provide button-pane)
(define button-pane (new horizontal-pane% [parent m-wnd]
                         [alignment (list 'center 'center)]))

(new button% [parent button-pane]
     [label "?"]
     [callback button-call0])
(new button% [parent button-pane]
     [label "Clear"]
     [callback button-call0])
(new button% [parent button-pane]
     [label "Save"]
     [callback button-call0])
(new button% [parent button-pane]
     [label "Test Color"]
     [callback button-call0])
(new button% [parent button-pane]
     [label "?"]
     [callback button-call0])


(send m-wnd show #t)
