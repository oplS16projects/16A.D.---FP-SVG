#lang racket
(require xml)

;;; Module to store list of elements placed on canvas,
;;; and for export/import of SVG documents. Used in "main.rkt".
;;
;; Dispatch Messages (work in progress):
;;
;;      'add-shape - add shape element to the list of elements
;;
;;      'get-e-list - retrieve list of elements
;;
;;      'set-wh - set width/height property of SVG document
;;
;;      'save - export SVG document


;;processing insruction
(define proc-inst (p-i 'racket
                       'racket
                       'xml
                       "version='1.0' standalone='no'"))

;prolog with SVG doc type declaration.
(define svg-prolog (prolog (list proc-inst)
                     (document-type
                      'svg
                      (external-dtd/public
                       "-//W3C//DTD SVG 1.1//EN"
                       "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd")
                      #f)
                     '()))

(provide svg)
(define (svg)
  (let ((elements-list '())
        (svg-width 0)
        (svg-height 0))

    ;; Set svg width/height
    (define (set-wh width height)
      (set! svg-width width)
      (set! svg-height height))
    
    ;; Add shape to the elements-list
    (define (add-shape type param)
      (if (not (eq? type 'n))
          (set! elements-list
            (append elements-list
                    (list (make-element type param))))
          (writeln "add-shape: empty type")))

    ;; Remove last shape from the list
    (define (remove-last)
      (cond ((not (null? elements-list))
             (set! elements-list (reverse
                                  (cdr
                                   (reverse
                                    elements-list)))))))

    ;; shortcut for number->string
    (define num->s number->string)

    ;;shortcut for string-append
    (define str-ap string-append)

    ;; Make rgb(R, G, B) string
    (define (rgb-string color)
      (str-ap
       "rgb("
       (num->s (send color red)) ","
       (num->s (send color green)) ","
       (num->s (send color blue)) ")"
       ))
    
    ;; Element object constructor
    ;; Takes "type" and "coords" of element
    (define (make-element type param)
      (let ((t type)
            (p param)
            (pen-color-obj (send (caadr param) get-color))
            (pen-stroke-w (send (caadr param) get-width))
            (brush-color-obj (send (car (cdadr param)) get-color)))
        (define (dispatch msg)
          (cond ((eq? msg 'get-param) p)
                ((eq? msg 'get-type) t)
                ((eq? msg 'get-coords) (car p))
                ((eq? msg 'get-stroke-color) pen-color-obj)
                ((eq? msg 'get-stroke-op) (send pen-color-obj alpha))
                ((eq? msg 'get-stroke-w) pen-stroke-w)
                ((eq? msg 'get-fill-color) brush-color-obj)
                ((eq? msg 'get-fill-op) (send brush-color-obj alpha))))
        dispatch))

    ;; ----------------------------------------------------------
    ;; svg-xml shape types
    (define (misc-attr element)
      (let ((stroke (rgb-string (element 'get-stroke-color)))
            (stroke-w (element 'get-stroke-w))
            (stroke-op (element 'get-stroke-op))
            (fill (rgb-string (element 'get-fill-color)))
            (fill-op (element 'get-fill-op)))
        (list (list 'stroke stroke)
              (list 'stroke-width (num->s stroke-w))
              (list 'stroke-opacity (num->s stroke-op))
              (list 'fill fill)
              (list 'fill-opacity (num->s fill-op)))))
    
    ;of type cx="50" cy="50" r="40"
    (define (mk-ellipse element)
      (let ((coords (car (element 'get-param))))
        (let ((cx (car coords))
              (cy (cadr coords))
              (rx (/ (- (caddr coords) (car coords)) 2.0))
              (ry (/ (- (cadddr coords) (cadr coords)) 2.0)))
          (list 'ellipse (append (list (list 'cx (num->s (+ cx rx)))
                                       (list 'cy (num->s (+ cy ry)))
                                       (list 'rx (num->s (abs rx)))
                                       (list 'ry (num->s (abs ry))))
                                 (misc-attr element))))))

    ;of type x1="0" y1="0" x2="200" y2="200"
    (define (mk-line element)
      (let ((coords (car (element 'get-param))))
        (let ((x1 (car coords))
              (y1 (cadr coords))
              (x2 (caddr coords))
              (y2 (cadddr coords)))
          (list 'line (append (list (list 'x1 (num->s x1))
                                    (list 'y1 (num->s y1))
                                    (list 'x2 (num->s x2))
                                    (list 'y2 (num->s y2)))
                              (misc-attr element))))))
    ;; ----------------------------------------------------------
    
    ; svg-body
    (define (mk-svg-body body-elements)
      (append (list 'svg
                    (list (list 'xmlns "http://www.w3.org/2000/svg")
                          (list 'version "1.1")
                          (list 'width (string-append (num->s svg-width) "px"))
                          (list 'height (string-append (num->s svg-height) "px"))))
;                          (list 'viewbox (string-append "0 0 "
;                                                        (num->s svg-width)
;                                                        " "
;                                                        (num->s svg-height)))))
              body-elements))
    
    ; generate xml body elements list
    (define (mk-body-elements)
      (map (λ(element) (cond ((eq? (element 'get-type) 'line)
                              (mk-line element))
                             ((eq? (element 'get-type) 'ellipse)
                              (mk-ellipse element)))) elements-list))
              
       ; xml-document
    (define (mk-svg-doc) (document
                          svg-prolog ;doc prolog
                          (xexpr->xml (mk-svg-body (mk-body-elements))) ;doc body. xexpr to xml.
                          '())) ;list of misc items
    
    ; Save svg
    (define (save-svg path)
      (define out (open-output-file path #:exists 'replace))
      (write-xml (mk-svg-doc) out)
      (close-output-port out))
       


    ; -------------------------------------------------------   
    ;; Dispatch
    (define (dispatch msg)
      (cond ((eq? msg 'add-shape) add-shape)

            ((eq? msg 'get-e-list) elements-list)

            ((eq? msg 'set-wh) set-wh)
            
            ((eq? msg 'save) save-svg)
            ((eq? msg 'remove-last) (remove-last))))
    dispatch)) ;(define (let