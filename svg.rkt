#lang racket
(require xml)
(require racket/gui)

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
;;      'clear - remove all eleements from elements-list
;;      'save - export SVG document
;;      'load - import SVG document


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

    ;; Clear elements list
    (define (remove-all)
      (set! elements-list '()))

    ;;;; Helper functions --------
    
    ;; string->number shortcut
    (define str->n string->number)
    
    ;; shortcut for number->string
    (define num->s number->string)
    
    ;; string-append shortcut
    (define str-ap string-append)

    ;; convert list of string to
    ;; list of numbers
    (define (slst->nlst slst)
      (map (λ(x)(str->n x)) slst))
    ;;;; ========================
    
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
    ;; ============================================================
    
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
    ; SVG Import Section

    ; Parse xexpr and return list of elements
    (define (read-elements xexpr-l)
      (let ((elements-list '())
            (line-attr
             '(x1 y1 x2 y2))
            (ellipse-attr
             '(cx cy rx ry))
            (brush-attr
             '(fill fill-opacity))
            (pen-attr
             '(stroke
               stroke-opacity
               stroke-width)))
        
        (define (mk-color rgb-str opacity)
          (let ((color-lst (string-split
                            (string-replace
                             (string-replace
                              (string-replace
                               rgb-str "rgb(" "")
                              ")" "")
                             "," " "))))
            (begin
              (set! color-lst (append
                               (map (λ(x)(str->n x))
                                    color-lst)
                               (list (str->n opacity))))
              (make-object
                  color%
                (car color-lst) ;r
                (cadr color-lst) ;g
                (caddr color-lst) ;b
                (cadddr color-lst))))) ;alpha
        
        (define (mk-pen element)
          (let ((f-pen-attr (get-attr
                             pen-attr
                             element)))
            (new pen%
                 [color (mk-color (car f-pen-attr)
                                  (cadr f-pen-attr))]
                 [width (str->n (caddr f-pen-attr))])))
        
        (define (mk-brush element)
          (let ((f-brush-attr (get-attr
                               brush-attr
                               element)))
            (new brush%
                 [color (mk-color (car f-brush-attr)
                                  (cadr f-brush-attr))])))
        
        (define (get-attr attr-lst element)
          (map (λ (x)
                 (cadar (filter
                         (λ(e)(eq? (car e) x))
                         element)))
               attr-lst))
        
        ;; shape specific procs for
        ;  internal representation
        (define (mk-line-in type-attr element)
          (begin
            (define coords (slst->nlst
                            (get-attr type-attr element)))
            (define pen (mk-pen element))
            (define brush (mk-brush element))
            
            (list coords
                  (list pen brush))))
        
        (define (mk-ellipse-in type-attr element)
          (begin
            (define coords (slst->nlst
                            (get-attr type-attr element)))
            (define pen (mk-pen element))
            (define brush (mk-brush element))
            
            (set! coords (list (- (car coords) (caddr coords))
                               (- (cadr coords) (cadddr coords))
                               (+ (car coords) (caddr coords))
                               (+ (cadr coords) (cadddr coords))))
            (list coords
                  (list pen brush))))
        
        ; iterate through list of elements
        (define (iter-e-lst e-lst)
          (define wrk-type '())
          (cond ((not (null? e-lst))
                 (begin
                   (set! wrk-type (caar e-lst))
                   (cond ((eq? wrk-type 'line)
                          (set! elements-list
                                (append elements-list
                                        (list (make-element
                                               wrk-type
                                               (mk-line-in
                                                line-attr (cadar e-lst)))))))
                         ((eq? wrk-type 'ellipse)
                          (set! elements-list
                                (append elements-list
                                        (list (make-element
                                               wrk-type
                                               (mk-ellipse-in
                                                ellipse-attr (cadar e-lst))))))))
                   (iter-e-lst (cdr e-lst))))))
        
        (iter-e-lst (cddr xexpr-l))
        elements-list)) ;return elements-list
    ; ===========================================================================
    
    ;; Load SVG
    (define (load-svg path)
      (define in (open-input-file path))
      (permissive-xexprs #t)
      (define xml-body (document-element (read-xml in)))
      (set! elements-list
            (read-elements (xml->xexpr xml-body)))
      (close-input-port in)
      (port-closed? in))
    ; =====================================================================
    ;; Dispatch
    (define (dispatch msg)
      (cond ((eq? msg 'add-shape) add-shape)

            ((eq? msg 'get-e-list) elements-list)

            ((eq? msg 'set-wh) set-wh)

            ((eq? msg 'clear) (remove-all))
            ((eq? msg 'save) save-svg)
            ((eq? msg 'load) load-svg)
            ((eq? msg 'remove-last) (remove-last))))
    dispatch)) ;(define (let