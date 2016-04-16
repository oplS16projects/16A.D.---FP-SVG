(let (( y1-coorid (cadr mouse-d-pos))
       (x1-coorid (car mouse-d-pos)))
(define x2-coorid ((λ () (send event get-x))))
(define y2-coorid ((λ () (send event get-y))))
(define y-run     ((λ () (abs (- y2-coorid  y1-coorid)))))
(define x-run     ((λ () (abs (- x2-coorid x1-coorid)))))
(define x-start   ((λ () (min x1-coorid x2-coorid))))
(define y-start   ((λ () (min y1-coorid y2-coorid))))
                             
(define make-perfect-shape ((λ ()  (max x-run y-run))))
  

                          (define line
(send canv-bitmap-dc draw-line
                   x-start
                  y-start
                   y2-coorid
                   x2-coorid))

  (define rectangle 

         
  (send canv-bitmap-dc draw-rectangle
                    x-start
                    y-start
                    x-run
                    y-run))
(define square  

         
  (send canv-bitmap-dc draw-rectangle
                    x-start
                   y-start


                   make-perfect-shape
                    make-perfect-shape))
  

  (define ellipse

         
  (send canv-bitmap-dc draw-ellipse
                    x-start
                    y-start
                   x-run
                    y-run))


  (define circle

         
  (send canv-bitmap-dc draw-ellipse
                 x-start
                 y-start
                   make-perfect-shape
                    make-perfect-shape
                    ))


  


(define (dispatch m )

  ( cond ((equal? m 'rectangle ) rectangle)
         ((equal? m 'ellipse ) ellipse)
        ((equal? m 'circle ) circle)
        ((equal? m 'line ) line)
        ((equal? m 'square ) square)
         (else (error "something bad happend" ))))

dispatch 

  )