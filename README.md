# SkraM SVG Editor

##Authors
Dmitri Kheifets

Ian Roberts

##Overview
A simple drawing environment with a focus on exporting generated images to an XML-based Scalable Vector Graphics document. The editor allows to draw basic shapes, export drawing to and import drawings from SkraM-compatible SVG documents.

##Screenshot
![screenshot][skram.png]

##Concepts Demonstrated

* **Data abstraction** is used extensively to describe appropriate shape elements when drawing, exporting to or importing from SVG document.
* Generated shape elements are stored as **objects** for internal representation.
* Core components organized as objects (SVG processing, GUI, drawing) intercommunicate by means of **message passing**.
* **HOPs** are used to run through various lists. E.g. list of shape objects is mapped to generate XML content, or list of string labels and tags is mapped to generate lists of GUI controls.


##External Technology and Libraries
Libraries used:
* racket xml - for import/export of SVG documents. Imported files are read into XML "document" structure and converted to lists of elements with sublists of attributes (x-expressions). For export, constructed x-expressions are converted back to XML "document".
*racket gui - general purpose UI elements, bitmap functionality for drawing, canvas class for rendering bitmaps.

##Favorite Scheme Expressions
####Dmitri's
```racket
        (define (get-attr attr-lst element)
          (map (λ (x)
                 (cadar (filter
                         (λ(e)(eq? (car e) x))
                         element)))
               attr-lst))
```
This procedure, an example of functional programming, maps a list of predefined attributes of type '(a b c d ...) to the attribute values of element in x-expr '((a data) (b data) (c data) (d data) ...)




#How to Download and Run
Latest release can be found at [https://github.com/oplS16projects/SkraM---FP-SVG/releases][release_link]
Current Master branch [https://github.com/oplS16projects/SkraM---FP-SVG/archive/master.zip][master_branch]

##Instructions
Start application with main.rkt

Main Window
-
Left Toolbar:
* select shapes to draw

Bottom Toolbar:
* Undo - remove drawn shapes in reverse one by one.
* Load - Import SkraM compatible SVG document.
* Save - Export SVG document.
* -n-  - not implemented
* !!Clear!! - clear the canvas entirely. Asks for confirmation.

Color Window
-
* Ellipse is for color reference
  (doesn't represent current shape)
	* Frame displays stroke color
	* Area displays fill color

* Set the color by selecting *stroke* or *fill* radio buttons,
	and adjusting R, G and B sliders.

<!-- Links -->

[skram.png]: ./Screenshot/SkraM.png
[release_link]: https://github.com/oplS16projects/SkraM---FP-SVG/releases
[master_branch]: https://github.com/oplS16projects/SkraM---FP-SVG/archive/master.zip
