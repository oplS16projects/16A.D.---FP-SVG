# Project Title: SVG Editor

### Statement
The goal of this project is to implement XML-based Scalable Vector Graphics specification on a limited basis and to provide means to edit svg documents graphically.

### Analysis
The project will make use of several concepts.

* **Recursion**
  * Recursive HOPs, such as map and filter, will be used to retrieve data from the lists populated with "elements" displayed on the     screen.
* **State modification and object-orientation**
  * Objects will be used to store individual "elements" data and properties. Each object's state will be modified to adjust/update element's properties.

### Deliverable and Demonstration
The deliverable will be a GUI application with minimalistic toolset and a canvas, where primitive objects, such as lines, squares and circles will be placed. The application will allow to store canvas drawing in form of SVG document, and be able to render SVG documents
created with it (due to limited nature of implementation, most SVG documents created with other editors will be only partially compatible or incompatible at all). There will be an option available to modify each element on the screen individually via properties dialog.

### Evaluation of Results
If an application will be able to store and render the same SVG document without distortions/errors, this would indicate a scuccess.

## Architecture Diagram
![diagram][archdiagram]

## Schedule

### First Milestone (Fri Apr 15)
* Semi-complete GUI frontend.
* Ability to draw at least one shape
  and disaplay properties of drawn objects.
* Export image to SVG.

### Second Milestone (Fri Apr 22)
* Complete GUI frontend
* Import from SVG.
* Ability to modify properties of individual
  shapes placed on canvas.
* More shapes added.

### Final Presentation (last week of semester)
* Refining the code/documentation,
* Expanding on / adding functionality - if time permits.

## Group Responsibilities

### Dmitri Kheifets @tetra-d
Will be working on implementation of SVG document structure and it's elements, import/export of SVG document and event handling.

### Ian Roberts @ia-n
Ian Roberts will be working on adding the shape components as well as implementing the event handling using respective libraries.
<!-- Links -->
[archdiagram]: ./archchart.png
