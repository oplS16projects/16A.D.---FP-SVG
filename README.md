# Project Title: SVG Editor

  [**Jump to the progress**](https://github.com/oplS16projects/SkraM---FP-SVG/blob/master/README.md#progress)

### Statement
The goal of this project is to implement XML-based Scalable Vector Graphics specification on a limited basis and to provide means to edit svg documents graphically.

### Analysis
The project will make use of several concepts.

* **Recursion**
  * Recursive HOPs, such as map and filter, will be used to retrieve data from the lists populated with "element" objects.
 
* **Object-orientation**
  * Objects will be used to store individual "elements" parameters.

### Deliverable and Demonstration
The deliverable will be a GUI application with minimalistic toolset and a canvas, where primitive objects, such as lines, squares and circles will be placed. The application will allow to store canvas drawing in form of SVG document, and be able to render SVG documents
created with it (due to limited nature of implementation, most SVG documents created with other editors will be only partially compatible or incompatible at all). There will be an option available to modify each element on the screen individually via properties dialog.

### Evaluation of Results
If an application will be able to store and render the same SVG document without distortions/errors, this would indicate a scuccess.

## Architecture Diagram
![diagram][archdiagram]

## Schedule

### Progress
 Goals for the second milestone are met partially. Currently, the framework of application is almost complete, basic functionality is
 in place - drawing shapes, exporting image to SVG.
 
 **Sample SVG output generated with SkraM:**
  ![sample][sample-out]
 
### First Milestone (Fri Apr 15)
* Semi-complete GUI frontend.
 * **##done** 
* Ability to draw at least one shape 
  and disaplay properties of drawn objects. 
 * **##partial - drawn shapes' properties aren't viewable yet**
* Export image to SVG. 
 * **##not implemented yet**

### Second Milestone (Fri Apr 22)
* Complete GUI frontend **##partial**
* Import from SVG. **##not implemented yet**
* Export image to SVG **##complete**
* Ability to modify properties of individual
  shapes placed on canvas. **##not implemented yet**
* More shapes added.

### Final Presentation (last week of semester)
* Refining the code/documentation, 
* Expanding on / adding functionality - if time permits.

## Group Responsibilities

### Dmitri Kheifets @tetra-d
Will be working on implementation of SVG document structure and it's elements, import/export of SVG document, event handling and overall application architecture.

### Ian Roberts @ia-n
Ian Roberts will be working on adding the shape components as well as implementing the event handling using respective libraries.
<!-- Links -->
[archdiagram]: ./archchart.png
[sample-out]: https://cdn.rawgit.com/oplS16projects/SkraM---FP-SVG/master/test1.svg
