<<<<<<< HEAD
# SkraM SVG Editor

##Authors
Dmitri Kheifets

Ian Roberts

##Overview
A simple drawing environment with a focus on exporting generated images to an XML-based Scalable Vector Graphics document. The editor allows to draw basic shapes, export drawing to and import drawings from SkraM-compatible SVG documents.

##Screenshot
![skram.png][screenshot]

Here's a demonstration of how to display an image that's uploaded to this repo:
![screenshot showing env diagram](withdraw.png)

##Concepts Demonstrated
Identify the OPL concepts demonstrated in your project. Be brief. A simple list and example is sufficient. 
* **Data abstraction** is used to provide access to the elements of the RSS feed.
* The objects in the OpenGL world are represented with **recursive data structures.**
* **Symbolic language processing techniques** are used in the parser.

##External Technology and Libraries
Briefly describe the existing technology you utilized, and how you used it. Provide a link to that technology(ies).

##Favorite Scheme Expressions
####Mark (a team member)
Each team member should identify a favorite expression or procedure, written by them, and explain what it does. Why is it your favorite? What OPL philosophy does it embody?
Remember code looks something like this:
```scheme
(map (lambda (x) (foldr compose functions)) data)
```
####Lillian (another team member)
This expression reads in a regular expression and elegantly matches it against a pre-existing hashmap....
```scheme
(let* ((expr (convert-to-regexp (read-line my-in-port)))
             (matches (flatten
                       (hash-map *words*
                                 (lambda (key value)
                                   (if (regexp-match expr key) key '()))))))
  matches)
```

##Additional Remarks
Anything else you want to say in your report. Can rename or remove this section.

#How to Download and Run
You may want to link to your latest release for easy downloading by people (such as Mark).

Include what file to run, what to do with that file, how to interact with the app when its running, etc. 

<!-- Links -->
[skram.png]: ./Screenshot/SkraM.png
=======
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
>>>>>>> master
