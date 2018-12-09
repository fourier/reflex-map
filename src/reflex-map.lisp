#|
  This file is a part of reflex-map project.

  (C) COPYRIGHT Alexey Veretennikov<alexey.veretennikov@gmail.com>, 2018
|#

(in-package :cl-user)
(defpackage reflex-map
  (:use :cl :alexandria :split-sequence)
  (:export main))

(in-package :reflex-map)


(define-condition parse-error (error)
  ((text :initarg :text))
  (:report (lambda (condition stream)
             (format stream "Parse file error: ~a" (slot-value condition 'text)))))

(define-condition parse-header-error (parse-error) nil)

(define-condition parse-entity-error (parse-error) nil)

(define-condition parse-brush-error (parse-error) nil)

(define-condition parse-vertices-error (parse-brush-error) nil)

(define-condition parse-face-error (parse-brush-error) nil)


(defclass reflex-map ()
  ((entities :initform nil :reader map-entities)
   (brushes :initform nil :reader map-brushes)))
    

(defclass entity ()
  ((type :initarg :type :initform nil)
   (properties :initarg :properties :initform nil))
  (:documentation "Entity section representation"))


(defclass brush ()
  ((vertices :initform nil :reader brush-vertices
             :documentation "A list of vertices")
   (faces :initform nil :reader brush-faces
          :documentation "A list of faces")))


(defclass vertex ()
  ((x :initarg :x :initform 0 :type 'float :reader vertex-x)
   (y :initarg :y :initform 0 :type 'float :reader vertex-y)
   (z :initarg :z :initform 0 :type 'float :reader vertex-z))
  (:documentation "Representation of the 3d vertex"))


(defclass face ()
  ((u :initarg :u :initform 0 :type 'float :reader face-u)
   (v :initarg :v :initform 0 :type 'float :reader face-v)
   (scale-u :initarg :scale-u :initform 1 :type 'float :reader face-scale-u)
   (scale-v :initarg :scale-v :initform 1 :type 'float :reader face-scale-v)
   (rotation :initarg :rotation :initform 0 :type 'float :reader face-rotation)
   (vertices :initarg :vertices :initform nil :reader face-vertices :documentation "an array of indexes of corresponding ver~tices")
   (texture-name :initarg :texture-name :initform "" :type 'string :reader face-texture-name)))


(defmethod print-object :after ((self entity) stream)
  "Print the contents of the ENTITY"
  (with-slots (type properties) self
    (format stream "Entity type: ~a~%Properties:~%" type)
    (dolist (x properties)
      (format stream "~a~%" x))))

(defmethod print-vertex ((self vertex) stream)
  "Print the contents of the VERTEX"
  (with-slots (x y z) self
    (format stream "( ~a ~a ~a )" x y z)))

(defmethod print-object :after ((self vertex) stream)
  "Print the contents of the VERTEX"
  (print-vertex self stream))

(defmethod print-object :after ((self brush) stream)
  "Print the contents of the BRUSH"
  (with-slots (vertices faces) self
    (format stream "Bursh~%Vertices:~%")
    (dolist (v vertices) (print-object v stream))
    (format stream "Faces:~%")
    (dolist (f faces) (print-object f stream))))


(defun read-windows-line (in)
  (when-let (line (read-line in nil))
    (string-right-trim '(#\Newline #\Return) line)))


(defun parse-header-line (in-stream)
  (when-let (line (read-windows-line in-stream))
    (destructuring-bind (game map version number)
        (split-sequence #\Space line)
      (and (string= game "reflex")
           (string= map "map")
           (string= version "version")
           (= (parse-integer number) 6)))))


(defmacro def-if-header (header)
  (let ((fun-name (concatenate 'string "if-" header)))
    `(defun ,(intern (string-upcase fun-name)) (line)
       (string=
        (string-trim '(#\Tab #\Space) line)
        ,header))))


(def-if-header "brush")
(def-if-header "entity")
(def-if-header "vertices")
(def-if-header "faces")


(defun if-indent (line count)
  (starts-with-subseq (make-list count :initial-element #\Tab)
                      line))


(defun parse-and-trim (line)
  (split-sequence:split-sequence #\Space (string-trim '(#\Tab #\Space) line)))


(defmethod parse-entity ((self entity) in)
  (with-slots (properties) self
    ;; first line must be a type
    (let ((first-line (read-windows-line in)))
      (unless (if-indent first-line 1)
        (error 'parse-entity-error :text "First entity line is not indented with 1 tab"))
      (let ((parsed-first-line (parse-and-trim first-line)))
        (unless (string= (car parsed-first-line) "type")
          (error 'parse-entity-error :text "First entity line is not a 'type'"))
        (setf (slot-value self 'type) (second parsed-first-line))))
    (loop for line = (read-windows-line in )
          while (if-indent line 1)
          do 
             (push (parse-and-trim line) properties)
          finally
             (setf properties (nreverse properties))
             (return line))))


(defun parse-vertex (line)
  (destructuring-bind (x y z)
      (mapcar #'read-from-string (parse-and-trim line))
    (make-instance 'vertex :x x :y y :z z)))

(defun parse-face (line)
  ;; example:
  ;; 0.000000 0.000000 1.000000 1.000000 0.000000 0 1 2 3 structural/dev/dev_nogrid_grey128
  ;; faces format:
  ;; offset u v, scale u v, rotation
  (let* ((parsed (parse-and-trim line))
         (texture-offsets (subseq parsed 0 5))
         (texture-name (car (last parsed)))
         (vertices (subseq parsed 5 (1- (length parsed)))))
    (destructuring-bind (u v s-u s-v rot)
        (mapcar #'read-from-string texture-offsets )
      (make-instance 'face
                     :u u :v v :scale-u s-u :scale-v s-v
                     :rotation rot
                     :texture-name texture-name
                     :vertices
                     (mapcar #'parse-integer
                             vertices)))))
                   
    


(defmethod parse-brush ((self brush) in)
  (with-slots (vertices faces) self
    (let ((line
            (when (if-vertices (read-windows-line in))
              (loop for line = (read-windows-line in )
                    while (if-indent line 2)
                    do
                       (push (parse-vertex line) vertices)
                    finally
                       (setf vertices (nreverse vertices))
                       (return line)))))
      (when (if-faces line)
        (loop for line = (read-windows-line in )
              while (if-indent line 2)
              do
                 (push (parse-face line) faces)
              finally
                 (setf faces (nreverse faces))
                 (return line))))))

  

(defmethod parse-next-object ((self reflex-map) line in)
  (with-slots (entities brushes) self
    (cond ((if-entity line)
           (let* ((e (make-instance 'entity))
                  (next-line (parse-entity e in)))
             (push e entities)
             next-line))
          ((if-brush line)
           (let* ((b (make-instance 'brush))
                  (next-line (parse-brush b in)))
             (push b brushes)
             next-line))
          (t (error 'parse-error :text "Unknown field")))))

            

(defun parse-reflex-map-file (filename)
  (with-open-file (in filename :if-does-not-exist nil)
    (when (parse-header-line in)
      (let ((m (make-instance 'reflex-map)))
        (loop while (listen in)
              with line = (read-windows-line in)
              do
                 (setf line (parse-next-object m line in)))
        (with-slots (brushes entities) m
          (setf brushes (nreverse brushes)
                entities (nreverse entities)))
          m))))


(defmethod export-brush ((self brush) out)
  (with-slots (vertices faces) self
    (format out "{~%")
    (dolist (f faces)
      (mapcar 
       (lambda (i)
         (print-vertex (elt vertices i) out)
         (format out " "))
       (subseq (face-vertices f) 0 3))
      (format out " rock4_1 0 0 0 1 1~%"))
    (format out "}~%")))

(defmethod create-qw-map-file ((self reflex-map) filename)
  (with-open-file (out filename :direction :output :if-exists :supersede)
    (format out "// Game: Quake
// Format: Standard
// entity 0
{
\"classname\" \"worldspawn\"
\"wad\" \"C:/q1mapping/wads/START.WAD\"~%")
    ;; implement this
    (with-slots (brushes) self
      (loop for br in brushes
            for i below (length brushes)
            do
               (format out "// brush ~d~%" i)
               (export-brush br out)))
    (format out "}~%")))


(defun main(argv)
  (declare (ignore argv))
  (format t "Hello world~%"))

;; Implementation
