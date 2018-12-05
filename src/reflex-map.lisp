#|
  This file is a part of reflex-map project.

  (C) COPYRIGHT Alexey Veretennikov<alexey.veretennikov@gmail.com>, 2018
|#

(in-package :cl-user)
(defpackage reflex-map
  (:use :cl :alexandria)
  (:export main))
(in-package :reflex-map)

;; faces format:
;; offset u v, scale u v, rotation

(defclass reflex-map ()
  ((entities :initform nil)
   (brushes :initform nil)))
    

(defclass entity ()
  ((type :initarg :type)
   (properties :initarg :properties))
  (:documentation "Entity section representation"))


(defclass brush ()
  ((vertices :initform nil
             :documentation "An array of vertices")
   (faces :initform nil
          :documentation "An array of faces")))


(defclass vertex ()
  ((x :initarg :x :initform 0 :type 'float :reader vertex-x)
   (y :initarg :y :initform 0 :type 'float :reader vertex-y)
   (z :initarg :z :initform 0 :type 'float :reader vertex-z))
  (:documentation "Representation of the 3d vertex"))


(defclass face ()
  ((u :initarg :u :initform 0 :type 'float :reader face-u)
   (v :initarg :v :initform 0 :type 'float :reader face-v)
   (scale-u :initarg :scale-u :initform 1 :type 'float :reader face-scale-u)
   (scale-v :initarg :scale-u :initform 1 :type 'float :reader face-scale-v)
   (rotation :initarg :rotation :initform 0 :type 'float :reader face-rotation)
   (vertices :initarg :vertices :initform nil :reader face-vertices :documentation "an array of indexes of corresponding ver~tices")
   (texture-name :initarg :texture-name :initform "" :type 'string :reader face-texture-name)))


            

(defun parse-reflex-map-file (filename)
  (with-open-file (in filename :if-does-not-exist nil)
    (parse-header-line in)
    (
      ;; (loop for line = (read-line in nil)
      ;;           while line
      ;;           collect (funcall parse-line-fun line))
      ;;   (close in)))))

(defun main(argv)
  (format t "Hello world~%"))

;; Implementation
