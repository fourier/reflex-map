#|
  This file is a part of reflex-map project.

  (C) COPYRIGHT Alexey Veretennikov<alexey.veretennikov@gmail.com>, 2018
|#
(in-package :reflex-map)

;; Implementation
;;
;; Implementation details
;; The main job is done by the function convert-reflex-to-qw (in-filename out-filename &optional (z-scale 1))
;; Usage example:

;; (reflex-map:convert-reflex-to-qw "my-reflex-map.map" "my-qw-map.map" 0.85)
;;
;; The implementation consists of 2 phases:
;; 1. parsing the Reflex Map via parse-reflex-map-file function to the reflex-map class instance
;; 2. creating the output TrenchBroom map via create-qw-map-file function.
;;
;; Parsing
;; Parsing is done via typicall lexing/parsing scheme.
;; As the Reflex map format is a text file with indentation-based blocking similar to Python,
;; the lexing is done on per-line basis. Main function to perform lexing of the line is
;; tokenize-string. This function is called iteratively from the file lexer reflex-map-stream-lexer,
;; which also counts indents and inserts "dedent" artificial token the the indentation has been changed
;; (block closed)
;; The parser is implemented using CL-YACC library. In order to convert tokens list to the lexer function
;; supported by CL-YACC, the reflex-list-lexer is added (adapted from documentation for CL-YACC) which converts
;; a list to a closure of a function without arguments which returs 1 token on each call.
;; The grammar is defined in the *reflex-map-parser* variable via yacc:define-parser macro.
;; The grammar is defined in a self-explanatory EBNF-style following the format of the Reflex map files.
;; Upon parsing the objects on necessary levels are created and assembled, and at the end
;; the fresh reflex-map class instance is created. The coordinates got shifted while creating the 'vertex'
;; class instances. This grammar is used in parse-reflex-map-file function which finally will return the instance of
;; reflex-map class.
;;
;; Generating
;; Having the parsed data in the reflex-map class instance, we can generate the output. There are 5 steps to perform
;; here in the create-qw-map-file function:
;; 1) Define a global transformation matrix which will be applied to all coordinates.
;;    This global transformation matrix is perfoming rescaling in Z direction right now using 3d-matrices library.
;; 2) Function export-prefab, to recursively export prefabs, starting from "global" prefab, applying transformations
;;    along the way. This is done via first exporting the prefab (brushes) themselves, then go through the list of
;;    prefabs "embedded" into it exporting them recursively (using the same export-prefab function).
;;    Every "sub"-prefab is defined via its name (hence we can find corresponding prefab is the global list),
;;    its position and angles. But prefab in a global list of prefabs is defined in own coordinates.
;;    Therefore we have to apply necessary transformations before exporting brushes.
;; 3) Convert player spawns to QW format and export them
;; 4) Convert lights to QW format and export
;; 5) Convert some items and export them - ammo, health, armors, weapons
;;


(defparameter *version* "0.7"
  "The software version to be used in UI and in help")

(defparameter *round-to-integers* nil
  "Determine if the floating coordinates values should be
rounded to nearest integer in export process")

(defparameter *export-lightsources* t
  "Determine if the light sources should be exported to QW map")

(defparameter *export-items* t
  "Determine if the items should be exported to QW map")

(defparameter *export-spawns* t
  "Determine if the spawn locations should be exported to QW map")


(defparameter *float-scanner*
  (ppcre:create-scanner "-?[0-9]+([.][0-9]+([Ee][0-9]+)?)")
  "Floating point numbers scanner (including scientific format")

(defparameter *int-scanner*
  (ppcre:create-scanner "-?[0-9]+")
  "Integers scanner (including negative)")

(defparameter *hex-scanner*
  (ppcre:create-scanner "0[xX][0-9a-fA-F]+")
  "Numbers in hex format scanner")


(defclass reflex-map ()
  ((version :initarg :version :initform nil :reader map-version)
   (prefabs :initarg :prefabs :initform nil :reader map-prefabs)
   (global-prefab :initarg :global :initform nil :reader map-global-prefab))
  (:documentation "The parsed data from the Reflex map file"))

(defclass prefab-base ()
  ((entities :initarg :entities :initform nil :reader prefab-entities)
   (brushes :initarg :brushes :initform nil :reader prefab-brushes))
  (:documentation "The base class for prefabs, containing both enities list and brushes"))

(defclass normal-prefab (prefab-base)
  ((name :initarg :name :initform "" :reader prefab-name))
  (:documentation "Named prefab (used in global geometry prefab and other prefabs"))

(defclass global-prefab (prefab-base)
  ()
  (:documentation "Global(main) prefab - containing the actual map geometry"))
    

(defclass entity ()
  ((type :initarg :type :initform nil :reader entity-type)
   (properties :initarg :properties :initform nil
               :reader entity-properties))
  (:documentation "Entity section representation"))


(defclass brush ()
  ((vertices :initarg :vertices :initform nil :reader brush-vertices
             :documentation "A list of vertices")
   (faces :initarg :faces :initform nil :reader brush-faces
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
   (model-color :initarg :model-color :initform "" :type 'string :reader face-model-color)
   (model-name :initarg :model-name :initform "" :type 'string :reader face-model-name))
  (:documentation "The parsed face information"))


(defmethod print-object :after ((self entity) stream)
  "Print the contents of the ENTITY"
  nil)
;;;   (with-slots (type properties) self
;;;     (format stream "Entity type: ~a~%Properties:~%" type)
;;;     (dolist (x properties)
;;;       (format stream "~a~%" x))))

(defmethod print-vertex ((self vertex) stream)
  "Print the contents of the VERTEX"
  (with-slots (x y z) self
    (format stream "( ~a ~a ~a )" x y z)))


(defmethod print-inverse-vertex ((self vertex) stream)
  "Print the contents of the VERTEX"
  (with-slots (x y z) self
;;    (format stream "( ~a ~a ~a )" z (round (* 0.6 y)) x)))
;;    (format stream "( ~a ~a ~a )" z x (round (* 0.6 y)))))
    (format stream "( ~a ~a ~a )" z x y)))


(defmethod print-object :after ((self vertex) stream)
  "Print the contents of the VERTEX"
  (print-vertex self stream))


(defmethod print-object :after ((self brush) stream)
  "Print the contents of the BRUSH"
  nil)
;;;   (with-slots (vertices faces) self
;;;     (format stream "Bursh~%Vertices:~%")
;;;     (dolist (v vertices) (print-object v stream))
;;;     (format stream "Faces:~%")
;;;     (dolist (f faces) (print-object f stream))))


(defun num (n)
  "Returns either the number or a rounded number, depending on
*ROUND-TO-INTEGERS* variable value"
  (if *round-to-integers* (round n) n))
           

(defmethod find-entity-property ((self entity) name)
  "Returns the list of properties for the entity by the NAME provided.
The property name is excluded"
  (when-let (found 
             (find-if (lambda (prop)
                        (string= (string-downcase (second prop))
                                 (string-downcase name)))
                      (entity-properties self)))
    (cddr found)))


(defun make-vertex (x y z)
  "Constructor for the VERTEX class instances"
  (make-instance 'vertex :x x :y y :z z))


(defmethod vertex-coords ((self vertex))
  "Return the simple list of coordinates of the vertex"
  (with-slots (x y z) self
    (list x y z)))
  

(defun read-windows-line (in)
  "Read the line from the stream.
Return the line without the trailing newline characters"
  (when-let (line (read-line in nil))
    (string-right-trim '(#\Newline #\Return) line)))


(defun count-indent (line &optional (indent-char #\tab))
  "Calculate the number of indentation characters in the beginning of the line"
  (loop for c across line
        for i below (length line)
        while (char= c indent-char)
        finally (return i)))

(defun if-indent (line count)
  "Determine if the LINE has COUNT indentation characters"
  (and line
       (= (count-indent line) count)))

(defun parse-and-trim (line)
  "Return a list of words from the string line"
  (split-sequence:split-sequence #\Space (string-trim '(#\Tab #\Space) line)))


(macrolet ((def-is-fun (type scanner-var)
             "Create a is- function (which receives a string as an argument).
The TYPE and the regexp SCANNER-VAR used to create a function name and a
matcher correspondingly"
             (let ((fun-name (intern (string-upcase (concatenate 'string "is-" (symbol-name type))))))
               `(progn
                  (defun ,fun-name (token)
                    (multiple-value-bind (start end)
                        (ppcre:scan ,scanner-var token)
                      (and start end
                           (= (- end start) (length token)))))))))
  (def-is-fun float *float-scanner*)
  (def-is-fun int   *int-scanner*)
  (def-is-fun hex   *hex-scanner*))
  
    
(defun tokenize-string (line)
  "Tokenize the given line (without newline character at the end).
This function produces the list of tokens:
- float number
- integers
- symbols if any of them in the symbols list
- strings otherwise"
  (mapcar (lambda (token)
            (cond ((or (is-float token)
                       (is-int token))
                   (read-from-string token))
                  ((member token '("reflex" "map" "version" "global" "prefab" "entity" "type" "brush" "vertices" "faces") :test #'string=)
                   (intern (string-upcase token) :reflex-map))
                  (t token)))
          (parse-and-trim line)))


(defun reflex-list-lexer (tokens)
  "Takes the list of tokens and returns a closure lexer.
The closure will take no arguments and on each call will return 2 values (values terminal value)
Here the terminal will be the symbol and value the actual value (if applicable) or the symbol again"
  #'(lambda ()
      (let ((value (pop tokens)))
        (if (null value)
            ;; terminator
            (values nil nil)
            (let ((terminal
                   ;; first check on hex as the hex is a subset of a string
                    (cond ((and (stringp value) (is-hex value)) 'hex) 
                          ((stringp value) 'string)
                          ;; integer strings are subsets of float strinngs,
                          ;; so check them first
                          ((integerp value) 'integer)
                          ((floatp value) 'float)
                          ;; value of symbol is the symbol itself
                          ((symbolp value) value)
                          (t (error "Unexpected value ~S" value)))))
              (values terminal value))))))


(defun reflex-map-stream-lexer (in-stream &optional (indent-char #\Tab))
  "The main lexer function for Reflex map format.
As Reflex Map format is indentation-based, so the lexer will perform
counting of the indentations and if it is changed insert either 'indent'
or 'dedent'(artificial) terminals to the output."
  (loop with stack = (list 0)
        with result = nil
        for line = (read-windows-line in-stream)
        while line
        for indent = (count-indent line indent-char)
        for tokens = (tokenize-string line)
        for prev-indent = (car stack)
        if (> indent prev-indent)
          do
             (dotimes (i (- indent prev-indent))
               (push indent stack)
               (push 'indent result))               
        else
          do
             (dotimes (i (- prev-indent indent))
               (push 'dedent result)
               (pop stack))
        do
           (dolist (tok tokens)
             (push tok result))
           (push 'newline result)
        finally
           (dotimes (i (car stack))
             (push 'dedent result))
           (return (nreverse result))))


(defun reflex-map-lexer (filename)
  "Lexer for the Reflex Map file FILENAME"
  (with-open-file (in filename :if-does-not-exist nil)
    (reflex-map-stream-lexer in)))


(defun prefab-from-entries (name entries)
  "Split the list of entries to entities and brushes and create a named prefab object.
This function is used in parser"
  (let ((entities-list (remove-if (lambda (x) (eql (type-of x) 'brush)) entries))
        (brushes-list (remove-if (lambda (x) (eql (type-of x) 'entity)) entries)))
    (make-instance 'normal-prefab :name name :entities entities-list :brushes brushes-list)))

(defun global-from-entries (entries)
  "Split the list of entries to entities and brushes and create a global prefab object.
This function is used in parser"  
  (let ((entities-list (remove-if (lambda (x) (eql (type-of x) 'brush)) entries))
        (brushes-list (remove-if (lambda (x) (eql (type-of x) 'entity)) entries)))
    (make-instance 'global-prefab :entities entities-list :brushes brushes-list)))

(defun create-reflex-map (version prefabs)
  "Create an instance of the reflex-map class by the given prefabs list and version"
  (let ((global (find-if (lambda (x) (eql (type-of x) 'global-prefab)) prefabs))
        (only-prefabs (remove-if (lambda (x) (eql (type-of x) 'global-prefab)) prefabs)))
    (make-instance 'reflex-map :version version
                               :global global
                               :prefabs only-prefabs)))

;; grammar of the Reflex Arena map file
(yacc:define-parser *reflex-map-parser*
  (:start-symbol reflex-map)
  (:terminals (string integer float hex newline indent dedent reflex map version global prefab entity type brush vertices faces))
  (reflex-map (header newline body (lambda (h n b)
                                     (declare (ignore n))
                                     (create-reflex-map h b))))

  (header (reflex map version integer
                  ;; return version only
                  (lambda (r m v i)
                    (declare (ignore r m v))
                    i)))
  (body prefabs (entries (lambda (e)
                           ;; still create a list of 1 element
                           (list (global-from-entries e)))))

  (prefabs (prefab-term #'list))
  (prefabs (prefab-term prefabs (lambda (p-t p) (cons p-t p))))
  (prefab-term
   (global newline indent entries dedent
           (lambda (g n i e d)
             (declare (ignore g n i d))
             (global-from-entries e)))
   (prefab prefab-name newline indent entries dedent
                       (lambda (p s n i e d)
                         (declare (ignore p n i d))
                         (prefab-from-entries (format nil "~a" s) e))))
  
  (prefab-name string integer hex float)
  
  (entries (entry #'list))
  (entries (entry entries (lambda (e es) (cons e es))))

  (entry entity-entry brush-entry)

  (entity-entry (entity newline indent type string newline dedent
                        (lambda (e n i type typename n-2 d)
                          (declare (ignore e n i type n-2 d))
                          (make-instance 'entity :type typename :properties nil)))

                (entity newline indent type string newline entry-attributes dedent
                        (lambda (e n i type typename n-2 e-a d)
                          (declare (ignore e n i type n-2 d))
                          (make-instance 'entity :type typename :properties e-a))))

  (entry-attributes (entry-attribute-line newline
                                          (lambda (e-a-l nl)
                                            (declare (ignore nl))
                                            (list e-a-l))))
  
  (entry-attributes (entry-attributes entry-attribute-line newline
                                      (lambda (e-a e-a-l nl)
                                        (declare (ignore nl))
                                        (nconc e-a (list e-a-l)))))
  
  (entry-attribute-line (entry-attribute-value #'list))

  (entry-attribute-line (entry-attribute-value entry-attribute-line
                                               (lambda (e-a-v e-a-l)
                                                 (cons e-a-v e-a-l))))
  (entry-attribute-value hex string integer float)

  (brush-entry (brush newline indent vertices-list faces-list dedent
                        (lambda (b n i v f d)
                          (declare (ignore b n i d))
                          (make-instance 'brush :vertices v :faces f))))


  (vertices-list (vertices newline indent vertices-lines dedent
                           (lambda (v n i v-l d)
                             (declare (ignore v n i d))
                             v-l)))

  (vertices-lines (vertex-line newline
                               (lambda (v n)
                                 (declare (ignore n))
                                 (list v))))
  (vertices-lines (vertices-lines vertex-line newline
                                  (lambda (v-l v n)
                                    (declare (ignore n))
                                    (nconc v-l (list v)))))
  (vertex-line (float float float
                      (lambda (a1 a2 a3)
                        (position-vector (list a1 a2 a3) :type :vertex)))) 

  (faces-list (faces newline indent faces-lines dedent
                     (lambda (f n i f-l d)
                       (declare (ignore f n i d))
                       f-l)))

  (faces-lines (face-line newline
                          (lambda (f-l n)
                            (declare (ignore n))
                            (list f-l))))
                            
  (faces-lines (faces-lines face-line newline
                            (lambda (f-l f n)
                              (declare (ignore n))
                              (nconc f-l (list f)))))

  (face-line (offsets vertices-list models
                      (lambda (offs ver tex)
                        (destructuring-bind (u v s-u s-v rot) offs
                          (make-instance 'face
                                         :u u :v v :scale-u s-u :scale-v s-v
                                         :rotation rot
                                         ;; TODO: fix this. could be hex, string or both
                                         :model-name tex
                                         :vertices ver)))))

  (offsets (float float float float float
                  (lambda (u v s-u s-v rot)
                    (list u v s-u s-v rot))))

  (vertices-list (integer #'list)
                 (integer vertices-list (lambda (i v-l)
                                          (cons i v-l))))

  (models nil hex string (hex string)))
;; end of grammar definition


(defun parse-reflex-map-file (filename)
  "Return the reflex-map object from the parsed file.
The main function to parse the Reflex map file."
  (yacc:parse-with-lexer (reflex-list-lexer (reflex-map-lexer filename)) *reflex-map-parser*))


;; Mathematics

(defgeneric plane-equation (v1 v2 v3)
  (:documentation
   "Calculate the plane equation in format
Ax+By+Cz+D=0 and returns values A B C D
v1,v2,v3 are vertices.

The equation is calculated via
https://www.wolframalpha.com/input/?i=Collect%5Bdet%5B%7Bx-x1,+x2-x1,+x3-x1%7D,%7By-y1,+y2-y1,+y3-y1%7D,%7Bz-z1,+z2-z1,+z3-z1%7D%5D,+%7Bx,y,z%7D%5D
  
x (y1 z2 - y1 z3 - y2 z1 + y2 z3 + y3 z1 - y3 z2) + y (-x1 z2 + x1 z3 + x2 z1 - x2 z3 - x3 z1 + x3 z2) + z (x1 y2 - x1 y3 - x2 y1 + x2 y3 + x3 y1 - x3 y2) - x1 y2 z3 + x1 y3 z2 + x2 y1 z3 - x2 y3 z1 - x3 y1 z2 + x3 y2 z1

Hence
A = (y1 z2 - y1 z3 - y2 z1 + y2 z3 + y3 z1 - y3 z2)
B = (-x1 z2 + x1 z3 + x2 z1 - x2 z3 - x3 z1 + x3 z2)
C = (x1 y2 - x1 y3 - x2 y1 + x2 y3 + x3 y1 - x3 y2)
D = - x1 y2 z3 + x1 y3 z2 + x2 y1 z3 - x2 y3 z1 - x3 y1 z2 + x3 y2 z1"))


(defun plane-equation-impl (x1 y1 z1 x2 y2 z2 x3 y3 z3)
  "Actual calculation of the plane equation"
  (let ((A (- (+ (* y1 z2) (* y2 z3) (* y3 z1))
              (+ (* y1 z3) (* y2 z1) (* y3 z2))))
        (B (- (+ (* x1 z3) (* x2 z1) (* x3 z2))
              (+ (* x1 z2) (* x2 z3) (* x3 z1))))
        (C (- (+ (* x1 y2) (* x2 y3) (* x3 y1))
              (+ (* x1 y3) (* x2 y1) (* x3 y2))))
        (D (- (+ (* x1 y3 z2) (* x2 y1 z3) (* x3 y2 z1))
              (+ (* x1 y2 z3) (* x2 y3 z1) (* x3 y1 z2)))))
    (values A B C D)))


(defmethod plane-equation ((v1 vertex) (v2 vertex) (v3 vertex))
  "Calculate plane equation parameters by given 3 vertexes"
  (let* ((x1 (vertex-x v1))
         (x2 (vertex-x v2))
         (x3 (vertex-x v3))
         (y1 (vertex-y v1))
         (y2 (vertex-y v2))
         (y3 (vertex-y v3))
         (z1 (vertex-z v1))
         (z2 (vertex-z v2))
         (z3 (vertex-z v3)))
    (plane-equation-impl x1 y1 z1 x2 y2 z2 x3 y3 z3)))


(defmethod plane-equation ((v1 vec4) (v2 vec4) (v3 vec4))
  "Calculate plane equation parameters by given 3 vectors"
  (let* ((x1 (vx v1))
         (x2 (vx v2))
         (x3 (vx v3))
         (y1 (vy v1))
         (y2 (vy v2))
         (y3 (vy v3))
         (z1 (vz v1))
         (z2 (vz v2))
         (z3 (vz v3)))
    (plane-equation-impl x1 y1 z1 x2 y2 z2 x3 y3 z3)))


(defmethod plane-equation ((v1 list) (v2 list) (v3 list))
  "Calculate plane equation parameters by given 3 lists of coordinates"
  (let* ((x1 (first v1))
         (x2 (first v2))
         (x3 (first v3))
         (y1 (second v1))
         (y2 (second v2))
         (y3 (second v3))
         (z1 (third v1))
         (z2 (third v2))
         (z3 (third v3)))
    (plane-equation-impl x1 y1 z1 x2 y2 z2 x3 y3 z3)))


(defun create-flip-transform (brushes)
  "Create a horizontal flip matrix transformation
from the list of brushes"
  ;; find center
  (let* ((center
           (loop for b in brushes
                 for verc = (mapcar #'vertex-coords
                                    (brush-vertices b))
                 for brush-bnds = (loop for v in verc
                                        minimizing (first v) into xmin
                                        minimizing (second v) into ymin
                                        minimizing (third v) into zmin
                                        maximizing (first v) into xmax
                                        maximizing (second v) into ymax
                                        maximizing (third v) into zmax
                                        finally (return (list xmin ymin zmin xmax ymax zmax)))
                 minimizing (elt brush-bnds 0) into xmin
                 minimizing (elt brush-bnds 1) into ymin
                 minimizing (elt brush-bnds 2) into zmin
                 maximizing (elt brush-bnds 3) into xmax
                 maximizing (elt brush-bnds 4) into ymax
                 maximizing (elt brush-bnds 5) into zmax
                 finally (return (list ;; perform swap z x y
                                  (/ (+ zmin zmax) 2)
                                  (/ (+ xmin xmax) 2)
                                  (/ (+ ymin ymax) 2)))))
         (mirror-matrix-x
           (mat4 '(-1 0 0 0
                   0 1 0 0
                   0 0 1 0
                   0 0 0 1)))
         (mirror-matrix-y
           (mat4 '(1 0 0 0
                   0 -1 0 0
                   0 0 1 0
                   0 0 0 1)))
         (mirror-matrix-z
           (mat4 '(1 0 0 0
                   0 1 0 0
                   0 0 -1 0
                   0 0 0 1)))
         (trans+center (mat4 (list 1 0 0 (first center)
                                   0 1 0 (second center)
                                   0 0 1 (third center)
                                   0 0 0 1)))
         (trans-center (mat4 (list 1 0 0 (- (first center))
                                   0 1 0 (- (second center))
                                   0 0 1 (- (third center))
                                   0 0 0 1))))
    ;; flip horizontally from TrenchBroom
    ;;  const auto transform = vm::translationMatrix(center) * vm::mirrorMatrix<FloatType>(axis) * vm::translationMatrix(-center);
    (declare (ignore mirror-matrix-x mirror-matrix-z))
    ;; (format t "geometry center: ~a ~a ~a~%" (first center)
    ;;         (second center) (third center))
    (m* (m* trans+center mirror-matrix-y) trans-center)))
  

(defun rotation-matrix (along-z along-x along-y)
  "Rotation matrix for the list of 3 angles given in degrees.
Angles are along z axis, x axis and y axis"
  (flet ((sind (x)     ; The argument is in degrees 
           (sin (* x (/ (float pi x) 180))))
         (cosd (x)     ; The argument is in degrees 
           (cos (* x (/ (float pi x) 180)))))
    (let* ((cx (cosd along-x))
           (sx (sind along-x))
           (x
             (mat4 (list 1 0 0 0
                         0 cx (- sx) 0
                         0 sx cx 0
                         0 0 0 1)))
           (cy (cosd along-y))
           (sy (sind along-y))
           (y
             (mat4 (list cy 0 sy 0
                         0  1 0  0
                         (- sy) 0 cy 0
                         0 0 0 1)))
           (cz (cosd along-z))
           (sz (sind along-z))
           (z
             (mat4 (list cz (- sz)  0  0
                         sz  cz     0  0
                         0 0 1 0
                         0 0 0 1))))
      (m* (m* x y) z))))


(defun position-vector (attrs &key (type :vertex))
  "Convert list of floats (coordinates in Reflex coordinate
system to appropriate vector depending of keyword :TYPE.
The coordinates swap is also performed.
TYPE could be one of either:
:VERTEX - returns an instance of VERTEX class
:VEC - returns an instance of vec3
:VEC4 - returns an instance of vec4"
  ;; transformation : x z y -> x y z
  (let ((x (first attrs))
        (y (third attrs))
        (z (second attrs)))
    (case type
      (:vertex (make-vertex x y z))
      (:vec (vec x y z))
      (:vec4 (vec4 x y z 1)))))


;; Exporting functions

(defun export-face (points transform out)
  "Export face to TB format.
POINTS is the list of points constituting face, where the point
is the list of coordinates.
During export we apply the transformation matrix TRANSFORM (mat 4x4)
to the coordinates.
OUT is the output stream."
  ;; take first 3 points from the face
  (let* ((vertices  (subseq points 0 3))
         ;; calculate the normal to the plane constructed from the
         ;; first 3 points
         (normal (multiple-value-list 
                  (apply #'plane-equation vertices)))
         ;; calculate the cos of the angle between 2 vectors
         ;; on a plane and normal, to determine the orientation
         ;; by the sign of the angle
         (angle (v. 
                 (vc (v- (apply #'vec3 (third vertices))
                         (apply #'vec3 (first vertices)))
                     (v- (apply #'vec3 (second vertices))
                         (apply #'vec3 (first vertices))))
                 (apply #'vec3 (subseq normal 0 3)))))
    ;; make sure the normal is in positive direction
    (when (> 0 angle)
      ;; if not, swap the vertices
      (rotatef (nth 1 vertices) (nth 2 vertices)))
    ;; apply transformation
    (let* ((new-vertices
             (mapcar (lambda (p)
                       (let* ((x (elt p 0))
                              (y (elt p 1))
                              (z (elt p 2)))
                         (m* transform (vec4 x y z 1))))
                     vertices))
           (new-normal (multiple-value-list 
                        (apply #'plane-equation new-vertices)))
           (new-angle
             (v. 
              (vc (v- (vxyz (third new-vertices))
                      (vxyz (first new-vertices)))
                  (v- (vxyz (second new-vertices))
                      (vxyz (first new-vertices))))
              (apply #'vec3 (subseq new-normal 0 3)))))
      ;; make sure the normal is in positive direction
      (when (> 0 new-angle)
        ;; if not, swap the vertices 
        (rotatef (nth 1 new-vertices) (nth 2 new-vertices)))
      ;; finally output all transformed vertices
      (mapc (lambda (v)
              (format out "( ~d ~d ~d ) " (num (vx v)) (num (vy v)) (num (vz v))))
            new-vertices))))



(defmethod export-brush ((self brush) transform out)
  "Export brush SELF to the stream OUT applying matrix TRANSFORM"
  (with-slots (vertices faces) self
    (format out "{~%")
    (dolist (f faces)
      ;; gather the points into the list of coordinates
      (let ((points
              (loop for vert-idx in (face-vertices f)
                    for coords = (vertex-coords (elt vertices vert-idx))
                    collect coords into vert-coords
                    finally (return vert-coords))))
        ;; export face applying transformation
        (export-face points transform out))
      ;; default face texture - just rock
      (format out " rock4_1 0 0 0 1 1~%"))
    (format out "}~%")))


(defmethod export-brushes ((self prefab-base) out transform)
  "Export all brushes from prefab SELF applying matrix TRANSFORM to the stream OUT"
  (with-slots (brushes) self
    (loop for br in brushes
          for i below (length brushes)
          do
             (format out "// brush ~d~%" i)
             (export-brush br transform out))))


(defmethod export-prefab ((self prefab-base) out
                          global-trans prefabs)
  "Export prefab to the stream OUT applying matrix GLOBAL-TRANS to all coordinates.
PREFABS is a list of named prefabs is the file. So if the prefab is using another
prefabs, it is searched in this list."
  ;; 1. export brushes
  (export-brushes self out global-trans)
  ;; 2. export prefabs if any
  (mapc
   ;; export every prefab entity
   (lambda (ent)
     ;; get all the prefab entity properties - name, position, angles
     (when-let (found
                (find-entity-property ent "prefabName"))
       (let ((position (find-entity-property ent "position"))
             (angles (find-entity-property ent "angles"))
             (name (car found)))
         ;; find corresponding prefab in in the list provided
         (when-let (prefab (find-if (lambda (p) (string= (prefab-name p) name)) prefabs))
           ;; sometimes prefabs are added without position, we ignore them (no where to place)
           (when position
             ;; translation matrix out of position
             (let ((transform (mtranslation (position-vector position :type :vec))))
               ;; if angles present, multiply transformation matrix by the rotation
               (when angles
                 ;; angles are in the following format:
                 (destructuring-bind (along-z along-x along-y)
                     angles
                   (setf transform
                         (m* transform
                           (rotation-matrix (- along-z)
                                            (- along-x)
                                            (- along-y))))))
               (format out "// prefab ~a, position: ~{~a~^, ~} angles: ~{~f~^, ~}~%" name position angles)
               (export-prefab prefab out (m* global-trans transform) prefabs)))))))
   ;; the list of prefab entities for current prefab
   (remove-if-not (lambda (e) (string= (string-downcase (entity-type e)) "prefab")) (prefab-entities self))))


(defmethod export-spawns ((self reflex-map) out global-trans)
  "Export spawn entities from parsed Reflex map SELF to the stream OUT.
Applying transformation matrix GLOBAL-TRANS to the coordinates"
  (format out "// spawns~%")
  (with-slots (entities) (map-global-prefab self)
    (mapc
     (lambda (ent)
       (let* ((position (find-entity-property ent "position"))
              (angles (find-entity-property ent "angles"))
              (angle (or (car angles) 0)))
         (when position
           (let ((p
                   (m* global-trans
                       ;; for info_player_start the hbox is defined as the following in TrenchBroom:
                       ;; @baseclass size(-16 -16 -24, 16 16 32) color(0 255 0) = PlayerClass []
                       ;; therefore we must offset spawn point
                       ;; by 24 units
                       (v+ (vec 0 0 24 0)
                           (position-vector position :type :vec4)))))
             (format out "{
\"classname\" \"info_player_deathmatch\"~%")
             (format out "\"origin\" ")
             (format out "\"~d ~d ~d\"~%" (num (vx p)) (num (vy p)) (num (vz p)))
             ;; in TrenchBroom only one angle supported - yaw
             (format out "\"angle\" ")
             (format out "\"~f\"~%" (- 90 angle))
             (format out "}~%")))))
     (remove-if-not (lambda (e) (string= (string-downcase (entity-type e)) "playerspawn")) entities))
    (values)))


(defmethod export-lights ((self reflex-map) out global-trans)
  "Export possible lights from the parsed Reflex Map. Only the following lights supported:
industrial/lights/light_rnd -> light_globe
industrial/lights/light_fluorescent -> light_flame_large_yellow
industrial/lights/light_step_sml -> light"
  (format out "// lights~%")
  (let ((light-sources (alist-hash-table
                        '(("industrial/lights/light_rnd" . "light_globe") ;; z: -8
                          ("industrial/lights/light_fluorescent" . "light_flame_large_yellow") ;; z: -12
                          ("industrial/lights/light_step_sml" . "light")) ;; z: -8
                        :test #'equalp)))
    (with-slots (entities) (map-global-prefab self)
      (mapc
       (lambda (ent)
         (let* ((position (find-entity-property ent "position"))
                (effect-type (car (find-entity-property ent "effectName"))))
           (when position
             (let ((p
                     (m* global-trans
                         (v+ (vec 0 0 8 0)
                             (position-vector position :type :vec4)))))
               (format out "{
\"classname\" \"~a\"~%" (gethash effect-type light-sources))
               (format out "\"origin\" ")
               (format out "\"~d ~d ~d\"~%" (num (vx p)) (num (vy p)) (num (vz p)))
               (format out "}~%")))))
       (remove-if-not
        (lambda (e)
          (let ((effect-type (find-entity-property e "effectName")))
            (and (string= (string-downcase (entity-type e))
                          "effect")
                 (gethash (string-downcase (car effect-type))
                          light-sources))))
        entities)))
    (values)))


;; item types:
;; Burst Gun 0
;; Shotgun 1
;; Grenade Launcher 2
;; Plasma Rifle 3
;; Rocket Launcer 4
;; Ion Cannon 5
;; Bolt Rifle 6
;; Stake Gun 7
;; 5 Health 40
;; 25 Health 41
;; 50 Health 42
;; 100 Health 43
;; 5 Armor 50
;; Light Armor 51
;; Medium Armor 52
;; Heavy Armor 53
;; Quad Damage 60
;; shotgun shells 21
;; gl ammo 22
;; rl ammo 24
;; ion ammo 25
;; plasma ammo 23 
;; bolt ammo 26
(defmethod export-items ((self reflex-map) out global-trans)
  "Export some of pickup items from the parsed Reflex map SELF.
See the mapping below which items are actually supported"
  (format out "// pickups~%")
  (let ((pickup-types (alist-hash-table
                        '((1 . "weapon_supershotgun") ;; shotgun
                          (2 . "weapon_grenadelauncher") ;; gl
                          (3 . "weapon_nailgun") ;; plasma
                          (4 . "weapon_rocketlauncher") ;; rl
                          (5 . "weapon_supernailgun") ;; ion
                          (6 . "weapon_lightning") ;; bolt
                          (21 . "item_shells") ;; shotty
                          (22 . "item_rockets") ;; nades
                          (23 . "item_spikes") ;; plasma
                          (24 . "item_rockets") ;; rockets
                          (25 . "item_spikes") ;; ion
                          (26 . "item_cells") ;; bolt
                          (41 . "item_health") ;; ?? 25 "spawnflags" "1"
                          (42 . "item_health") ;; ?? 50 "spawnflags" "0"
                          (43 . "item_health") ;; "spawnflags" "2"
                          (51 . "item_armor1")
                          (52 . "item_armor2")
                          (53 . "item_armorInv")))))
    (with-slots (entities) (map-global-prefab self)
      (mapc
       (lambda (ent)
         (let* ((position (find-entity-property ent "position"))
                (pickup-type (car (find-entity-property ent "pickupType"))))
           (when position
             (let ((p
                     (m* global-trans
                         (v+ (vec 0 0 8 0)
                             (position-vector position :type :vec4)))))
               (format out "{
\"classname\" \"~a\"~%" (gethash pickup-type pickup-types))
               (format out "\"origin\" ")
               (format out "\"~d ~d ~d\"~%" (num (vx p)) (num (vy p)) (num (vz p)))
               (cond ((= pickup-type 41)
                      (format out "\"spawnflags\" \"~d\"~%" 1))
                     ((= pickup-type 42)
                      (format out "\"spawnflags\" \"~d\"~%" 0))
                     ((= pickup-type 43)
                      (format out "\"spawnflags\" \"~d\"~%" 2)))
               (format out "}~%")))))
       (remove-if-not
        (lambda (e)
          (let ((pickup-type (find-entity-property e "pickupType")))
            (and (string= (string-downcase (entity-type e))
                          "pickup")
                 (gethash (car pickup-type)
                          pickup-types))))
        entities)))
    (values)))


(defmethod create-qw-map-file ((self reflex-map) filename &optional (scales (list 1 1 1)))
  "Export parsed Reflex map SELF to the TB format file FILENAME.
SCALES is a list of scale transformations: along x, y, z axis."
  (with-open-file (out filename :direction :output :if-exists :supersede)
    (format out "// Game: Quake
// Format: Standard
// entity 0
{
\"classname\" \"worldspawn\"
\"wad\" \"C:/q1mapping/wads/START.WAD\"~%")
    ;; get the global transformation matrix
    (let ((global-trans  (nmscale (meye 4) (apply #'vec scales))))
      ;; recursively export prefabs/geometry
      (export-prefab (map-global-prefab self) out
                     global-trans (map-prefabs self))
      (format out "}~%")
      ;; export everything else
      (when *export-spawns*
        (export-spawns self out global-trans))
      (when *export-lightsources* 
        (export-lights self out global-trans))
      (when *export-items*
        (export-items  self out global-trans)))))


;; The application main function
(defun convert-reflex-to-qw (in-filename out-filename &optional (z-scale 1))
  "Parse the Reflex Map file IN-FILENAME and export the TrenchBroom supported file OUT-FILENAME.
The Z-SCALE specifies the multiplier for the height of objects"
  (when-let ((map (parse-reflex-map-file in-filename)))
    (create-qw-map-file map out-filename (list 1 1 z-scale))))



