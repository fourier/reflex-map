#|
  This file is a part of reflex-map project.

  (C) COPYRIGHT Alexey Veretennikov<alexey.veretennikov@gmail.com>, 2018
|#

(in-package :cl-user)
(defpackage reflex-map
  (:use :cl :alexandria :split-sequence :3d-matrices :3d-vectors   #+:lispworks :capi)
  (:export convert-reflex-to-qw main))

(in-package :reflex-map)

;; Implementation

(defparameter *float-scanner*
  (ppcre:create-scanner "-?[0-9]+([.][0-9]+([Ee][0-9]+)?)"))

(defparameter *int-scanner*
  (ppcre:create-scanner "-?[0-9]+"))

(defparameter *hex-scanner*
  (ppcre:create-scanner "0[xX][0-9a-fA-F]+"))



(defclass reflex-map ()
  ((entities :initarg :entities :initform nil :reader map-entities)
   (brushes :initarg :brushes :initform nil :reader map-brushes)))
    

(defclass entity ()
  ((type :initarg :type :initform nil)
   (properties :initarg :properties :initform nil))
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
   (texture-color :initarg :texture-color :initform "" :type 'string :reader face-texture-color)
   (texture-name :initarg :texture-name :initform "" :type 'string :reader face-texture-name)))


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


(defun make-vertex (x y z)
  (make-instance 'vertex :x x :y y :z z))

(defmethod vertex-coords ((self vertex))
  (with-slots (x y z) self
    (list x y z)))
  

(defun read-windows-line (in)
  (when-let (line (read-line in nil))
    (string-right-trim '(#\Newline #\Return) line)))


(defun count-indent (line &optional (indent-char #\tab))
  (loop for c across line
        for i below (length line)
        while (char= c indent-char)
        finally (return i)))

(defun if-indent (line count)
  (and line
       (= (count-indent line) count)))

(defun parse-and-trim (line)
  (split-sequence:split-sequence #\Space (string-trim '(#\Tab #\Space) line)))


(defmacro def-is-fun (type scanner-var)
  (let ((fun-name (intern (string-upcase (concatenate 'string "is-" (symbol-name type))))))
  `(progn
     (defun ,fun-name (token)
       (multiple-value-bind (start end)
           (ppcre:scan ,scanner-var token)
         (and start end
              (= (- end start) (length token))))))))

(def-is-fun float *float-scanner*)
(def-is-fun int   *int-scanner*)
(def-is-fun hex   *hex-scanner*)
  
    
(defun tokenize-string (line)
  (mapcar (lambda (token)
            (cond ((or (is-float token)
                       (is-int token))
                   (read-from-string token))
                  ((member token '("reflex" "map" "version" "global" "prefab" "entity" "type" "brush" "vertices" "faces") :test #'string=)
                   (intern (string-upcase token) :reflex-map))
                  (t token)))
          (parse-and-trim line)))


(defun reflex-list-lexer (tokens)
  #'(lambda ()
      (let ((value (pop tokens)))
        (print value)
        (if (null value)
            (values nil nil)
            (let ((terminal
                    (cond ((and (stringp value) (is-hex value)) 'hex)
                          ((stringp value) 'string)
                          ((integerp value) 'integer)
                          ((floatp value) 'float)
                          ;; value of symbol is the symbol itself
                          ((symbolp value) value)
                          (t (error "Unexpected value ~S" value)))))
              (values terminal value))))))


(defun reflex-map-stream-lexer (in-stream &optional (indent-char #\Tab))
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
  ;; list of tokens:
  ;; tab, string, newline
  (with-open-file (in filename :if-does-not-exist nil)
    (reflex-map-stream-lexer in)))


;; grammar:
;; reflex-map ::= header newline body
;; header ::= reflex map version integer
;; NOTE: condition here handles 6 vs 8 map version formats
;; body ::= global newline indent entries dedent | entries
;;
;;
;; entries ::= entry | entry entries
;; entry ::= entity-entry | brush-entry 
;; entity-entry ::= entity newline indent type string newline dedent
;; entity-entry ::= entity newline indent type string newline entry-attributes dedent
;; entry-attributes ::= entry-attribute-line newline | entry-attributes entry-attribute-line newline
;; entry-attribute-line ::= entry-attribute-value | entry-attribute-value entry-attribute-line
;; entry-attribute-value ::= string | integer | float
;; brush-entry ::= brush newline indent vertices-list faces-list dedent
;; vertices-list ::= vertices newline indent vertices-lines dedent
;; vertices-lines ::= vertex-line newline | vertices-lines vertex-line newline
;; vertex-line ::= float float float
;; faces-list ::= faces newline indent faces-lines dedent
;; faces-lines ::= face-line newline | faces-lines face-line newline
;; face-line ::= float float float float float integer integer integer string | float float float float float integer integer integer integer string | float float float float float integer integer integer | float float float float float integer integer integer integer
;; 


(yacc:define-parser *reflex-map-parser*
  (:start-symbol reflex-map)
  (:terminals (string integer float hex newline indent dedent reflex map version global prefab entity type brush vertices faces))
  (reflex-map (header newline body
                      (lambda (h n b)
                        (declare (ignore h n))
                                 b)))

  (header (reflex map version integer))
  (body
   (prefabs
    (lambda (p)
      (let ((entities-list (remove-if (lambda (x) (eql (type-of x) 'brush)) p))
            (brushes-list (remove-if (lambda (x) (eql (type-of x) 'entity)) p)))
        (make-instance 'reflex-map :entities entities-list :brushes brushes-list))))
   (entries
    (lambda (e)
      (let ((entities-list (remove-if (lambda (x) (eql (type-of x) 'brush)) e))
            (brushes-list (remove-if (lambda (x) (eql (type-of x) 'entity)) e)))
        (make-instance 'reflex-map :entities entities-list :brushes brushes-list)))))

  (prefabs prefab-term)
  (prefabs (prefab-term prefabs))
  (prefab-term
   (global newline indent entries dedent
           (lambda (g n i e d)
             (declare (ignore g n i d))
             e))
   (prefab string newline indent entries dedent
                       (lambda (p s n i e d)
                         (declare (ignore p s n i d))
                         e)))

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
  (vertex-line (float float float (lambda (x y z) (make-instance 'vertex :x x :y y :z z))))

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

  (face-line (offsets vertices-list textures
                      (lambda (offs ver tex)
                        (destructuring-bind (u v s-u s-v rot) offs
                          (make-instance 'face
                                         :u u :v v :scale-u s-u :scale-v s-v
                                         :rotation rot
                                         ;; TODO: fix this. could be hex, string or both
                                         :texture-name tex
                                         :vertices ver)))))

  (offsets (float float float float float
                  (lambda (u v s-u s-v rot)
                    (list u v s-u s-v rot))))

  (vertices-list (integer integer integer integer)
                 (integer integer integer))

  (textures nil hex string (hex string)))

(defun parse-reflex-map-file (filename)
  (yacc:parse-with-lexer (reflex-list-lexer (reflex-map-lexer filename)) *reflex-map-parser*))


;; math

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
           
    
(defun export-face (points transform out)
  (let* ((vertices  (subseq points 0 3))
         (normal (multiple-value-list 
                  (apply #'plane-equation vertices)))
         (angle (v. 
                 (vc (v- (apply #'vec3 (third vertices))
                         (apply #'vec3 (first vertices)))
                     (v- (apply #'vec3 (second vertices))
                         (apply #'vec3 (first vertices))))
                 (apply #'vec3 (subseq normal 0 3)))))
    ;; make sure the normal is in positive direction
    (when (> 0 angle)
      (rotatef (nth 1 vertices) (nth 2 vertices)))
    ;; apply transformation
    (let* ((new-vertices
             (mapcar (lambda (p)
                       ;; swap : x y z -> z x y
                       (let* ((x (elt p 2))
                              (y (elt p 0))
                              (z (elt p 1)))
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
        (rotatef (nth 1 new-vertices) (nth 2 new-vertices)))
      (mapc (lambda (v) (format out "( ~a ~a ~a ) " (vx v) (vy v) (vz v))) new-vertices))))




  

(defmethod export-brush ((self brush) transform out)
  (with-slots (vertices faces) self
    (format out "{~%")
    (dolist (f faces)
      (let ((points
              (loop for vert-idx in (face-vertices f)
                    for coords = (vertex-coords (elt vertices vert-idx))
                    collect coords into vert-coords
                    finally (return vert-coords))))
        (export-face points transform out))
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
      (let ((transform (create-flip-transform brushes)))       
        (loop for br in brushes
              for i below (length brushes)
              do
                 (format out "// brush ~d~%" i)
                 (export-brush br transform out))))
    (format out "}~%")))


(defun convert-reflex-to-qw (in-filename out-filename)
  (when-let ((map (parse-reflex-map-file in-filename)))
    (create-qw-map-file map out-filename)))

#+lispworks
(capi:define-interface my-interface ()
  ()
  (:panes
   (input-file-edit text-input-choice 
                    :title "Input (reflex) MAP file"
                    :buttons 
                    '(:browse-file (:image :std-file-open) :ok nil))
   (output-file-edit text-input-choice
                          :title"Output (QW) MAP file"
                          :buttons 
                          '(:browse-file (:image :std-file-open) :ok nil))
   (convert-button push-button :text "Convert" :callback 'on-convert-button))
  (:layouts
   (main-layout column-layout '(input-file-edit
                                output-file-edit
                                convert-button)
                :adjust :center
                :internal-border 20))
  (:default-initargs :title "Reflex Arena map geometry converter"
   :layout 'main-layout
   :initial-focus 'input-file-edit
   :visible-min-width 400
   ;;   :help-callback 'on-main-window-tooltip
   ))

#+:lispworks
(defun on-convert-button (data self)
  (declare (ignore data))           
  (with-slots (input-file-edit
               output-file-edit
               convert-button) self
    (flet ((enable-interface (enable)
             (setf (button-enabled convert-button) enable
                   (text-input-pane-enabled input-file-edit) enable
                   (text-input-pane-enabled output-file-edit) enable)))
      (let ((source-path (text-input-pane-text input-file-edit))
            (dest-path (text-input-pane-text output-file-edit)))
        ;; verify what paths are not empty
        (when (and (> (length source-path) 0) (> (length dest-path) 0))
          (enable-interface nil)
          (convert-reflex-to-qw source-path dest-path)
          (enable-interface t))))))


(defun main(&optional argv)
  #+:lispworks (declare (ignore argv))
  #-:lispworks (let ((from (first argv))
                     (to (second argv)))
                 (convert-reflex-to-qw from to))
  #+:lispworks
  (capi:display (make-instance 'my-interface)))

