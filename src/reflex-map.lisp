#|
  This file is a part of reflex-map project.

  (C) COPYRIGHT Alexey Veretennikov<alexey.veretennikov@gmail.com>, 2018
|#

(in-package :cl-user)
(defpackage reflex-map
  (:use :cl :alexandria :split-sequence)
  (:export convert-reflex-to-qw main))

(in-package :reflex-map)

;; Implementation

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






;;; new implementation

(defparameter *parsed-queue* nil)

(defparameter *float-scanner*
  (ppcre:create-scanner "-?[0-9]+([.][0-9]+([Ee][0-9]+)?)"))

(defparameter *int-scanner*
  (ppcre:create-scanner "-?[0-9]+"))


(defun is-float (token)
  "check if the full string is a full floating point number"
  (multiple-value-bind (start end)
      (ppcre:scan *float-scanner* token)
    (and start end
         (= (- end start) (length token)))))

(defun is-int (token)
  "check if the full string is a full floating point number"
  (multiple-value-bind (start end)
      (ppcre:scan *int-scanner* token)
    (and start end
         (= (- end start) (length token)))))


    
(defun tokenize-string (line)
  (mapcar (lambda (token)
            (cond ((or (is-float token)
                       (is-int token))
                   (read-from-string token))
                  ((member token '("reflex" "map" "version" "global" "entity" "type" "brush" "vertices" "faces") :test #'string=)
                   (intern (string-upcase token)))
                  (t token)))
          (parse-and-trim line)))


(defun reflex-list-lexer (tokens)
  #'(lambda ()
      (let ((value (pop tokens)))
        (if (null value)
            (values nil nil)
            (let ((terminal
                   (cond ((stringp value) 'string)
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
        if (< indent prev-indent)
          do
             (dotimes (i (- prev-indent indent))
               (push 'dedent result)
               (pop stack))
        do
           ;; (format t "'~a' current indent: ~a prev: ~a~%"
           ;;         line
           ;;         indent prev-indent)
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
;; map ::= header body
;; header ::= reflex map version integer newline
;; body ::= global newline indent entries dedent
;; entries ::= entry newline | entries entry newline
;; entry ::= brush_entry | entity_entry
;; entity_entry ::= entity newline indent type string newline entry_attributes dedent
;; entry_attributes ::= entry_attribute newline | entry_attributes entry_attribute newline
;; entry_attribute ::= string | entry_attribute string



(yacc:define-parser *reflex-map-parser*
  (:start-symbol reflex-map)
  (:terminals (string integer float newline indent dedent reflex map version global entity type brush vertices faces))
  (reflex-map (header newline body
                      (lambda (h n b)
                        (declare (ignore h n))
                        (let ((entities-list (remove-if (lambda (x) (eql (type-of x) 'brush)) b))
                              (brushes-list (remove-if (lambda (x) (eql (type-of x) 'entity)) b)))
                          (make-instance 'reflex-map :entities entities-list :brushes brushes-list)))))

  (header (reflex map version integer))

  (body (global newline indent entries dedent
                (lambda (g n i e d)
                  (declare (ignore g n i d))
                  e)))
  (body entries)

  (entries (entry #'list))
  (entries (entry entries (lambda (e es) (cons e es))))

  (entry entity-entry brush-entry)

  (entity-entry (entity newline indent type string newline entry-attributes dedent
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
  (entry-attribute-value string
                         integer
                         float)

  (brush-entry (brush newline indent vertices-list faces-list dedent
                        (lambda (b n i v f d)
                          (declare (ignore b n i d))
                          (make-instance 'brush :vertices v :faces f))))


  (vertices-list (vertices newline indent vertices-lines dedent
                           (lambda (v n i v-l d)
                             (declare (ignore v n i d))
                             v-l)))

  (vertices-lines (vertex-line newline
                               (lambda (v n) (list v))))
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
                            
  (face-line (float float float float float integer integer integer string
                    (lambda (u v s-u s-v rot v1 v2 v3 texture-name)
                      (make-instance 'face
                                     :u u :v v :scale-u s-u :scale-v s-v
                                     :rotation rot
                                     :texture-name texture-name
                                     :vertices (list v1 v2 v3)))))

  (face-line (float float float float float integer integer integer integer string
                    (lambda (u v s-u s-v rot v1 v2 v3 v4 texture-name)
                      (make-instance 'face
                                     :u u :v v :scale-u s-u :scale-v s-v
                                     :rotation rot
                                     :texture-name texture-name
                                     :vertices (list v1 v2 v3 v4)))))

  (face-line (float float float float float integer integer integer
                    (lambda (u v s-u s-v rot v1 v2 v3)
                      (make-instance 'face
                                     :u u :v v :scale-u s-u :scale-v s-v
                                     :rotation rot
                                     :texture-name ""
                                     :vertices (list v1 v2 v3)))))

  (face-line (float float float float float integer integer integer integer
                    (lambda (u v s-u s-v rot v1 v2 v3 v4)
                      (make-instance 'face
                                     :u u :v v :scale-u s-u :scale-v s-v
                                     :rotation rot
                                     :texture-name ""
                                     :vertices (list v1 v2 v3 v4))))))

(defun parse-reflex-map-file (filename)
  (yacc:parse-with-lexer (reflex-list-lexer (reflex-map-lexer filename)) *reflex-map-parser*))


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


(defun convert-reflex-to-qw (in-filename out-filename)
  (when-let ((map (parse-reflex-map-file in-filename)))
    (create-qw-map-file map out-filename)))


(defun main(argv)
  (declare (ignore argv))
  (format t "Hello world~%"))