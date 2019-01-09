#|
  This file is a part of reflex-map project.

  (C) COPYRIGHT Alexey Veretennikov<alexey.veretennikov@gmail.com>, 2018-2019
|#
(in-package :reflex-map)

;;;;;;;;;;;; Application entry point ;;;;;;;;;;;;

(defun usage (name)
  (format t "Usage: ~a input-file output-file [z-scale=1]~%there input-file is a Reflex Arena Map (.map) file, output-file - generated Quake1 .map file~%and optional 3rd argument specifies floating point z-scale of the converted map (1 is for 100%) which is default" name))

(defun main(&optional argv)
  (if (and (/= (length argv) 3)
           (/= (length argv) 4))
      (usage (car argv))
      (let ((from (second argv))
            (to (third argv)))
        (convert-reflex-to-qw from to
                              (if (= (length argv) 4)
                                  (read-from-string (fourth argv))
                                  1.0)))))


