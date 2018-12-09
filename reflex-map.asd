#|
  This file is a part of reflex-map project.

  (C) COPYRIGHT Alexey Veretennikov<alexey.veretennikov@gmail.com>, 2018
|#
(in-package :cl-user)
(defpackage reflex-map-asd
  (:use :cl :asdf))
(in-package :reflex-map-asd)

(defsystem reflex-map
  :version "0.1"
  :author ""
  :license ""
  :depends-on (alexandria split-sequence)
  :components ((:module "src"
                :components
                ((:file "reflex-map"))))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.md"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op reflex-map-test))))
