#|
  This file is a part of reflex-map project.

  (C) COPYRIGHT Alexey Veretennikov<alexey.veretennikov@gmail.com>, 2018
|#

(in-package :cl-user)
(defpackage reflex-map-test-asd
  (:use :cl :asdf))
(in-package :reflex-map-test-asd)

(defsystem reflex-map-test
  :author ""
  :license ""
  :depends-on (:reflex-map
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "reflex-map"))))
  :description "Test system for reflex-map"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
