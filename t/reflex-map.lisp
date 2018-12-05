#|
  This file is a part of reflex-map project.

  (C) COPYRIGHT Alexey Veretennikov<alexey.veretennikov@gmail.com>, 2018
|#

(in-package :cl-user)
(defpackage reflex-map-test
  (:use :cl
        :reflex-map
        :prove))
(in-package :reflex-map-test)

;; NOTE: To run this test file, execute `(asdf:test-system :reflex-map)' in your Lisp.

(plan nil)

;; Tests

(finalize)
