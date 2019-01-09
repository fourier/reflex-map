#|
  This file is a part of reflex-map project.

  (C) COPYRIGHT Alexey Veretennikov<alexey.veretennikov@gmail.com>, 2018
|#

(in-package :cl-user)
(defpackage reflex-map-test
  (:use :cl
        :reflex-map
        :prove)
  (:shadowing-import-from reflex-map
                          plane-equation
                          make-vertex))

(in-package :reflex-map-test)

;; turn off ansi colors in report output
(setf prove.color:*enable-colors* nil)
;; change type of the reporter to Test Anything Protocol
(setf prove:*default-reporter* :tap)

;; NOTE: To run this test file, execute `(asdf:test-system :reflex-map)' in your Lisp.

(plan nil)

;; Tests
(subtest "Testing arithmetics"
  (is (multiple-value-list
       (reflex-map::plane-equation (make-vertex -3 2 -1) (make-vertex -1 2 4) (make-vertex 3 3 -1)))
      '(-5 30 2 -73) :test #'equal))

(finalize)
