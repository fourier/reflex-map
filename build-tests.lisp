#|
  This file is a part of reflex-map project.

  Copyright (C) Alexey Veretennikov<alexey.veretennikov@gmail.com>, 2018
|#

(ql:quickload "reflex-map-test")
(ql:quickload "trivial-dump-core")
(trivial-dump-core:save-executable "reflex-map-test.exe" 
  (lambda () 
    (let ((prove:*enable-colors* nil)
          (prove:*default-reporter* :fiveam))
       (asdf:test-system "reflex-map-test"))))
(quit)
