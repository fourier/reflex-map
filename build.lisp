#|
  This file is a part of reflex-map project.

  (C) COPYRIGHT Alexey Veretennikov<alexey.veretennikov@gmail.com>, 2018

  We reserve all rights in this file and in the information
  contained therein. Reproduction, use or disclosure to third
  parties without express authority is strictly forbidden.
|#

(ql:quickload :reflex-map)
(ql:quickload "trivial-dump-core")
(trivial-dump-core:save-executable "reflex-map-converter" (lambda () (reflex-map:main #+:ccl ccl:*command-line-argument-list* #+:sbcl *posix-argv*)))
(quit)
