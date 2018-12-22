#|
  This file is a part of reflex-map project.

  (C) COPYRIGHT Alexey Veretennikov<alexey.veretennikov@gmail.com>, 2018

  We reserve all rights in this file and in the information
  contained therein. Reproduction, use or disclosure to third
  parties without express authority is strictly forbidden.
|#

(in-package "CL-USER")

;;; The following lines added by ql:add-to-init-file:
#-quicklisp
(let ((quicklisp-init #+:MSWINDOWS "C:/apps/quicklisp/setup.lisp"
					  #-:MSWINDOWS (merge-pathnames ".quicklisp/setup.lisp"
                                                                        (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;; add Sources/ directory to quicklisp local directories
;; add Sources/ directory to quicklisp local directories
(push (pathname #+:MSWINDOWS "C:/Sources/lisp" #-:MSWINDOWS "~/Sources/lisp") ql:*local-project-directories*)
(ql:register-local-projects)


;;; Load the application:

(ql:quickload :reflex-map)
#-lispworks
(progn 
  (ql:quickload "trivial-dump-core")
  (trivial-dump-core:save-executable "reflex-map-converter" (lambda () (reflex-map:main #+:ccl ccl:*command-line-argument-list* #+:sbcl *posix-argv*)))
  (quit))


#+lispworks
(progn 
  (load-all-patches)

  #+MSWINDOWS (load "C:/apps/asdf/asdf.lisp")
  ;; Extend the stack size to be able to load all dependencies
  (hcl:extend-current-stack 400)
  
  ;;; Load the exmaple file that defines WRITE-MACOS-APPLICATION-BUNDLE
  ;;; to create the bundle.
  #+cocoa
  (compile-file (sys:example-file "configuration/macos-application-bundle.lisp") :load t)

  (require "ole")

  (deliver 'reflex-map:main-ui
           #+cocoa
           (create-macos-application-bundle
            "~/Sources/lisp/reflex-map/Reflex Map Converter.app"
            ;; Do not copy file associations...
            :document-types nil
            ;; ...or CFBundleIdentifier from the LispWorks bundle
            :identifier "com.github.fourier.reflex-map")
           #+MSWINDOWS
           "C:/Sources/lisp/reflex-map/reflex-map-converter.exe"
           1
           :interface :capi
         ;:keep-modules t
           :startup-bitmap-file nil))

