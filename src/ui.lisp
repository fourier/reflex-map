#|
  This file is a part of reflex-map project.

  (C) COPYRIGHT Alexey Veretennikov<alexey.veretennikov@gmail.com>, 2018
|#

(in-package :reflex-map)

(define-interface reflex-map-converter-interface ()
  ()
  (:menus
   (application-menu
    "File"
    ((:component
      (("About"
        :callback
        (lambda ()
          (display-message-on-screen
           (convert-to-screen nil)
           "Reflex Arena -> Quake 1 Map Converter ~a~%Copyright (c) Alexey Veretennikov(fourier) 2018" *version*))
        :callback-type :none))))))
  (:panes
   (input-file-edit text-input-choice 
                    :title "Input (reflex) MAP file"
                    :callback 'on-convert-button
                    :drop-callback 'on-drop
                    :buttons 
                    '(:browse-file (:image :std-file-open) :ok nil))
   (output-file-edit text-input-choice
                          :title"Output (QW) MAP file"
                          :callback 'on-convert-button
                          :drop-callback 'on-drop
                          :buttons 
                          '(:browse-file (:image :std-file-open) :ok nil))
   (z-scale-options option-pane 
                    :items (list "100" "95" "90" "85" "80" "75" "70")
                    :selected-item "85"
                    :title "Z-scale, percents of original")
   (convert-button push-button :text "Convert" :callback 'on-convert-button))
  (:layouts
   (main-layout column-layout '(input-file-edit
                                output-file-edit
                                z-scale-options
                                convert-button)
                :adjust :center
                :internal-border 20))
  (:menu-bar application-menu)
  (:default-initargs :title (format nil "Reflex Arena -> Quake 1 Map Converter ~a" *version*)
   :drop-callback 'on-drop
   :layout 'main-layout
   :initial-focus 'input-file-edit
   :visible-min-width 400
   ;;   :help-callback 'on-main-window-tooltip
   ))


(defmethod initialize-instance :after ((self reflex-map-converter-interface) &key &allow-other-keys)
  (setf (capi:capi-object-property self 'drag-and-drop-list-keyword) 
        :filename-list))


(defun on-convert-button (data self)
  (declare (ignore data))           
  (with-slots (input-file-edit
               output-file-edit
               convert-button
               z-scale-options) self
    (flet ((enable-interface (enable)
             (capi:apply-in-pane-process self
                                         (lambda ()
                                           (setf (button-enabled convert-button) enable
                                                 (text-input-pane-enabled input-file-edit) enable
                                                 (text-input-pane-enabled output-file-edit) enable
                                                 (simple-pane-cursor self) (if enable nil :wait))))))
      (let ((source-path (text-input-pane-text input-file-edit))
            (dest-path (text-input-pane-text output-file-edit))
            (z-scale (/ (read-from-string (choice-selected-item z-scale-options)) 100.0)))
        ;; verify what paths are not empty
        (when (and (> (length source-path) 0) (> (length dest-path) 0))
          (mp:process-run-function "Convert files" nil
                                   (lambda ()
                                     (enable-interface nil)
                                     (convert-reflex-to-qw source-path dest-path z-scale)
                                     (enable-interface t)
                                     (apply-in-pane-process self (lambda ()
                                                                   (display-message "Done"))))))))))


(defun on-drop (pane drop-object stage)
  (let ((keyword (capi:capi-object-property pane 'drag-and-drop-list-keyword)))
    (case stage
      (:formats
       (capi:set-drop-object-supported-formats drop-object (list keyword)))
      ((:drag :enter)
       (setf (capi:drop-object-drop-effect drop-object) :copy))
      (:drop
       ;; only use first from the list of files dropped
       (let ((filenames (capi:drop-object-get-object
                         drop-object pane keyword)))
         (with-slots (input-file-edit) pane
           (setf (text-input-pane-text input-file-edit) (car filenames))))))))


(defun main-ui ()
  (capi:display (make-instance ' reflex-map-converter-interface)))
