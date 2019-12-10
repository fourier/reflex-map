#|
  This file is a part of reflex-map project.

  (C) COPYRIGHT Alexey Veretennikov<alexey.veretennikov@gmail.com>, 2018
|#

(in-package :reflex-map)

;; shortcut/convenience macro
(defmacro with-pane-process ((self) &body body)
  `(capi:apply-in-pane-process ,self
                              (lambda ()
                                ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Implementation is taken from mediaimport project
(defclass settings ()
  ((company :reader company
            :initarg :company
            :initform "com.github.fourier"
            :documentation "Company name")
   (name :reader application-name
         :initarg :application-name
         :initform "reflex-map-converter"
         :documentation "Application name")
   (version :reader application-version
            :initarg :application-version
            :initform *version*
            :documentation "Application version")
   (product-symbol :reader product
    :documentation "A symbol produced from the company name")
   (settings-path :reader settings
                  :initform "Settings"
                  :initarg :settings-path
                  :documentation "A path to the generic application settings"))
  (:documentation "Settings class provides application-specific persistence settings"))


(defmethod initialize-instance :after ((self settings) &key)
  "Constructor for SETTINGS class"
  (with-slots (company name version product-symbol) self
    (setf version *version*)
    (setf product-symbol (intern name "KEYWORD"))
    (setf (sys:product-registry-path product-symbol)
          (list "Software" company name version))))


(defmethod get-value ((self settings) key &optional (fallback-value nil fallback-value-supplied-p) )
  "Get the value identified by KEY from the storage SELF.
If FALLBACK-VALUE specified, use this if not found (and update the storage)"
  (with-slots (product-symbol settings-path) self
    ;; handle paths like "Presets/Mypreset"
    (let ((path (split-sequence "/" key)))
      ;; if single key prepend with "Settings"
      (if (= 1 (length path))
          (setf path (list settings-path))
          ;; otherwise split the path and a key
          (setf key (car (last path))
                path (butlast path)))
      (multiple-value-bind (value result)
          (lw:user-preference path key :product product-symbol)
        ;; now if found just return result and indication that everything is alright
        (cond (result (values value result))
              ;; otherwise check if the fallback was provided
              (fallback-value-supplied-p
               (progn
                 (setf (lw:user-preference path key :product product-symbol) fallback-value)
                 (values (lw:user-preference path key :product product-symbol) t)))
              ;; and just return nil otherwise
              (t (values nil nil)))))))


(defmethod set-value ((self settings) key value)
  "Set and save the VALUE identified by the KEY in storage SELF."
  (with-slots (product-symbol settings-path) self
    (let ((path (split-sequence "/" key)))
      ;; if single key prepend with "Settings"
      (if (= 1 (length path))
          (setf path (list settings-path))
          ;; otherwise split the path and a key
          (setf key (car (last path))
                path (butlast path)))
      (setf (lw:user-preference path key :product product-symbol) value)
      value)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-interface reflex-map-converter-interface ()
  ((settings :initform (make-instance
                        'settings
                        :application-name "ReflexMapConverter" :application-version *version*)))
  (:menus
   (application-menu
    "File"
    ((:component
      (("About"
        :callback
        (lambda ()
          (display-message-on-screen
           (convert-to-screen nil)
           "Reflex Arena -> Quake 1 Map Converter ~a~%Copyright (c) Alexey Veretennikov(fourier) 2018-2019" *version*))
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
                    :test-function #'equalp
                    :selected-item "85"
                    :title "Z-scale, percents of original")
   (generate-options-panel check-button-panel
                           :visible-max-width nil
                           :visible-max-height nil
                           :items '((:round-to-integers . "Round coordinates to integers")
                                    (:export-lightsources . "Export light sources")
                                    (:export-items . "Export items")
                                    (:export-spawns . "Export spawns"))
                           :print-function #'cdr
                           :selection-callback (rcurry #'options-panel-callback t)
                           :retract-callback (rcurry #'options-panel-callback nil)
                           ;; used to compare items. first argument is the supplied by
                           ;; user's function value, i.e. :round-to-integers, second
                           ;; is the value from the :items, i.e. (:export-items . "Export items")
                           :test-function (lambda (x y) (eql x (car y)))
                           :layout-class 'column-layout
                           :layout-args '(:adjust :right ;;:internal-border 20
                                          :uniform-size-p t))
   (convert-button push-button :text "Convert" :callback 'on-convert-button))
  (:layouts
   (generate-options-layout column-layout
                            '(z-scale-options generate-options-panel)
                            :title "Generating options"
                            :title-position :frame)
   (main-layout column-layout '(input-file-edit
                                output-file-edit
                                generate-options-layout
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
  ;; set window drag-n-droppable, to allow drag-n-drop files[s] from Explorer
  (setf (capi:capi-object-property self 'drag-and-drop-list-keyword) 
        :filename-list)
  ;; restore the fields from registry (using settings class)
  (with-slots (input-file-edit
               output-file-edit
               z-scale-options
               settings
               generate-options-panel) self
    (macrolet ((get-field-from-settings (field accessor default-value)
                 ;; shortcut
                 `(setf (,accessor ,field)
                        (get-value settings ,(symbol-name field) ,default-value))))
      (get-field-from-settings input-file-edit text-input-pane-text "")
      (get-field-from-settings output-file-edit text-input-pane-text "")
      (get-field-from-settings z-scale-options choice-selected-item "85"))
    ;; fill the values for options checkboxes
    (let (options-selection)
      ;; shortcut
      (macrolet ((set-option (var keyword)
                   `(progn
                      (setf ,var (get-value settings (symbol-name ,keyword) ,var))
                      (when ,var
                        (push ,keyword options-selection)))))
        (set-option *round-to-integers* :round-to-integers)
        (set-option *export-lightsources* :export-lightsources)
        (set-option *export-items* :export-items)
        (set-option *export-spawns* :export-spawns))
      (setf (choice-selected-items generate-options-panel) options-selection))))


(defun options-panel-callback (data interface enabled)
  (declare (ignore interface))
  (let ((option (car data)))
    (case option
      (:round-to-integers (setf *round-to-integers* enabled))
      (:export-lightsources (setf *export-lightsources* enabled))
      (:export-items (setf *export-items* enabled))
      (:export-spawns (setf *export-spawns* enabled)))))


(defun on-convert-button (data self)
  "Callback called then the user press Convert button"
  (declare (ignore data))           
  (with-slots (input-file-edit
               output-file-edit
               convert-button
               z-scale-options
               settings) self
    (flet ((enable-interface (enable)
             ;; enable/disable fields and show/hide busy cursor
             (with-pane-process (self)
               (setf (button-enabled convert-button) enable
                     (text-input-pane-enabled input-file-edit) enable
                     (text-input-pane-enabled output-file-edit) enable
                     (simple-pane-cursor self) (if enable nil :wait)))))
      (let ((source-path (text-input-pane-text input-file-edit))
            (dest-path (text-input-pane-text output-file-edit))
            (z-scale (/ (read-from-string (choice-selected-item z-scale-options)) 100.0)))
        ;; verify what paths are not empty
        (when (and (> (length source-path) 0) (> (length dest-path) 0))
          ;; ok, now we can save the values to registry
          (macrolet ((set-field-from-settings (field accessor)
                       `(set-value settings ,(symbol-name field) (,accessor ,field))))
            (set-field-from-settings input-file-edit text-input-pane-text)
            (set-field-from-settings output-file-edit text-input-pane-text)
            (set-field-from-settings z-scale-options choice-selected-item)
            (set-value settings (symbol-name :round-to-integers) *round-to-integers*)
            (set-value settings (symbol-name :export-items) *export-items*)
            (set-value settings (symbol-name :export-lightsources) *export-lightsources*)
            (set-value settings (symbol-name :export-spawns) *export-spawns*)
            ;; and run the computation in separate thread
            (mp:process-run-function "Convert files" nil
                                     (lambda ()
                                       (enable-interface nil)
                                       ;; the main job done by this function
                                       (convert-reflex-to-qw source-path dest-path z-scale)
                                       (enable-interface t)
                                       (with-pane-process (self)
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


(defmethod setting-selected ((self reflex-map-converter-interface) option)
  "Check if the settings checkbox selected."
  (when-let (selected (mapcar #'car (choice-selected-items (slot-value self 'generate-options-panel))))
    (member option selected)))


(defun main-ui ()
  (capi:display (make-instance ' reflex-map-converter-interface)))
