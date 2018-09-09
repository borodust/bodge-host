(cl:in-package :bodge-host)

(defclass application ()
  ((window :initform nil :reader %window-of)
   (gl-major-version :initform 3)
   (gl-minor-version :initform 3)
   (title :initform nil :initarg :viewport-title)
   (height :initform nil :initarg :viewport-height)
   (width :initform nil :initarg :viewport-width)))


(defmethod initialize-instance :after ((this application) &key opengl-version)
  (with-slots (gl-major-version gl-minor-version) this
    (setf gl-major-version (or (first opengl-version) 3)
          gl-minor-version (or (second opengl-version) 3))))


(defgeneric on-init (application)
  (:method (app) (declare (ignore app))))


(defgeneric on-destroy (application)
  (:method (app) (declare (ignore app))))


(defgeneric on-log (application level control-string &rest arguments)
  (:method (application level control-string &rest arguments)
    (declare (ignore application level control-string arguments))))


(defgeneric on-hide (application)
  (:method (app) (declare (ignore app))))


(defgeneric on-key-action (application key state)
  (:method (app key state) (declare (ignore app key state))))


(defgeneric on-mouse-action (application button state)
  (:method (app key state) (declare (ignore app key state))))


(defgeneric on-cursor-movement (application x y)
  (:method (app x y) (declare (ignore app x y))))


(defgeneric on-scroll (application x-offset y-offset)
  (:method (app x y) (declare (ignore app x y))))


(defgeneric on-framebuffer-size-change (application width height)
  (:method (app w h) (declare (ignore app w h))))


(defgeneric on-viewport-size-change (application width height)
  (:method (app w h) (declare (ignore app w h))))


(defgeneric on-character-input (application character)
  (:method (app c) (declare (ignore app c))))


(defun create-window (width height title gl-major-version gl-minor-version
                      &key (shared (cffi:null-pointer)) (visible nil) (samples 1))
  (if (featurep :bodge-gl2)
      (progn
        (unless (and (= gl-major-version 2) (= gl-minor-version 1))
          (warn ":bodge-gl2 feature detected, forcing OpenGL 2.1 context (~A.~A requested)"
                gl-major-version gl-minor-version))
        (%glfw:window-hint %glfw:+context-version-major+ 2)
        (%glfw:window-hint %glfw:+context-version-minor+ 1))
      (progn
        (%glfw:window-hint %glfw:+context-version-major+ gl-major-version)
        (%glfw:window-hint %glfw:+context-version-minor+ gl-minor-version)
        (%glfw:window-hint %glfw:+opengl-profile+ %glfw:+opengl-core-profile+)
        (%glfw:window-hint %glfw:+opengl-forward-compat+ %glfw:+true+)))
  (glfw:with-window-hints ((%glfw:+depth-bits+ 24)
                           (%glfw:+stencil-bits+ 8)
                           (%glfw:+resizable+ %glfw:+false+)
                           (%glfw:+doublebuffer+ (if visible %glfw:+true+ %glfw:+false+))
                           (%glfw:+client-api+ %glfw:+opengl-api+)
                           (%glfw:+context-creation-api+ %glfw:+native-context-api+)
                           (%glfw:+samples+ samples)
                           (%glfw:+visible+ (if visible %glfw:+true+ %glfw:+false+)))
    (prog1 (%glfw:create-window width height title (cffi:null-pointer) shared)
      (%glfw:make-context-current (cffi:null-pointer)))))


(defun init-application (application)
  (with-slots ((this-window window) gl-major-version gl-minor-version width height title) application
    (on-log application :debug "Initializing GLFW context for OpenGL version ~A.~A"
            gl-major-version gl-minor-version)
    (let ((window (create-window (or width 640) (or height 480) (or title "Bodge Window")
                                 gl-major-version gl-minor-version :visible t)))
      (unless window
        (error "Failed to create main window. Please, check OpenGL version. Requested: ~A.~A"
               gl-major-version gl-minor-version))
      (init-callbacks window)
      (setf this-window window)
      (on-init application))))


(defun destroy-application (application)
  (with-slots (window) application
    (on-destroy application)
    (when window
      (%glfw:destroy-window window))))


(defun make-shared-context (window gl-major-version gl-minor-version)
  (create-window 1 1 "" gl-major-version gl-minor-version :shared window))


(defun calc-dpi-scale (monitor)
  (claw:c-let ((mon-width :int)
               (video-mode %glfw:vidmode :from (%glfw:get-video-mode monitor)))
    (%glfw:get-monitor-physical-size monitor (mon-width &) (claw:ptr nil))
    (let* ((current-dpi (/ (video-mode :width) (/ mon-width 25.4))))
      (max (f (floor (/ current-dpi +expected-dpi+))) 1f0))))


(defun calc-scale (window)
  (claw:c-let ((fb-width :int)
               (win-width :int))
    (%glfw:get-framebuffer-size window (fb-width &) (claw:ptr nil))
    (%glfw:get-window-size window (win-width &) (claw:ptr nil))
    (if (> fb-width win-width)
        1f0
        (calc-dpi-scale (%glfw:get-primary-monitor)))))


(defun swap-buffers (application)
  (with-slots (window) application
    (%glfw:swap-buffers window)))


(defun (setf viewport-title) (value application)
  (with-slots (window) application
    ;; some darwin systems go crazy throwing FPE around while setting a title
    (claw:with-float-traps-masked ()
      (%glfw:set-window-title window (format nil "~a" value))
      value)))


(defun viewport-size (application)
  (claw:c-with ((width :int)
                (height :int))
    (%glfw:get-window-size (%window-of application) (width &) (height &))
    (vec2 width height)))


(defun framebuffer-size (application)
  (claw:c-with ((width :int)
                (height :int))
    (%glfw:get-framebuffer-size (%window-of application) (width &) (height &))
    (vec2 width height)))


(defun (setf viewport-size) (value application)
  ;; same as with #'(setf viewport-title)
  ;; some darwin systems go nuts throwing FPE around while setting a size
  (claw:with-float-traps-masked ()
    (%glfw:set-window-size (%window-of application) (floor (x value)) (floor (y value)))))


(defun cursor-position (application &optional (result-vec (vec2)))
  (let ((height (y (viewport-size application))))
    (claw:c-with ((x :double)
                  (y :double))
      (%glfw:get-cursor-pos (%window-of application) (x &) (y &))
      (setf (x result-vec) x
            (y result-vec) (- height y))
      result-vec)))


(defun mouse-button-state (application button)
  (glfw-enumval->button-state
   (%glfw:get-mouse-button (%window-of application) (mouse-button->glfw-enumval button))))


(defun keyboard-button-state (application button)
  (glfw-enumval->keyboard-key
   (%glfw:get-key (%window-of application) (keyboard-key->glfw-enumval button))))


(defun lock-cursor (application)
  (with-slots (window) application
    (%glfw:set-input-mode window %glfw:+cursor+ %glfw:+cursor-disabled+)))


(defun unlock-cursor (application)
  (with-slots (window) application
    (%glfw:set-input-mode window %glfw:+cursor+ %glfw:+cursor-normal+)))


(defun viewport-scale (application)
  (with-slots (window) application
    (calc-scale window)))


(defun (setf fullscreen-viewport-p) (value application)
  (with-slots (window) application
    (if value
        (let* ((monitor (%glfw:get-primary-monitor)))
          (claw:c-let ((video-mode %glfw:vidmode :from (%glfw:get-video-mode monitor)))
            (%glfw:set-window-monitor window monitor 0 0
                                      (video-mode :width)
                                      (video-mode :height)
                                      (video-mode :refresh-rate))))
        (%glfw:set-window-monitor window (cffi:null-pointer) 100 100 640 480 %glfw:+dont-care+))))


(defun make-shared-rendering-context (application)
  (with-slots (window gl-major-version gl-minor-version) application
    (make-shared-context window gl-major-version gl-minor-version)))


(defun destroy-shared-rendering-context (context)
  (%glfw:destroy-window context))


(defun bind-shared-rendering-context (shared-context)
  (%glfw:make-context-current shared-context))


(defun release-rendering-context ()
  (%glfw:make-context-current (cffi:null-pointer)))
