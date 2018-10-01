(cl:in-package :bodge-host)

(defclass window ()
  ((handle :initform nil :reader %handle-of)
   (gl-major-version :initform 3)
   (gl-minor-version :initform 3)
   (title :initform nil :initarg :viewport-title)
   (height :initform nil :initarg :viewport-height)
   (width :initform nil :initarg :viewport-width)
   (resizable :initform nil :initarg :viewport-resizable)
   (decorated :initform t :initarg :viewport-decorated)))


(defmethod initialize-instance :after ((this window) &key opengl-version)
  (with-slots (gl-major-version gl-minor-version) this
    (setf gl-major-version (or (first opengl-version) 3)
          gl-minor-version (or (second opengl-version) 3))))


(defgeneric on-init (window)
  (:method (app) (declare (ignore app))))


(defgeneric on-destroy (window)
  (:method (app) (declare (ignore app))))


(defgeneric on-log (window level control-string &rest arguments)
  (:method (handle level control-string &rest arguments)
    (declare (ignore handle level control-string arguments))))


(defgeneric on-hide (window)
  (:method (app) (declare (ignore app))))


(defgeneric on-key-action (window key state)
  (:method (app key state) (declare (ignore app key state))))


(defgeneric on-mouse-action (window button state)
  (:method (app key state) (declare (ignore app key state))))


(defgeneric on-cursor-movement (window x y)
  (:method (app x y) (declare (ignore app x y))))


(defgeneric on-scroll (window x-offset y-offset)
  (:method (app x y) (declare (ignore app x y))))


(defgeneric on-framebuffer-size-change (window width height)
  (:method (app w h) (declare (ignore app w h))))


(defgeneric on-viewport-size-change (window width height)
  (:method (app w h) (declare (ignore app w h))))


(defgeneric on-character-input (window character)
  (:method (app c) (declare (ignore app c))))


(defun create-window (width height title gl-major-version gl-minor-version
                      &key (shared (cffi:null-pointer)) (visible nil) (samples 1) (decorated t)
                        (resizable nil))
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
                           (%glfw:+resizable+ (if resizable %glfw:+true+ %glfw:+false+))
                           (%glfw:+decorated+ (if decorated %glfw:+true+ %glfw:+false+))
                           (%glfw:+doublebuffer+ (if visible %glfw:+true+ %glfw:+false+))
                           (%glfw:+client-api+ %glfw:+opengl-api+)
                           (%glfw:+context-creation-api+ %glfw:+native-context-api+)
                           (%glfw:+samples+ samples)
                           (%glfw:+visible+ (if visible %glfw:+true+ %glfw:+false+)))
    (prog1 (%glfw:create-window width height title (cffi:null-pointer) shared)
      (%glfw:make-context-current (cffi:null-pointer)))))


(defun init-window (window)
  (with-slots ((this-handle handle) gl-major-version gl-minor-version width height title
               resizable decorated)
      window
    (on-log window :debug "Initializing GLFW context for OpenGL version ~A.~A"
            gl-major-version gl-minor-version)
    (let ((handle (create-window (or width 640) (or height 480) (or title "Bodge Window")
                                 gl-major-version gl-minor-version :visible t
                                                                   :resizable resizable
                                                                   :decorated decorated)))
      (unless handle
        (error "Failed to create main window. Please, check OpenGL version. Requested: ~A.~A"
               gl-major-version gl-minor-version))
      (init-callbacks handle)
      (setf this-handle handle)
      (on-init window))))


(defun destroy-window (window)
  (with-slots (handle) window
    (on-destroy window)
    (when handle
      (%glfw:destroy-window handle))))


(defun make-shared-context (window gl-major-version gl-minor-version)
  (create-window 1 1 "" gl-major-version gl-minor-version :shared window))


(defun calc-dpi-scale (monitor)
  (claw:c-let ((mon-width :int)
               (video-mode %glfw:vidmode :from (%glfw:get-video-mode monitor)))
    (%glfw:get-monitor-physical-size monitor (mon-width &) (claw:ptr nil))
    (let* ((current-dpi (/ (video-mode :width) (/ mon-width 25.4))))
      (max (f (floor (/ current-dpi +expected-dpi+))) 1f0))))


(defun calc-scale (handle)
  (claw:c-let ((fb-width :int)
               (win-width :int))
    (%glfw:get-framebuffer-size handle (fb-width &) (claw:ptr nil))
    (%glfw:get-window-size handle (win-width &) (claw:ptr nil))
    (if (> fb-width win-width)
        1f0
        (calc-dpi-scale (%glfw:get-primary-monitor)))))


(defun swap-buffers (window)
  (with-slots (handle) window
    (%glfw:swap-buffers handle)))


(defun (setf viewport-title) (value window)
  (with-slots (handle) window
    ;; some darwin systems go crazy throwing FPE around while setting a title
    (claw:with-float-traps-masked ()
      (%glfw:set-window-title handle (format nil "~a" value))
      value)))


(defun viewport-size (window)
  (claw:c-with ((width :int)
                (height :int))
    (%glfw:get-window-size (%handle-of window) (width &) (height &))
    (vec2 width height)))


(defun framebuffer-size (window)
  (claw:c-with ((width :int)
                (height :int))
    (%glfw:get-framebuffer-size (%handle-of window) (width &) (height &))
    (vec2 width height)))


(defun (setf viewport-size) (value window)
  ;; same as with #'(setf viewport-title)
  ;; some darwin systems go nuts throwing FPE around while setting a size
  (claw:with-float-traps-masked ()
    (%glfw:set-window-size (%handle-of window) (floor (x value)) (floor (y value)))))


(defun cursor-position (window &optional (result-vec (vec2)))
  (let ((height (y (viewport-size window))))
    (claw:c-with ((x :double)
                  (y :double))
      (%glfw:get-cursor-pos (%handle-of window) (x &) (y &))
      (setf (x result-vec) x
            (y result-vec) (- height y))
      result-vec)))


(defun mouse-button-state (window button)
  (glfw-enumval->button-state
   (%glfw:get-mouse-button (%handle-of window) (mouse-button->glfw-enumval button))))


(defun keyboard-button-state (window button)
  (glfw-enumval->keyboard-key
   (%glfw:get-key (%handle-of window) (keyboard-key->glfw-enumval button))))


(defun lock-cursor (window)
  (with-slots (handle) window
    (%glfw:set-input-mode handle %glfw:+cursor+ %glfw:+cursor-disabled+)))


(defun unlock-cursor (window)
  (with-slots (handle) window
    (%glfw:set-input-mode handle %glfw:+cursor+ %glfw:+cursor-normal+)))


(defun viewport-scale (window)
  (with-slots (handle) window
    (calc-scale handle)))


(defun (setf fullscreen-viewport-p) (value window)
  (with-slots (handle) window
    (if value
        (let* ((monitor (%glfw:get-primary-monitor)))
          (claw:c-let ((video-mode %glfw:vidmode :from (%glfw:get-video-mode monitor)))
            (%glfw:set-window-monitor handle monitor 0 0
                                      (video-mode :width)
                                      (video-mode :height)
                                      (video-mode :refresh-rate))))
        (%glfw:set-window-monitor handle (cffi:null-pointer) 100 100 640 480 %glfw:+dont-care+))))


(defun make-shared-rendering-context (window)
  (with-slots (handle gl-major-version gl-minor-version) window
    (make-shared-context handle gl-major-version gl-minor-version)))


(defun destroy-shared-rendering-context (context)
  (%glfw:destroy-window context))


(defun bind-shared-rendering-context (shared-context)
  (%glfw:make-context-current shared-context))


(defun release-rendering-context ()
  (%glfw:make-context-current (cffi:null-pointer)))
