(cl:in-package :bodge-host)

(bodge-util:define-constant +default-window-width+ 640)

(bodge-util:define-constant +default-window-height+ 480)

(defclass window ()
  ((handle :initform nil :reader %handle-of)
   (cursor :initform nil)
   (gl-major-version :initform 3)
   (gl-minor-version :initform 3)
   (title :initform nil :initarg :title)
   (height :initform nil :initarg :height)
   (width :initform nil :initarg :width)
   (resizable :initform nil :initarg :resizable)
   (decorated :initform t :initarg :decorated)
   (transparent :initform nil :initarg :transparent)
   (floating :initform nil :initarg :floating)
   (maximized :initform nil :initarg :maximized)
   (samples :initform nil :initarg :samples)
   (position :initform nil :initarg :position)
   (autoscaled :initform t :initarg :autoscaled :reader viewport-autoscaled-p)))


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


(defun %bool (val)
  (if val %glfw:+true+ %glfw:+false+))


(defun create-window (width height title gl-major-version gl-minor-version
                      &key (shared (cffi:null-pointer)) (visible nil) (samples nil) (decorated t)
                        (resizable nil) (transparent nil) (floating nil) (maximized nil) (focused t))
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
        (unless (= gl-major-version 2)
          (%glfw:window-hint %glfw:+opengl-profile+ %glfw:+opengl-core-profile+)
          (%glfw:window-hint %glfw:+opengl-forward-compat+ %glfw:+true+))))
  (when samples
    (%glfw:window-hint %glfw:+samples+ samples))
  (glfw:with-window-hints ((%glfw:+alpha-bits+ 8)
                           (%glfw:+depth-bits+ 24)
                           (%glfw:+stencil-bits+ 8)
                           (%glfw:+resizable+ (%bool resizable))
                           (%glfw:+decorated+ (%bool decorated))
                           (%glfw:+doublebuffer+ (%bool visible))
                           (%glfw:+client-api+ %glfw:+opengl-api+)
                           (%glfw:+context-creation-api+ %glfw:+native-context-api+)
                           (%glfw:+visible+ (%bool visible))
                           (%glfw:+floating+ (%bool floating))
                           (%glfw:+maximized+ (%bool maximized))
                           (%glfw:+focused+ (%bool focused))
                           (%glfw:+transparent-framebuffer+ (%bool transparent)))
    (let ((win (%glfw:create-window (round width)
                                    (round height)
                                    title
                                    (cffi:null-pointer)
                                    shared)))
      (when (and transparent
                 (= %glfw:+false+ (%glfw:get-window-attrib win %glfw:+transparent-framebuffer+)))
        (warn "Transparency requested, but not supported"))
      (%glfw:make-context-current (cffi:null-pointer))
      win)))


(defun init-window (window)
  (with-slots ((this-handle handle) gl-major-version gl-minor-version
               width height title
               autoscaled resizable decorated transparent
               floating maximized samples position)
      window
    (on-log window :debug "Initializing GLFW context for OpenGL version ~A.~A"
            gl-major-version gl-minor-version)
    (let* ((scale (if autoscaled (primary-monitor-content-scale) 1f0))
           (width (* (or width +default-window-width+) scale))
           (height (* (or height +default-window-height+) scale))
           (handle (create-window width height
                                  (or title "Bodge Window")
                                  gl-major-version gl-minor-version :visible t
                                  :resizable resizable
                                  :decorated decorated
                                  :transparent transparent
                                  :floating floating
                                  :maximized maximized
                                  :samples samples)))
      (unless handle
        (error "Failed to create main window. Please, check OpenGL version. Requested: ~A.~A"
               gl-major-version gl-minor-version))
      (init-callbacks handle)
      (setf this-handle handle)
      (when position
        (setf (viewport-position window) position))
      (on-init window))))


(defun destroy-window (window)
  (with-slots (handle) window
    (unwind-protect
         (on-destroy window)
      (bind-main-rendering-context window)
      (%glfw:destroy-window handle)
      (setf handle nil))))


(defun make-shared-context (window gl-major-version gl-minor-version)
  (create-window 1 1 "" gl-major-version gl-minor-version :shared window))


(defun calc-dpi-scale (monitor expected-dpi)
  (claw:c-let ((mon-width :int)
               (video-mode %glfw:vidmode :from (%glfw:get-video-mode monitor)))
    (%glfw:get-monitor-physical-size monitor (mon-width &) (claw:ptr nil))
    (let* ((current-dpi (/ (video-mode :width) (/ mon-width 25.4))))
      (max (f (round (/ current-dpi expected-dpi))) 1f0))))


(defun calc-scale (handle expected-dpi)
  (claw:c-let ((fb-width :int)
               (win-width :int))
    (%glfw:get-framebuffer-size handle (fb-width &) (claw:ptr nil))
    (%glfw:get-window-size handle (win-width &) (claw:ptr nil))
    (if (> fb-width win-width)
        1f0
        (calc-dpi-scale (%glfw:get-primary-monitor) expected-dpi))))


(defun swap-buffers (window)
  (with-slots (handle) window
    (%glfw:swap-buffers handle)))


(defun (setf viewport-title) (value window)
  (with-slots (handle) window
    ;; some darwin systems go crazy throwing FPE around while setting a title
    (claw:with-float-traps-masked ()
      (%glfw:set-window-title handle (format nil "~a" value))
      value)))


(defun %viewport-dimensions (window)
  (claw:c-with ((width :int)
                (height :int))
    (%glfw:get-window-size (%handle-of window) (width &) (height &))
    (let ((scale (%viewport-autoscale window)))
      (values (round (/ width scale))
              (round (/ height scale))))))


(defmacro with-viewport-dimensions ((width height) window &body body)
  `(multiple-value-bind (,width ,height) (%viewport-dimensions ,window)
     (declare (ignorable ,width ,height))
     ,@body))


(defun viewport-size (window &optional (result-vec (vec2)))
  (with-viewport-dimensions (width height) window
    (setf (x result-vec) width
          (y result-vec) height))
  result-vec)


(defun select-monitor (x-win y-win)
  (claw:c-with ((x-monitor :int)
                (y-monitor :int))
    (flet ((%intersecting-p (x-origin y-origin rect-width rect-height)
             (let ((x-len (- x-win x-origin))
                   (y-len (- y-win y-origin)))
               (and (<= 0 x-len rect-width)
                    (<= 0 y-len rect-height)))))
      (loop for monitor in (bodge-host:available-monitors)
            for video-mode = (bodge-host:monitor-video-mode monitor)
              thereis (progn
                        (%glfw:get-monitor-pos monitor (x-monitor &) (y-monitor &))
                        (and (%intersecting-p x-monitor y-monitor
                                              (bodge-host:video-mode-width video-mode)
                                              (bodge-host:video-mode-height video-mode))
                             monitor))
            finally (return (bodge-host:primary-monitor))))))


(defun %window-monitor (win-handle)
  (claw:c-with ((x-pos :int)
                (y-pos :int))
    (%glfw:get-window-pos win-handle (x-pos &) (y-pos &))
    (let ((reported-monitor (%glfw:get-window-monitor win-handle)))
      (if (claw:null-pointer-p reported-monitor)
          (select-monitor x-pos y-pos)
          reported-monitor))))


(defun window-monitor (window)
  (%window-monitor (%handle-of window)))


(defun (setf viewport-position) (value window)
  (let* ((monitor (window-monitor window))
         (monitor-height (video-mode-height (monitor-video-mode monitor))))
    (claw:c-with ((height :int))
      (%glfw:get-window-size (%handle-of window) nil (height &))
      (%glfw:set-window-pos (%handle-of window)
                            (round (x value))
                            (round (- monitor-height (+ (y value) height))))))
  value)

(defun viewport-position (window &optional (result-vec (vec2)))
  (let* ((monitor (window-monitor window))
         (monitor-height (video-mode-height (monitor-video-mode monitor))))
    (claw:c-with ((height :int)
                  (x-pos :int)
                  (y-pos :int))
      (%glfw:get-window-size (%handle-of window) nil (height &))
      (%glfw:get-window-pos (%handle-of window) (x-pos &) (y-pos &))
      (setf (x result-vec) x-pos
            (y result-vec) (- monitor-height (+ y-pos height)))))
  result-vec)


(defun %framebuffer-dimensions (window)
  (claw:c-with ((width :int)
                (height :int))
    (%glfw:get-framebuffer-size (%handle-of window) (width &) (height &))
    (values width height)))


(defmacro with-framebuffer-dimensions ((width height) window &body body)
  `(multiple-value-bind (,width ,height) (%framebuffer-dimensions ,window)
     (declare (ignorable ,width ,height))
     ,@body))


(defun framebuffer-size (window &optional (result-vec (vec2)))
  (with-framebuffer-dimensions (width height) window
    (setf (x result-vec) width
          (y result-vec) height))
  result-vec)


(defun (setf viewport-size) (value window)
  ;; same as with #'(setf viewport-title)
  ;; some darwin systems go nuts throwing FPE around while setting a size
  (let ((scale (%viewport-autoscale window)))
    (claw:with-float-traps-masked ()
      (%glfw:set-window-size (%handle-of window)
                             (round (* (x value) scale))
                             (round (* (y value) scale)))))
  value)


(defun cursor-position (window &optional (result-vec (vec2)))
  (claw:c-with ((x-pos :double)
                (y-pos :double)
                (height :int))
    (%glfw:get-window-size (%handle-of window) nil (height &))
    (%glfw:get-cursor-pos (%handle-of window) (x-pos &) (y-pos &))
    (let ((scale (%viewport-autoscale window)))
      (setf (x result-vec) (/ x-pos scale)
            (y result-vec) (/ (- height y-pos) scale))))
  result-vec)


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


(defun show-window (window)
  (with-slots (handle) window
    (%glfw:show-window handle)))


(defun hide-window (window)
  (with-slots (handle) window
    (%glfw:hide-window handle)))


(defun viewport-scale (window &optional expected-dpi)
  (with-slots (handle) window
    (if expected-dpi
        (calc-scale handle expected-dpi)
        (monitor-content-scale (window-monitor window)))))


(defun %viewport-autoscale (window)
  (with-slots (autoscaled) window
    (if autoscaled
        (viewport-scale window)
        1f0)))


(defun (setf fullscreen-viewport-p) (value window)
  (with-slots (handle) window
    (if value
        (let* ((monitor (%glfw:get-primary-monitor)))
          (claw:c-let ((video-mode %glfw:vidmode :from (%glfw:get-video-mode monitor)))
            (%glfw:set-window-monitor handle monitor 0 0
                                      (video-mode :width)
                                      (video-mode :height)
                                      (video-mode :refresh-rate))))
        (%glfw:set-window-monitor handle (cffi:null-pointer) 100 100 640 480 %glfw:+dont-care+)))
  value)


(defun make-shared-rendering-context (window)
  (with-slots (handle gl-major-version gl-minor-version) window
    (make-shared-context handle gl-major-version gl-minor-version)))


(defun bind-shared-rendering-context (shared-context)
  (%glfw:make-context-current shared-context))


(defun destroy-shared-rendering-context (context)
  (bind-shared-rendering-context context)
  (%glfw:destroy-window context))


(defun release-rendering-context ()
  (%glfw:make-context-current (cffi:null-pointer)))


(defun cursor (window)
  (with-slots (cursor) window
    cursor))


(defun (setf cursor) (cursor window)
  (with-slots ((this-cursor cursor)) window
    (%glfw:set-cursor (%handle-of window) cursor)
    (setf this-cursor cursor)))


(defun register-controller-hub (hub))

(defun remove-controller-hub (hub))
