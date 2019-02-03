(cl:in-package :bodge-host)

(glfw:define-window-close-callback on-close (window)
  (%glfw:hide-window window)
  (on-hide (find-window-by-handle window)))


(glfw:define-key-callback on-key-action (window key scancode action mod-keys)
  (declare (ignore scancode mod-keys))
  (on-key-action (find-window-by-handle window)
                 (glfw-enumval->keyboard-key key)
                 (glfw-enumval->button-state action)))


(glfw:define-mouse-button-callback on-mouse-action (window button action mod-keys)
  (declare (ignore mod-keys))
  (on-mouse-action (find-window-by-handle window)
                   (glfw-enumval->mouse-button button) (glfw-enumval->button-state action)))


(glfw:define-cursor-pos-callback on-cursor-movement (window x y)
  (claw:c-with ((height :int))
    (%glfw:get-window-size window nil (height &))
    (let* ((win (find-window-by-handle window))
           (scale (%viewport-autoscale win)))
      (on-cursor-movement win (/ x scale) (/ (- height y) scale)))))


(glfw:define-scroll-callback on-scroll (window x y)
  (let* ((win (find-window-by-handle window))
         (scale (%viewport-autoscale win)))
    (on-scroll win (/ x scale) (/ (- y) scale))))


(glfw:define-framebuffer-size-callback on-framebuffer-size-change (window w h)
  (on-framebuffer-size-change (find-window-by-handle window) w h))


(glfw:define-window-size-callback on-viewport-size-change (window w h)
  (let ((scale (%viewport-autoscale (find-window-by-handle window))))
    (on-viewport-size-change (find-window-by-handle window)
                             (floor (/ w scale))
                             (floor (/ h scale)))))


(glfw:define-char-callback on-character-input (window char-code)
  (let ((character (code-char char-code)))
    (on-character-input (find-window-by-handle window) character)))


(claw:defcallback on-glfw-error :void ((code :int) (error-string :pointer))
  (warn "GLFW error ~A: ~A" code (cffi:foreign-string-to-lisp error-string)))


(defun init-callbacks (window)
  (%glfw:set-window-close-callback window (claw:callback 'on-close))
  (%glfw:set-key-callback window (claw:callback 'on-key-action))
  (%glfw:set-mouse-button-callback window (claw:callback 'on-mouse-action))
  (%glfw:set-cursor-pos-callback window (claw:callback 'on-cursor-movement))
  (%glfw:set-scroll-callback window (claw:callback 'on-scroll))
  (%glfw:set-framebuffer-size-callback window (claw:callback 'on-framebuffer-size-change))
  (%glfw:set-window-size-callback window (claw:callback 'on-viewport-size-change))
  (%glfw:set-char-callback window (claw:callback 'on-character-input)))
