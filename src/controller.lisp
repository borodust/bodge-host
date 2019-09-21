(cl:in-package :bodge-host)

;;;
;;; CONTROLLER
;;;
(defstruct (axis
            (:constructor %make-axis (id controller-id)))
  (id 0 :type fixnum :read-only t)
  (controller-id 0 :type fixnum :read-only t))


(defstruct (button
            (:constructor %make-button (id controller-id))
            (:include axis)))


(defstruct (hat
            (:constructor %make-hat (id controller-id))
            (:include axis)))


(defstruct (controller
            (:constructor %make-controller (id name guid axes buttons hats)))
  (id 0 :read-only t)
  (name "" :type string :read-only t)
  (guid "" :type string)
  (axes nil :type list :read-only t)
  (buttons nil :type list :read-only t)
  (hats nil :type list :read-only t))


(defun make-controller (id)
  (c-with ((axis-count :int)
           (hat-count :int)
           (button-count :int))
    (%glfw:get-joystick-axes id (axis-count &))
    (%glfw:get-joystick-hats id (hat-count &))
    (%glfw:get-joystick-buttons id (button-count &))
    (flet ((map-axes (constructor count)
             (loop for idx from 0 below count
                   collect (funcall constructor idx id))))
      (%make-controller id
                        (cffi:foreign-string-to-lisp (%glfw:get-joystick-name id))
                        (cffi:foreign-string-to-lisp (%glfw:get-joystick-guid id))
                        (map-axes #'%make-axis axis-count)
                        (map-axes #'%make-button button-count)
                        (map-axes #'%make-hat hat-count)))))


(defun controller-axis-value (axis)
  (check-host-thread)
  (c-with ((len :int))
    (let ((axis-values (%glfw:get-joystick-axes (axis-controller-id axis) (len &)))
          (idx (axis-id axis)))
      (c-val ((axis-values :float))
        (when (< -1 idx len)
          (* (+ (axis-values idx) 1) 0.5))))))


(defun controller-button-pressed-p (button)
  (check-host-thread)
  (c-with ((len :int))
    (let ((button-values (%glfw:get-joystick-buttons (button-controller-id button)
                                                     (len &)))
          (idx (axis-id button)))
      (c-val ((button-values :char))
        (and (< -1 idx len)
             (= %glfw:+press+ (button-values idx)))))))


(defun glfw->hat-state (state)
  (ecase state
    (%glfw:+hat-centered+ :centered)
    (%glfw:+hat-up+ :up)
    (%glfw:+hat-right+ :right)
    (%glfw:+hat-down+ :down)
    (%glfw:+hat-left+ :left)
    (%glfw:+hat-right-up+ :right-up)
    (%glfw:+hat-right-down+ :right-down)
    (%glfw:+hat-left-up+ :left-up)
    (%glfw:+hat-left-down+ :left-down)))


(defun controller-hat-state (hat)
  (check-host-thread)
  (c-with ((len :int))
    (let ((hat-values (%glfw:get-joystick-hats (axis-controller-id hat) (len &)))
          (idx (axis-id hat)))
      (c-val ((hat-values :char))
        (when (< -1 idx len)
          (glfw->hat-state (hat-values idx)))))))


;;;
;;; GAMEPAD
;;;
(defstruct (gamepad
            (:constructor %make-gamepad (id name guid %state)))
  (id 0 :type fixnum)
  (name "" :type string)
  (guid "" :type string)
  %state)


(defun make-gamepad (joystick-id)
  (c-let ((state %glfw:gamepadstate :alloc t :clear t))
    (%make-gamepad joystick-id
                   (cffi:foreign-string-to-lisp
                    (%glfw:get-gamepad-name joystick-id))
                   (cffi:foreign-string-to-lisp
                    (%glfw:get-joystick-guid joystick-id))
                   (state &))))


(defun destroy-gamepad (gamepad)
  (cffi:foreign-free (gamepad-%state gamepad)))


(defun gamepad-state (gamepad)
  (gamepad-%state gamepad))


(defun %gamepad-button-pressed-p (state button-id)
  (c-val ((state %glfw:gamepadstate))
    (= (state :buttons button-id) %glfw:+press+)))


(defun gamepad-button->nk (button)
  (ecase button
    (:a %glfw:+gamepad-button-a+)
    (:b %glfw:+gamepad-button-b+)
    (:x %glfw:+gamepad-button-x+)
    (:y %glfw:+gamepad-button-y+)
    (:left-bumper %glfw:+gamepad-button-left-bumper+)
    (:right-bumper %glfw:+gamepad-button-right-bumper+)
    (:start %glfw:+gamepad-button-start+)
    (:back %glfw:+gamepad-button-back+)
    (:guide %glfw:+gamepad-button-guide+)
    (:left-thumb %glfw:+gamepad-button-left-thumb+)
    (:right-thumb %glfw:+gamepad-button-right-thumb+)))


(defun gamepad-state-button-pressed-p (gamepad-state button)
  (%gamepad-button-pressed-p gamepad-state (gamepad-button->nk button)))


(defun gamepad-state-dpad (gamepad-state)
  (let ((up (%gamepad-button-pressed-p gamepad-state
                                       %glfw:+gamepad-button-dpad-up+))
        (down (%gamepad-button-pressed-p gamepad-state
                                         %glfw:+gamepad-button-dpad-down+))
        (left (%gamepad-button-pressed-p gamepad-state
                                         %glfw:+gamepad-button-dpad-left+))
        (right (%gamepad-button-pressed-p gamepad-state
                                          %glfw:+gamepad-button-dpad-right+)))
    (cond
      ((and right up) :right-up)
      ((and right down) :right-down)
      ((and left up) :left-up)
      ((and left down) :left-down)
      (up :up)
      (down :down)
      (left :left)
      (right :right)
      (t :centered))))


(defun gamepad-state-left-stick (gamepad-state &optional (result (vec2)))
  (c-val ((gamepad-state %glfw:gamepadstate))
    (setf (x result) (gamepad-state :axes %glfw:+gamepad-axis-left-x+)
          (y result) (- (gamepad-state :axes %glfw:+gamepad-axis-left-y+))))
  result)


(defun gamepad-state-right-stick (gamepad-state &optional (result (vec2)))
  (c-val ((gamepad-state %glfw:gamepadstate))
    (setf (x result) (gamepad-state :axes %glfw:+gamepad-axis-right-x+)
          (y result) (- (gamepad-state :axes %glfw:+gamepad-axis-right-y+))))
  result)


(defun gamepad-state-left-trigger (gamepad-state)
  (c-val ((gamepad-state %glfw:gamepadstate))
    (* (+ (gamepad-state :axes %glfw:+gamepad-axis-left-trigger+) 1) 0.5)))


(defun gamepad-state-right-trigger (gamepad-state)
  (c-val ((gamepad-state %glfw:gamepadstate))
    (* (+ (gamepad-state :axes %glfw:+gamepad-axis-right-trigger+) 1) 0.5)))


;;;
;;; CONTROLLER HUB
;;;
(defun make-controller-hub ()
  (check-host-thread)
  (loop with controller-hub = (make-hash-table)
        for joystick-id from %glfw:+joystick-1+ upto %glfw:+joystick-last+
        when (= (%glfw:joystick-present joystick-id) %glfw:+true+)
          do (register-controller controller-hub joystick-id)
        finally (return controller-hub)))


(defun register-controller (hub joystick-id)
  (let ((controller (make-controller joystick-id)))
    (setf (gethash joystick-id hub) (cons controller nil))
    (unless (= (%glfw:joystick-is-gamepad joystick-id) %glfw:+false+)
      (let ((gamepad (make-gamepad joystick-id)))
        (setf (cdr (gethash joystick-id hub)) gamepad)))))


(defun remove-controller (hub joystick-id)
  (destructuring-bind (controller . gamepad) (gethash joystick-id hub)
    (declare (ignore controller))
    (when gamepad
      (destroy-gamepad gamepad))
    (remhash joystick-id hub)))


(defun destroy-controller-hub (hub)
  (loop for controller being the hash-value of hub
        for controller-id = (controller-id (car controller))
        do (remove-controller hub controller-id)))


(defun controller-hub-controllers (hub)
  (loop for controller being the hash-value of hub
        collect (car controller)))


(defun controller-hub-gamepads (hub)
  (loop for controller being the hash-value of hub
        for gamepad = (cdr controller)
        when gamepad
          collect gamepad))


(defun find-controller (hub controller-id)
  (car (gethash controller-id hub)))


(defun find-gamepad (hub gamepad-id)
  (cdr (gethash gamepad-id hub)))


(defun for-each-updated-gamepad (hub fu)
  (c-with ((tmp-state %glfw:gamepadstate :clear t))
    (loop for controller being the hash-value of hub
          for gamepad = (cdr controller)
          when gamepad
            do (%glfw:get-gamepad-state (gamepad-id gamepad) (tmp-state &))
               (unless (= (%libc.es:memcmp (tmp-state &) (gamepad-%state gamepad)
                                           (cffi:foreign-type-size '%glfw:gamepadstate))
                          0)
                 (funcall fu gamepad (gamepad-%state gamepad) (tmp-state &))
                 (%libc.es:memcpy (gamepad-%state gamepad) (tmp-state &)
                                  (cffi:foreign-type-size '%glfw:gamepadstate))))))
