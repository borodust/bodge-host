(cl:in-package :bodge-host)


(defstruct (controller
            (:constructor %make-controller (id name axis-count button-count hat-count)))
  (id 0 :read-only t)
  (name "" :read-only t)
  (axis-count 0 :read-only t)
  (button-count 0 :read-only t)
  (hat-count 0 :read-only t))


(defun make-controller (id)
  (claw:c-with ((axis-count :int)
                (hat-count :int)
                (button-count :int))
    (%glfw:get-joystick-axes id (axis-count &))
    (%glfw:get-joystick-hats id (hat-count &))
    (%glfw:get-joystick-buttons id (button-count &))
    (%make-controller id
                      (cffi:foreign-string-to-lisp (%glfw:get-joystick-name id))
                      axis-count
                      button-count
                      hat-count)))


(defun find-controller (hub controller-id)
  (gethash controller-id hub))


(defun for-each-controller (hub fu)
  (loop for controller being the hash-value of hub
        do (log-errors (funcall fu controller))))


(defun make-controller-hub ()
  (check-host-thread)
  (%glfw:set-joystick-callback (claw:callback 'on-joystick-event))
  (loop with controller-hub = (make-hash-table)
        for joystick-id from %glfw:+joystick-1+ upto %glfw:+joystick-last+
        when (= (%glfw:joystick-present joystick-id) %glfw:+true+)
          do (setf (gethash joystick-id controller-hub) (make-controller joystick-id))
        finally (return controller-hub)))


(defun destroy-controller-hub (hub)
  (declare (ignore hub))
  (check-host-thread)
  (%glfw:set-joystick-callback nil))


(defun controller-axis-value (controller idx)
  (check-host-thread)
  (claw:c-let ((len :int :from *foreign-int-place*))
    (let ((axis-values (%glfw:get-joystick-axes (controller-id controller) (len &))))
      (claw:c-val ((axis-values :float))
        (when (< -1 idx len)
          (axis-values idx))))))


(defun controller-button-pressed-p (controller idx)
  (check-host-thread)
  (claw:c-let ((len :int :from *foreign-int-place*))
    (let ((button-values (%glfw:get-joystick-buttons (controller-id controller) (len &))))
      (claw:c-val ((button-values :char))
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


(defun controller-hat-state (controller idx)
  (check-host-thread)
  (claw:c-let ((len :int :from *foreign-int-place*))
    (let ((hat-values (%glfw:get-joystick-hats (controller-id controller) (len &))))
      (claw:c-val ((hat-values :char))
        (when (< -1 idx len)
          (glfw->hat-state (hat-values idx)))))))


(defgeneric on-controller-connect (listener controller)
  (:method (listener controller) (declare (ignore listener controller))))


(defgeneric on-controller-disconnect (listener controller)
  (:method (listener controller) (declare (ignore listener controller))))
