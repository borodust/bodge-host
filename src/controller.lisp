(cl:in-package :bodge-host)

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
            (:constructor %make-controller (id name axes buttons hats)))
  (id 0 :read-only t)
  (name "" :type string :read-only t)
  (axes nil :type list :read-only t)
  (buttons nil :type list :read-only t)
  (hats nil :type list :read-only t))


(defun make-controller (id)
  (claw:c-with ((axis-count :int)
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
                        (map-axes #'%make-axis axis-count)
                        (map-axes #'%make-button button-count)
                        (map-axes #'%make-hat hat-count)))))


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


(defun controller-axis-value (axis)
  (check-host-thread)
  (claw:c-let ((len :int :from *foreign-int-place*))
    (let ((axis-values (%glfw:get-joystick-axes (axis-controller-id axis) (len &)))
          (idx (axis-id axis)))
      (claw:c-val ((axis-values :float))
        (when (< -1 idx len)
          (axis-values idx))))))


(defun controller-button-pressed-p (button)
  (check-host-thread)
  (claw:c-let ((len :int :from *foreign-int-place*))
    (let ((button-values (%glfw:get-joystick-buttons (axis-controller-id button) (len &)))
          (idx (axis-id button)))
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


(defun controller-hat-state (hat)
  (check-host-thread)
  (claw:c-let ((len :int :from *foreign-int-place*))
    (let ((hat-values (%glfw:get-joystick-hats (axis-controller-id hat) (len &)))
          (idx (axis-id hat)))
      (claw:c-val ((hat-values :char))
        (when (< -1 idx len)
          (glfw->hat-state (hat-values idx)))))))


(defgeneric on-controller-connect (listener controller)
  (:method (listener controller) (declare (ignore listener controller))))


(defgeneric on-controller-disconnect (listener controller)
  (:method (listener controller) (declare (ignore listener controller))))
