(cl:in-package :bodge-host)


(declaim (special *window*))


(defvar *event-wait-timeout* (float 1/120 0d0))
(defvar *gamepad-buttons* '(:a :b :x :y
                            :left-bumper :right-bumper
                            :start :back :guide
                            :left-thumb :right-thumb))


(defclass host-context (bodge-concurrency:lockable)
  ((task-queue :initform (make-task-queue))
   (enabled-p :initform nil)
   (swap-interval :initform 0)
   (window-table :initform (make-hash-table))
   (controller-hub :initform nil)))

(defvar *context* (make-instance 'host-context))


(definline context-enabled-p ()
  (with-slots (enabled-p) *context*
    enabled-p))


(defmacro with-context-locked (&body body)
  `(bodge-concurrency:with-instance-lock-held (*context*)
     ,@body))


(defun push-to-main-thread (fn)
  (with-slots (task-queue) *context*
    (push-task fn task-queue)
    (%glfw:post-empty-event)))


(defmacro progm (&body body)
  `(push-to-main-thread (lambda () ,@body)))


(defun for-each-window (fu)
  (with-slots (window-table) *context*
    (loop for window being the hash-value of window-table
          do (funcall fu window))))


(defmacro do-windows ((win) &body body)
  `(for-each-window (lambda (,win) ,@body)))


(defun invoke-controller-listeners (controller-fu gamepad-fu joystick-id)
  (with-slots (window-table controller-hub) *context*
    (loop with controller = (find-controller controller-hub joystick-id)
          with gamepad = (find-gamepad controller-hub joystick-id)
          for window being the hash-value of window-table
          do (log-errors
               (when controller
                 (funcall controller-fu window controller))
               (when gamepad
                 (funcall gamepad-fu window gamepad))))))


(glfw:define-joystick-callback on-joystick-event (joystick-id event-id)
  (with-slots (controller-hub) *context*
    (progm
      (cond
        ((= event-id %glfw:+connected+)
         (register-controller controller-hub joystick-id)
         (invoke-controller-listeners #'on-controller-connect
                                      #'on-gamepad-connect
                                      joystick-id))
        ((= event-id %glfw:+disconnected+)
         (invoke-controller-listeners #'on-controller-disconnect
                                      #'on-gamepad-disconnect
                                      joystick-id)
         (remove-controller controller-hub joystick-id))))))


(defun update-gamepad-mappings (mappings)
  (with-slots (controller-hub window-table) *context*
    (flet ((for-each-window (gamepad-action controller-action)
             (loop for win being the hash-value of window-table
                   do (loop for gamepad in (controller-hub-gamepads controller-hub)
                            do (funcall gamepad-action win gamepad))
                      (loop for controller in (controller-hub-controllers controller-hub)
                            do (funcall controller-action win controller)))))
      (%glfw:set-joystick-callback nil)
      (for-each-window #'on-gamepad-disconnect #'on-controller-disconnect)
      (destroy-controller-hub controller-hub)
      (when (= (%glfw:update-gamepad-mappings mappings) %glfw:+false+)
        (warn "Failed to read gamepad mappings"))
      (%glfw:set-joystick-callback (claw:callback 'on-joystick-event))
      (setf controller-hub (make-controller-hub))
      (for-each-window #'on-gamepad-connect #'on-controller-connect))))


(defun %update-gamepad-mappings ()
  (when-let ((controller-db-file (bodge-util:getenv "BODGE_GAMECONTROLLERDB")))
    (if-let ((controller-db-truename (probe-file controller-db-file)))
      (update-gamepad-mappings (bodge-util:read-file-into-string controller-db-truename))
      (warn "Gamepad mappings file ~A not found" controller-db-file))))


(defun init-context (init-task)
  (with-slots (enabled-p controller-hub) *context*
    (flet ((%init-task ()
             (setf controller-hub (make-controller-hub))
             (%glfw:set-joystick-callback (claw:callback 'on-joystick-event))
             (%update-gamepad-mappings)
             (funcall init-task)))
      ;; don't expose hats as buttons
      (%glfw:init-hint %glfw:+joystick-hat-buttons+ %glfw:+false+)
      (setf enabled-p t
            *foreign-int-place* (claw:calloc :int))
      (%glfw:set-error-callback (claw:callback 'on-glfw-error))
      (with-body-in-main-thread ()
        (let ((*host-thread-p* t))
          (init-main-loop #'%init-task))))))


(defun release-context ()
  (with-slots (enabled-p window-table controller-hub) *context*
    (%glfw:set-joystick-callback nil)
    (destroy-controller-hub controller-hub)
    (claw:free *foreign-int-place*)
    (setf enabled-p nil
          *foreign-int-place* nil)
    (unwind-protect
         (loop for window being the hash-value in window-table
               do (handler-case
                      (destroy-window window)
                    (serious-condition (e)
                      (warn "Window destructuring failed. Skipping: ~A" e))))
      (clrhash window-table))
    (stop-main-runner)))


(defun update-gamepads ()
  (with-slots (controller-hub) *context*
    (flet ((%updated-gamepad (gamepad old-state new-state)
             (loop for button in *gamepad-buttons*
                   unless (eq (gamepad-state-button-pressed-p old-state button)
                              (gamepad-state-button-pressed-p new-state button))
                     do (let ((state (if (gamepad-state-button-pressed-p new-state button)
                                         :pressed
                                         :released)))
                          (do-windows (win)
                            (on-gamepad-action win gamepad button state))))
             (let ((new-dpad-state (gamepad-state-dpad new-state)))
               (unless (eq (gamepad-state-dpad old-state) new-dpad-state)
                 (do-windows (win)
                   (on-dpad-action win gamepad new-dpad-state))))
             (let ((old-vec (vec2))
                   (new-vec (vec2)))
               (unless (vec= (gamepad-state-left-stick old-state old-vec)
                             (gamepad-state-left-stick new-state new-vec))
                 (do-windows (win)
                   (on-left-stick-movement win gamepad (x new-vec) (y new-vec))))
               (unless (vec= (gamepad-state-right-stick old-state old-vec)
                             (gamepad-state-right-stick new-state new-vec))
                 (do-windows (win)
                   (on-right-stick-movement win gamepad (x new-vec) (y new-vec)))))
             (unless (= (gamepad-state-left-trigger old-state)
                        (gamepad-state-left-trigger new-state))
               (do-windows (win)
                 (on-left-trigger win gamepad (gamepad-state-left-trigger new-state))))
             (unless (= (gamepad-state-right-trigger old-state)
                        (gamepad-state-right-trigger new-state))
               (do-windows (win)
                 (on-right-trigger win
                                   gamepad
                                   (gamepad-state-right-trigger new-state))))))
      (for-each-updated-gamepad controller-hub #'%updated-gamepad))))


(defun run-main-loop ()
  (with-slots (task-queue) *context*
    (tagbody begin
       (restart-case
           (loop while (context-enabled-p)
                 do (%glfw:wait-events-timeout (float *event-wait-timeout* 0d0))
                    (update-gamepads)
                    (drain task-queue))
         (continue ()
           :report "Continue looping in main thread"
           (go begin))))))


(defun init-main-loop (init-task)
  (claw:with-float-traps-masked ()
    (glfw:with-init ()
      (unwind-protect
           (progn
             (funcall init-task)
             (run-main-loop))
        (with-context-locked
          (release-context))))))


(defun ensure-context (init-task)
  (if (context-enabled-p)
      (push-to-main-thread init-task)
      (init-context init-task)))


(defun sweep-context ()
  (with-slots (enabled-p window-table) *context*
    (when enabled-p
      (when (= (hash-table-count window-table) 0)
        (setf enabled-p nil)))))


(defun bind-main-rendering-context (window)
  (%glfw:make-context-current (%handle-of window)))


(defun find-window-by-handle (win-handle)
  (with-slots (window-table) *context*
    (unless (claw:null-pointer-p win-handle)
      (gethash (cffi:pointer-address (claw:ptr win-handle)) window-table))))


(defun register-window (window)
  (when-let ((win (%handle-of window)))
    (with-slots (window-table) *context*
      (let ((key (cffi:pointer-address (claw:ptr win))))
        (setf (gethash key window-table) window)))))


(defun remove-window (window)
  (when-let ((win (%handle-of window)))
    (with-slots (window-table) *context*
      (remhash (cffi:pointer-address (claw:ptr win)) window-table))))



(defun open-window (window)
  (with-context-locked
    (labels ((%destroy-window ()
               (destroy-window window))
             (%task ()
               (with-context-locked
                 (unless (find-window-by-handle (%handle-of window))
                   (bodge-util:bind-for-serious-condition (#'%destroy-window)
                     (init-window window))
                   (register-window window)))))
      (ensure-context #'%task)))
  window)


(defun close-window (window)
  (with-context-locked
    (unless (find-window-by-handle (%handle-of window))
      (warn "Window is already closed"))
    (progm
      (remove-window window)
      (unwind-protect
           (destroy-window window)
        (with-context-locked
          (sweep-context)))))
  window)


(defun swap-interval ()
  (with-slots (swap-interval) *context*
    swap-interval))


(defun (setf swap-interval) (value)
  (with-slots (swap-interval) *context*
    (%glfw:swap-interval (setf swap-interval (round value)))
    swap-interval))


(defun list-controllers ()
  (with-slots (controller-hub) *context*
    (controller-hub-controllers controller-hub)))


(defun list-gamepads ()
  (with-slots (controller-hub) *context*
    (controller-hub-gamepads controller-hub)))
