(cl:in-package :bodge-host)


(declaim (special *window*))

(defclass host-context (bodge-concurrency:lockable)
  ((task-queue :initform (make-task-queue))
   (enabled-p :initform nil)
   (swap-interval :initform 0)
   (window-table :initform (make-hash-table))
   (controller-hub :initform nil)
   (controller-listeners :initform nil)))

(defvar *context* (make-instance 'host-context))


(definline context-enabled-p ()
  (with-slots (enabled-p) *context*
    enabled-p))


(defmacro with-context-locked (&body body)
  `(bodge-concurrency:with-instance-lock-held (*context*)
     ,@body))


(defun invoke-controller-listeners (fu joystick-id)
  (with-slots (controller-listeners controller-hub) *context*
    (loop with controller = (find-controller controller-hub joystick-id)
          for listener in controller-listeners
          do (log-errors (funcall fu listener controller)))))


(glfw:define-joystick-callback on-joystick-event (joystick-id event-id)
  (progm
    (cond
      ((= event-id %glfw:+connected+)
       (invoke-controller-listeners #'on-controller-connect joystick-id))
      ((= event-id %glfw:+disconnected+)
       (invoke-controller-listeners #'on-controller-disconnect joystick-id)))))


(defun init-context (init-task)
  (with-slots (enabled-p controller-hub) *context*
    (flet ((%init-task ()
             (setf controller-hub (make-controller-hub))
             (funcall init-task)))
      (%glfw:init-hint %glfw:+joystick-hat-buttons+ %glfw:+false+)
      (setf enabled-p t
            *foreign-int-place* (claw:calloc :int))
      (%glfw:set-error-callback (claw:callback 'on-glfw-error))
      (with-body-in-main-thread ()
        (let ((*host-thread-p* t))
          (init-main-loop #'%init-task))))))


(defun release-context ()
  (with-slots (enabled-p window-table controller-hub) *context*
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


(defun run-main-loop ()
  (with-slots (task-queue) *context*
    (tagbody begin
       (restart-case
           (loop while (context-enabled-p)
                 do (%glfw:wait-events)
                    (drain task-queue))
         (ignore ()
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


(defun push-to-main-thread (fn)
  (with-slots (task-queue) *context*
    (push-task fn task-queue)
    (%glfw:post-empty-event)))


(defmacro progm (&body body)
  `(push-to-main-thread (lambda () ,@body)))


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
    (%glfw:swap-interval (setf swap-interval (floor value)))
    swap-interval))


(defun register-controller-listener (listener)
  (with-slots (controller-listeners controller-hub) *context*
    (progm
      (pushnew listener controller-listeners)
      (flet ((%invoke-connected (controller)
               (on-controller-connect listener controller)))
        (for-each-controller controller-hub #'%invoke-connected)))
    (values)))


(defun remove-controller-hub (listener)
  (with-slots (controller-hub controller-listeners) *context*
    (progm
      (flet ((%invoke-disconnected (controller)
               (on-controller-disconnect listener controller)))
        (for-each-controller controller-hub #'%invoke-disconnected))
      (deletef controller-listeners listener))
    (values)))
