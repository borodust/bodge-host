(cl:in-package :bodge-host)


(declaim (special *window*))


(define-constant +expected-dpi+ 96)


(defvar *context* nil)
(defvar *context-lock* (bt:make-recursive-lock))


(defclass host-context (bodge-concurrency:lockable)
  ((task-queue :initform (make-task-queue))
   (enabled-p :initform t)
   (swap-interval :initform 0)
   (window-table :initform (make-hash-table))
   (shutdown-latch :initform (mt:make-latch))))


(defun bind-main-rendering-context (window)
  (%glfw:make-context-current (%handle-of window)))


(defun find-window-by-handle (win)
  (when-let ((context *context*))
    (with-slots (window-table) context
      (when win
        (gethash (cffi:pointer-address (claw:ptr win)) window-table)))))


(defun register-window (window)
  (when-let* ((context *context*)
              (win (%handle-of window)))
    (with-slots (window-table) context
      (bodge-concurrency:with-instance-lock-held (context)
        (let ((key (cffi:pointer-address (claw:ptr win))))
          (setf (gethash key window-table) window))))))


(defun remove-window (window)
  (when-let* ((context *context*)
              (win (%handle-of window)))
    (with-slots (window-table) context
      (bodge-concurrency:with-instance-lock-held (context)
        (remhash (cffi:pointer-address (claw:ptr win)) window-table)))))


(defun push-to-main-thread (fn)
  (when-let ((context *context*))
    (with-slots (task-queue) context
      (push-task fn task-queue)
      (%glfw:post-empty-event))))


(defmacro progm (&body body)
  `(push-to-main-thread (lambda () ,@body)))


(defun run-main-loop (context init-latch)
  (with-slots (enabled-p shutdown-latch task-queue) context
    (unwind-protect
         (claw:with-float-traps-masked ()
           (glfw:with-init ()
             (%glfw:set-error-callback (claw:callback 'on-glfw-error))
             (mt:open-latch init-latch)
             (loop while enabled-p
                   do (%glfw:wait-events)
                      (drain task-queue))))
      (mt:open-latch shutdown-latch))))


(defun create-context ()
  (let ((ctx (make-instance 'host-context)))
    (mt:wait-with-latch (latch)
      (with-body-in-main-thread ()
        (unwind-protect
             (run-main-loop ctx latch)
          (mt:open-latch latch))))
    ctx))


(defun open-window (window)
  (mt:wait-with-latch (latch)
    (labels ((open-latch ()
               (mt:open-latch latch))
             (%close-window ()
               (close-window window)))
      (in-new-thread ("bodge-window-start-thread")
        (bind-for-serious-condition (#'open-latch)
          (bt:with-recursive-lock-held (*context-lock*)
            (unless *context*
              (setf *context* (create-context)))
            (when (find-window-by-handle (%handle-of window))
              (error "Window already started"))
            (progm
              (bodge-util:bind-for-serious-condition (#'%close-window)
                (unwind-protect
                     (progn
                       (init-window window)
                       (register-window window))
                  (open-latch)))))))))
  window)


(defun sweep-context (context)
  (with-slots (app-table enabled-p shutdown-latch window-table) context
    (with-instance-lock-held (context)
      (when (= (hash-table-count window-table) 0)
        (progm
          (setf enabled-p nil))
        (mt:wait-for-latch shutdown-latch)
        (stop-main-runner)
        t))))


(defun close-window (window)
  (mt:wait-with-latch (latch)
    (in-new-thread ("bodge-window-stop-thread")
      (unwind-protect
           (bt:with-recursive-lock-held (*context-lock*)
             (unless (find-window-by-handle (%handle-of window))
               (warn "Window is not running"))
             (remove-window window)
             (progm
               (destroy-window window))
             (if (sweep-context *context*)
                 (setf *context* nil)))
        (mt:open-latch latch))))
  window)


(defun swap-interval ()
  (when-let ((context *context*))
    (with-slots (swap-interval) context
      swap-interval)))


(defun (setf swap-interval) (value)
  (when-let ((context *context*))
    (with-slots (swap-interval) context
      (%glfw:swap-interval (setf swap-interval (floor value)))
      swap-interval)))
