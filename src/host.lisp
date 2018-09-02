(cl:in-package :bodge-host)


(declaim (special *application*))


(define-constant +expected-dpi+ 96)


(defvar *context* nil)
(defvar *context-lock* (bt:make-recursive-lock))


(defclass host-context (bodge-concurrency:lockable)
  ((task-queue :initform (make-task-queue))
   (enabled-p :initform t)
   (swap-interval :initform 0)
   (application-table :initform (make-hash-table))
   (shutdown-latch :initform (mt:make-latch))))


(defun bind-main-rendering-context (application)
  (%glfw:make-context-current (%window-of application)))


(defun find-application-by-window (win)
  (when-let ((context *context*))
    (with-slots (application-table) context
      (when win
        (gethash (cffi:pointer-address (claw:ptr win)) application-table)))))


(defun register-application (application)
  (when-let* ((context *context*)
              (win (%window-of application)))
    (with-slots (application-table) context
      (bodge-concurrency:with-instance-lock-held (context)
        (let ((key (cffi:pointer-address (claw:ptr win))))
          (setf (gethash key application-table) application))))))


(defun remove-application (application)
  (when-let* ((context *context*)
              (win (%window-of application)))
    (with-slots (application-table) context
      (bodge-concurrency:with-instance-lock-held (context)
        (remhash (cffi:pointer-address (claw:ptr win)) application-table)))))


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
             (%glfw:swap-interval 0)
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


(defun start-application (application)
  (mt:wait-with-latch (latch)
    (labels ((open-latch ()
               (mt:open-latch latch))
             (%stop-application ()
               (stop-application application)))
      (in-new-thread ("bodge-application-start-thread")
        (bind-for-serious-condition (#'open-latch)
          (bt:with-recursive-lock-held (*context-lock*)
            (unless *context*
              (setf *context* (create-context)))
            (when (find-application-by-window (%window-of application))
              (error "Application already started"))
            (progm
              (bodge-util:bind-for-serious-condition (#'%stop-application)
                (unwind-protect
                     (progn
                       (init-application application)
                       (register-application application))
                  (open-latch)))))))))
  application)


(defun sweep-context (context)
  (with-slots (app-table enabled-p shutdown-latch application-table) context
    (with-instance-lock-held (context)
      (when (= (hash-table-count application-table) 0)
        (progm
          (setf enabled-p nil))
        (mt:wait-for-latch shutdown-latch)
        (stop-main-runner)
        t))))


(defun stop-application (application)
  (mt:wait-with-latch (latch)
    (in-new-thread ("bodge-application-stop-thread")
      (unwind-protect
           (bt:with-recursive-lock-held (*context-lock*)
             (unless (find-application-by-window (%window-of application))
               (warn "Application is not running"))
             (remove-application application)
             (progm
               (destroy-application application))
             (if (sweep-context *context*)
                 (setf *context* nil)))
        (mt:open-latch latch))))
  application)


(defun swap-interval ()
  (when-let ((context *context*))
    (with-slots (swap-interval) context
      swap-interval)))


(defun (setf swap-interval) (value)
  (when-let ((context *context*))
    (with-slots (swap-interval) context
      (%glfw:swap-interval (setf swap-interval (floor value)))
      swap-interval)))
