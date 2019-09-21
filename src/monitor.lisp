(cl:in-package :bodge-host)


(defun available-monitors ()
  (c-with ((monitor-count :int))
    (c-let ((monitor-array :pointer :from (%glfw:get-monitors (monitor-count &))))
      (loop for idx from 0 below monitor-count
            collect (monitor-array idx)))))


(defun primary-monitor ()
  (%glfw:get-primary-monitor))


(defun monitor-name (monitor)
  (%glfw:get-monitor-name monitor))


(defun monitor-video-mode (monitor)
  (%glfw:get-video-mode monitor))


(defun monitor-position (monitor &optional (result-vec (vec2)))
  (c-with ((width :int)
           (height :int))
    (%glfw:get-monitor-pos monitor (width &) (height &))
    (setf (x result-vec) width
          (y result-vec) height))
  result-vec)


(defun monitor-content-scale (monitor)
  (c-with ((x :float))
    (%glfw:get-monitor-content-scale monitor (x &) nil)
    x))


(defun primary-monitor-content-scale ()
  (monitor-content-scale (%glfw:get-primary-monitor)))


(defun video-mode-width (video-mode)
  (c-val ((video-mode %glfw:vidmode))
    (video-mode :width)))


(defun video-mode-height (video-mode)
  (c-val ((video-mode %glfw:vidmode))
    (video-mode :height)))


(defun video-mode-refresh-rate (video-mode)
  (c-val ((video-mode %glfw:vidmode))
    (video-mode :refresh-rate)))


(defun video-mode-red-bits (video-mode)
  (c-val ((video-mode %glfw:vidmode))
    (video-mode :red-bits)))


(defun video-mode-green-bits (video-mode)
  (c-val ((video-mode %glfw:vidmode))
    (video-mode :green-bits)))


(defun video-mode-blue-bits (video-mode)
  (c-val ((video-mode %glfw:vidmode))
    (video-mode :blue-bits)))
