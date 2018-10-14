(cl:in-package :bodge-host)


(defun available-monitors ()
  (claw:c-with ((monitor-count :int)
                (monitor-array :pointer :from (%glfw:get-monitors (monitor-count &))))
    (loop for idx from 0 below monitor-count
          collect (monitor-array idx))))


(defun primary-monitor ()
  (%glfw:get-primary-monitor))


(defun monitor-name (monitor)
  (%glfw:get-monitor-name monitor))


(defun monitor-video-mode (monitor)
  (%glfw:get-video-mode monitor))


(defun monitor-position (monitor &optional (result-vec (vec2)))
  (claw:c-with ((width :int)
                (height :int))
    (%glfw:get-monitor-pos monitor (width &) (height &))
    (setf (x result-vec) width
          (y result-vec) height))
  result-vec)


(defun video-mode-width (video-mode)
  (claw:c-val ((video-mode %glfw:vidmode))
    (video-mode :width)))


(defun video-mode-height (video-mode)
  (claw:c-val ((video-mode %glfw:vidmode))
    (video-mode :height)))


(defun video-mode-refresh-rate (video-mode)
  (claw:c-val ((video-mode %glfw:vidmode))
    (video-mode :refresh-rate)))


(defun video-mode-red-bits (video-mode)
  (claw:c-val ((video-mode %glfw:vidmode))
    (video-mode :red-bits)))


(defun video-mode-green-bits (video-mode)
  (claw:c-val ((video-mode %glfw:vidmode))
    (video-mode :green-bits)))


(defun video-mode-blue-bits (video-mode)
  (claw:c-val ((video-mode %glfw:vidmode))
    (video-mode :blue-bits)))
