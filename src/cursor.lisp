(cl:in-package :bodge-host)


(defenum cursor-type
  :arrow
  :ibeam
  :crosshair
  :hand
  :hresize
  :vresize)


(definline cursor-type->glfw (cursor-type)
  (ecase cursor-type
    (:arrow %glfw:+arrow-cursor+)
    (:ibeam %glfw:+ibeam-cursor+)
    (:crosshair %glfw:+crosshair-cursor+)
    (:hand %glfw:+hand-cursor+)
    (:center %glfw:+center-cursor+)
    (:hresize %glfw:+hresize-cursor+)
    (:vresize %glfw:+vresize-cursor+)))


(defun make-standard-cursor (cursor-type)
  "Must be run from main thread (see progm macro)"
  (%glfw:create-standard-cursor (cursor-type->glfw cursor-type)))


(defun destroy-cursor (cursor)
  "Must be run from main thread (see progm macro)"
  (%glfw:destroy-cursor cursor))
