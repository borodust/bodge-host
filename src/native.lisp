(cl:in-package :bodge-host)


(defun read-screen-region (x y width height data-ptr)
  (bodge-host.native:read-screen-region x y width height data-ptr))
