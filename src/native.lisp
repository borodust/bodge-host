(cl:in-package :bodge-host)


(defun read-screen-region (x y width height image-array)
  (bodge-util:with-simple-array-pointer (ptr image-array)
    (bodge-host.native:read-screen-region x y width height ptr)))
