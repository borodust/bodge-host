(cl:in-package :bodge-host.native)


(defun read-screen-region (x y width height data-ptr)
  (declare (ignore data-ptr))
  (let* ((x (floor x))
         (y (floor y))
         (width (floor width))
         (height (floor height))
         (display (%host.native:cg-main-display-id)))
    (declare (ignore x y width height display))
    (error "Unimplemented")))
