(cl:in-package :bodge-host.native)


(defun print-x-image-info (image)
  (claw:c-val ((image %host.native:x-image))
    (format t "~%Depth:~20T ~A
Bits per pixel:~20T ~A
Bytes per line:~20T ~A
Bitmap unit:~20T ~A
Bitmap padding:~20T ~A
X Offset:~20T ~A
"
            (image :depth)
            (image :bits-per-pixel)
            (image :bytes-per-line)
            (image :bitmap-unit)
            (image :bitmap-pad)
            (image :xoffset))))


(defun read-screen-region (x y width height data-ptr)
  (let* ((x (floor x))
         (y (floor y))
         (width (floor width))
         (height (floor height))
         (display (%glfw:get-x11display))
         (default-screen (%host.native:x-default-screen display))
         (root-window (%host.native:x-root-window display default-screen)))
    (claw:c-with ((win-attribs %host.native:x-window-attributes))
      (%host.native:x-get-window-attributes display root-window win-attribs)
      (let ((image (%host.native:x-get-image display root-window
                                             x (- (win-attribs :height) (+ y height)) width height
                                             %host.native:+all-planes+
                                             %host.native:+z-pixmap+)))
        (claw:c-val ((image %host.native:x-image))
          (unless (= (image :bits-per-pixel) 32)
            (error "Unexpected pixmap bits per pixel: ~A" (image :bits-per-pixel)))
          (claw:memcpy data-ptr (image :data) (* width height 4)))
        (%host.native:x-free image)))))
