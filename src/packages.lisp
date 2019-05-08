(bodge-util:define-package :bodge-host
  (:use :cl :bodge-concurrency :bodge-util :bodge-math)
  (:reexport-from :bodge-math
                  #:x
                  #:y)
  (:export #:window
           #:open-window
           #:close-window
           #:show-window
           #:hide-window

           #:progm
           #:push-to-main-thread

           #:on-init
           #:on-destroy
           #:on-hide
           #:on-key-action
           #:on-mouse-action
           #:on-cursor-movement
           #:on-scroll
           #:on-framebuffer-size-change
           #:on-viewport-size-change
           #:on-character-input
           #:on-log

           #:register-controller-listener
           #:remove-controller-listener
           #:on-controller-connect
           #:on-controller-disconnect
           #:controller-name
           #:controller-axes
           #:controller-axis-value
           #:controller-buttons
           #:controller-button-pressed-p
           #:controller-hats
           #:controller-hat-state

           #:bind-main-rendering-context
           #:bind-shared-rendering-context
           #:release-rendering-context
           #:make-shared-rendering-context
           #:destroy-shared-rendering-context

           #:swap-buffers
           #:swap-interval
           #:viewport-title
           #:viewport-size
           #:with-viewport-dimensions
           #:viewport-position
           #:viewport-autoscaled-p
           #:framebuffer-size
           #:with-framebuffer-dimensions
           #:fullscreen-viewport-p
           #:lock-cursor
           #:unlock-cursor
           #:viewport-scale

           #:cursor-position
           #:keyboard-button-state
           #:mouse-button-state

           #:cursor
           #:make-standard-cursor
           #:destroy-cursor

           #:window-monitor
           #:available-monitors
           #:primary-monitor
           #:monitor-name
           #:monitor-position
           #:monitor-video-mode
           #:video-mode-width
           #:video-mode-height
           #:video-mode-refresh-rate
           #:video-mode-red-bits
           #:video-mode-green-bits
           #:video-mode-blue-bits

           #:read-screen-region))
