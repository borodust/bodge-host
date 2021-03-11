(bodge-util:define-package :bodge-host
  (:use :cl :bodge-concurrency :bodge-util :bodge-math :cffi-c-ref)
  (:reexport-from :bodge-math
                  #:x
                  #:y)
  (:export #:*event-wait-timeout*
           #:window
           #:open-window
           #:close-window
           #:show-window
           #:hide-window

           #:progm
           #:push-to-main-thread

           #:on-init
           #:on-destroy
           #:on-hide
           #:modifiers-engaged-p
           #:on-key-action
           #:on-mouse-action
           #:on-cursor-movement
           #:on-scroll
           #:on-framebuffer-size-change
           #:on-viewport-size-change
           #:on-character-input
           #:on-gamepad-action
           #:on-dpad-action
           #:on-left-stick-movement
           #:on-right-stick-movement
           #:on-left-trigger
           #:on-right-trigger
           #:on-log

           #:on-controller-connect
           #:on-controller-disconnect
           #:list-controllers
           #:controller-name
           #:controller-guid
           #:controller-axes
           #:controller-axis-value
           #:controller-buttons
           #:controller-button-pressed-p
           #:controller-hats
           #:controller-hat-state

           #:update-gamepad-mappings
           #:list-gamepads
           #:on-gamepad-connect
           #:on-gamepad-disconnect
           #:gamepad-name
           #:gamepad-guid
           #:gamepad-state
           #:gamepad-state-button-pressed-p
           #:gamepad-state-dpad
           #:gamepad-state-left-stick
           #:gamepad-state-right-stick
           #:gamepad-state-left-trigger
           #:gamepad-state-right-trigger

           #:bind-main-rendering-context
           #:bind-shared-rendering-context
           #:release-rendering-context
           #:make-shared-rendering-context
           #:destroy-shared-rendering-context

           #:swap-buffers
           #:swap-interval
           #:list-controllers
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
           #:video-mode-blue-bits))
