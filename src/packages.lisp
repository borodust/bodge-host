(bodge-util:define-package :bodge-host
    (:use :cl :bodge-concurrency :bodge-util :bodge-math)
  (:export window
           open-window
           close-window

           progm
           push-to-main-thread

           on-init
           on-destroy
           on-hide
           on-key-action
           on-mouse-action
           on-cursor-movement
           on-scroll
           on-framebuffer-size-change
           on-viewport-size-change
           on-character-input
           on-log

           bind-main-rendering-context
           bind-shared-rendering-context
           release-rendering-context
           make-shared-rendering-context
           destroy-shared-rendering-context

           swap-buffers
           swap-interval
           viewport-title
           viewport-size
           framebuffer-size
           fullscreen-viewport-p
           lock-cursor
           unlock-cursor
           viewport-scale

           cursor-position
           keyboard-button-state
           mouse-button-state))
