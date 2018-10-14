(asdf:defsystem :bodge-host
  :description "OS-dependent routines"
  :version "1.0.0"
  :license "MIT"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :depends-on (bodge-utilities bodge-concurrency bodge-math bodge-glfw glfw-blob)
  :pathname "src/"
  :serial t
  :components ((:file "packages")
               (:file "utils")
               (:file "callbacks")
               (:file "cursor")
               (:file "window")
               (:file "monitor")
               (:file "host")))
