(asdf:defsystem :bodge-host
  :description "OS-dependent routines"
  :version "1.0.0"
  :license "MIT"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :depends-on (:bodge-utilities :bodge-host/native
               :cffi-c-ref :claw :bodge-libc-essentials :bodge-concurrency
               :bodge-math :bodge-glfw :glfw-blob :float-features)
  :pathname "src/"
  :serial t
  :components ((:file "packages")
               (:file "utils")
               (:file "cursor")
               (:file "monitor")
               (:file "window")
               (:file "controller")
               (:file "host")))
