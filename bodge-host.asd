(asdf:defsystem :bodge-host
  :description "OS-dependent routines"
  :version "1.0.0"
  :license "MIT"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :depends-on (bodge-utilities claw bodge-concurrency bodge-math bodge-glfw glfw-blob)
  :pathname "src/"
  :serial t
  :components ((:file "packages")
               (:module native-module
                :pathname "native"
                :serial t
                :components ((:file "packages")
                             (:static-file "bodge_host.h")
                             (:file "claw")
                             (:file "native-unix"
                              :if-feature (:and :unix (:not :darwin)))
                             (:file "native-unknown"
                              :if-feature (:not (:or :unix :darwin :windows)))
                             (:module spec)))
               (:file "utils")
               (:file "callbacks")
               (:file "cursor")
               (:file "window")
               (:file "monitor")
               (:file "native")
               (:file "host")))
