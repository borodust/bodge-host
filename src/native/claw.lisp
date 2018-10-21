(claw:c-include (native-module "bodge_host.h") bodge-host
  :in-package :%host.native
  :spec-module (native-module spec)
  :include-definitions (;; X11
                        "XDefaultScreen"
                        "XRootWindow"
                        "XGetImage"
                        "XImage"
                        "XFree"
                        "AllPlanes"
                        "ZPixmap"
                        "XGetWindowAttributes"
                        "XWindowAttributes"
                        ;; CG
                        "CGMainDisplayID"
                        "CGDisplayCreateImageForRect"
                        "CGImageRelease"
                        "CFDataGetBytePtr"
                        "CFDataGetLength"
                        "CFDataGetLength"
                        "CFRelease"))
