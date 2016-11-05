;;;; frc-hal.asd

(asdf:defsystem #:frc-hal
  :description "Hal bindings"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :depends-on (#:cffi)
  :components ((:file "package")
               (:file "frc-hal")))
