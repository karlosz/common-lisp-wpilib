;;;; frc-hal.lisp

(in-package #:frc-hal)

(define-foreign-library libhalathena
  (t (:default "libHALAthena")))

(use-foreign-library libhalathena)

(defcfun (hal-initialize "HALInitialize") :int
  (mode :int))
