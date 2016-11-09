;;;; frc-hal.lisp

(in-package #:frc-hal)

(define-foreign-library libhalathena
  (t (:default "libHALAthena")))
(use-foreign-library libhalathena)

(defcfun "HALInitialize" :int (mode :int))
(defcfun "getHALErrorMessage" :string (code :int32))

(defcfun "getFPGAButton" :bool (status :pointer :int32))

;; 26 digital channels, 0-9 are DIO, 10-25 are MXP
(defcfun "getPort" :pointer (pin :uint8))
(defcfun "initializeDigitalPort" :pointer (port-pointer :pointer) (status :pointer :int32))
(defcfun "freeDigitalPort" :void (digital-port-pointer :pointer))
(defcfun "allocateDIO" :bool (digital-port-pointer :pointer) (input :bool) (status :pointer :int32))
(defcfun "freeDIO" :void (digital-port-pointer :pointer) (status :pointer :int32))
(defcfun "setDIO" :void (digital-port-pointer :pointer) (value :int16) (status :pointer :int32))
(defcfun "getDIO" :bool (digital-port-pointer :pointer) (status :pointer :int32))

(defvar *status-code-ptr* (foreign-alloc :int32))

(defclass dio-port ()
  ((%channel :reader dio-port-channel :initarg :channel)
   (%pointer :reader dio-port-pointer :initarg :pointer)
   (%direction :accessor dio-port-direction :initarg :direction)))

(defun make-dio-port (channel direction)
  "Takes a channel and direction, where input is either :OUTPUT or :INPUT."
  (unless (<= 0 channel 25)
    (error "Not a port number between 0 and 26."))
  (make-instance 'dio-port :channel channel
                           :pointer (let ((pointer (initializedigitalport (getport channel)
                                                                          *status-code-ptr*)))
                                      (allocatedio pointer
                                                   (ecase direction
                                                     (:output nil)
                                                     (:input t))
                                                   *status-code-ptr*)
                                      pointer)
                           :direction direction))

(defun read-dio-port (dio-port)
  (unless (eq dio-port-direction :input)
    (error "Not input direction dio-port."))
  (getdio (dio-port-pointer dio-port) *status-code-ptr*))

(defun set-dio-port (dio-port new-value)
  (unless (eq dio-port-direction :output)
    (error "not output direction dio-port."))
  (config-dio-port (dio-port-channel dio) nil)
  (setdio dio-port (if new-value 1 0) *status-code-ptr*))

(defun clear-status ()
  (setf (mem-ref *status-code-ptr* :int32) 0))

(defun get-status ()
  (mem-ref *status-code-ptr* :int32))

(defun print-status ()
  (let ((status (get-status)))
    (if (not (zerop status))
	(print (gethalerrormessage status)))))

(defmacro check-status (&body code)
  `(progn
     (clear-status)
     (let ((result (progn ,@code)))
       (print-status)
       result)))
