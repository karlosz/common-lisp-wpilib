;;;; frc-hal.lisp

(in-package #:frc-hal)

(define-foreign-library libhalathena
  (t (:default "libHALAthena")))
(use-foreign-library libhalathena)

(define-foreign-library libc
  (:unix (:or "libc.so.6")))
(use-foreign-library libc)

(defcfun "usleep" :int (usec :uint))

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

(defcenum counter-mode
  (:two-pulse 0)
  (:semi-period 1)
  (:pulse-length 2)
  (:external-direction 3))

(defcfun "initializeCounter" :pointer (mode counter-mode) (index :pointer :uint32) (status :pointer :int32))
(defcfun "freeCounter" :void (counter-pointer :pointer) (status :pointer :int32))
(defcfun "setCounterUpSource" :void (counter-pointer :pointer) (pin :uint32) (analog-trigger :bool) (status :pointer :int32))
(defcfun "setCounterSemiPeriodMode" :void (counter-pointer :pointer) (high-semi-period :bool) (status :pointer :int32))
(defcfun "getCounterPeriod" :double (counter-pointer :pointer) (status :pointer :int32))
(defcfun "setCounterMaxPeriod" :void (counter-pointer :pointer) (max-period :double) (status :pointer :int32))

(defvar *status-code-ptr* (foreign-alloc :int32))

(defclass pulse-measurer ()
  ((%pointer :reader pulse-measurer-pointer :initarg :pointer)
   (%index :reader pulse-measurer-index :initarg :index)))

(defun make-pulse-measurer (channel polarity max-period)
  "Takes a DIO channel, signal polarity, and max period measurement in seconds. The polarity is either :HIGH or :LOW."
  (unless (<= 0 channel 25)
    (error "Not a port number between 0 and 25."))
  (with-foreign-pointer (index 4)
    (make-instance 'pulse-measurer :pointer (let ((pointer (initializecounter :semi-period index *status-code-ptr*)))
					      (setcountersemiperiodmode pointer
									(ecase polarity
									  (:high t)
									  (:low nil))
									  *status-code-ptr*)
					      (setcounterupsource pointer channel nil *status-code-ptr*)
					      (setcountermaxperiod pointer max-period *status-code-ptr*)
					      pointer)
		                   :index (mem-ref index :uint32))))

(defun pulse-measurer-period (pulse-measurer)
  (getcounterperiod (pulse-measurer-pointer pulse-measurer) *status-code-ptr*))

(defclass dio-port ()
  ((%channel :reader dio-port-channel :initarg :channel)
   (%pointer :reader dio-port-pointer :initarg :pointer)
   (%direction :reader dio-port-direction :initarg :direction)))

(defun make-dio-port (channel direction)
  "Takes a channel and direction, where input is either :OUTPUT or :INPUT."
  (unless (<= 0 channel 25)
    (error "Not a port number between 0 and 25."))
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
  (getdio (dio-port-pointer dio-port) *status-code-ptr*))

(defun write-dio-port (dio-port new-value)
  (setdio (dio-port-pointer dio-port) (if new-value 1 0) *status-code-ptr*))

(defmethod (setf dio-port-direction) (new-value (dio-port dio-port))
  (setf (slot-value dio-port '%direction) new-value)
  (config-dio-port dio-port new-value))

(defun config-dio-port (dio-port direction)
  (freedio (dio-port-pointer dio-port) *status-code-ptr*)
  (allocatedio (dio-port-pointer dio-port)
	       (ecase direction
		 (:output nil)
		 (:input t))
	       *status-code-ptr*)
  (setf (dio-port-direction dio-port) direction))

; the speed of sound in dry air at 20C is 343.2 m/s
(defconstant +sound-s-per-cm+ (/ 1 343.2d0 100))
(defun read-ping-sensor (dio-port pulse-measurer)
  "Returns the distance read by a PING))) sensor in inches."
  (write-dio-port dio-port t)
  (setf (dio-port-direction dio-port) :output)
  (usleep 5)
  (setf (dio-port-direction dio-port) :input)
  (loop until (read-dio-port dio-port))
  (loop while (read-dio-port dio-port))
  (let ((period (pulse-measurer-period pulse-measurer)))
    (usleep 200)
    (/ period 2 +sound-s-per-cm+)))

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

