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

(defparameter nullptr (make-pointer 0))
(defvar *status-code-ptr* (foreign-alloc :pointer)))
(defvar *dio-ports* (make-array 26 :element-type 'sb-sys:system-area-pointer :initial-element nullptr))

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

(defun get-dio-port (channel)
  (if (or (< channel 0) (>= channel 26)) nullptr)
  (when (null-pointer-p (aref *dio-ports* channel))
      (setf (aref *dio-ports* channel)
	    (check-status
	      (initializedigitalport (getport channel) *status-code-ptr*)))
      (check-status 
	(allocatedio (aref *dio-ports* channel) t *status-code-ptr*)))
  (aref *dio-ports* channel))

(defun config-dio-port (channel input)
  (let ((port (get-dio-port channel)))
    (if (null-pointer-p port)
	nil
	(check-status
	  (freedio port *status-code-ptr*)
	  (allocatedio port input *status-code-ptr*)))))

(defun set-dio-output (channel value)
  (let ((port (get-dio-port channel)))
    (if (null-pointer-p port) (return-from set-dio-output))
    (config-dio-port channel nil)
    (check-status
      (setdio port (if value 1 0) *status-code-ptr*))))
