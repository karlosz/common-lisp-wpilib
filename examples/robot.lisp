;;;; Basic robot code

(defpackage basic-robot
  (:use #:cl #:wpilib/iterative-robot))

(in-package #:basic-robot)

(defclass my-robot (wpilib:iterative-robot)
  ((robot-drive :reader robot-drive :initarg :robot-drive)
   (stick       :reader stick       :initarg :stick)))

(defmethod autonomous-periodic ((robot my-robot) (joystick xbox-joystick))
  (drive (robot-drive robot) 0 0))

(defmethod autonomous-periodic ((robot my-robot) (joystick code-joystick))
  (drive (robot-drive robot) 0 0))

(defmethod teleop-periodic ((robot my-robot))

(defun robot-main ()
  (wpilib:run (make-instance 'my-robot
                             :robot-drive (wpilib:make-robot-drive 0 1)
                             :stick (wpilib:make-joystick 1))))
