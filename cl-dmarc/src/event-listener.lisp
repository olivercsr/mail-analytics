;;(declaim (optimize (speed 0) (space 0) (compilation-speed 0) (debug 3)))

(in-package :event-listener)

(defgeneric connect (event-listener))
(defgeneric disconnect (event-listener))
