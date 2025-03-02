;;(declaim (optimize (speed 0) (space 0) (compilation-speed 0) (debug 3)))

(in-package :event-listener)

(se:defgeneric connect (event-listener))
(se:defgeneric disconnect (event-listener))
