;;(declaim (optimize (speed 0) (space 0) (compilation-speed 0) (debug 3)))

(in-package :pubsub)

(se:defgeneric consume (pubsub &rest args))
(se:defgeneric produce (pubsub topic message &rest args))
