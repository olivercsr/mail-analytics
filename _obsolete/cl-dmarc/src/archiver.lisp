(in-package #:archiver)

;;(se:defgeneric unarchive (archiver in-stream out-handler))

(se:defun ensure-unarchived (in-stream out-handler)
  (arch-zip:unarchive in-stream out-handler))
