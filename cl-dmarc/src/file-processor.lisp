(in-package :file-processor)

(defgeneric lock-next-file (file-processor))
(defgeneric file-done (file-processor file))
(defgeneric archive-done-files (file-processor))
(defgeneric reset-stuck-files (file-processor))
