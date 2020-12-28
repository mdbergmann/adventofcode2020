(defpackage :day6
  (:use :cl :arrows :fiveam))

(in-package :day6)

(defun input-to-list ()
  (uiop:read-file-lines "input/day6.txt"))


;; ------------- tests -------------

(def-suite day6-tests)
(in-suite day6-tests)

