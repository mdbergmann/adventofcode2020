(ql:quickload :alexandria)

(defpackage :day1
  (:use :cl))

(in-package :day1)

(defun input-to-list (input delimiter)
  (mapcar #'parse-integer
           (uiop:read-file-lines input)))

(defparameter *day1-in* (input-to-list #P"input/day1.txt" #\linefeed))

(defun day1-1 ()
  (caar
   (remove-if #'null
              (loop :for i :in *day1-in*
                    :collect
                    (loop :for j :in *day1-in*
                          :if (= 2020 (+ i j))
                            :collect (* i j))))))

(defun day1-2 ()
  (let ((result))
    (loop :for i :in *day1-in*
          :do
          (loop :for j :in *day1-in*
                :do
                (loop :for k :in *day1-in*
                      :if (= 2020 (+ i j k))
                        :do (setf result (* i j k)))))
    result))

(defun day1-2_1 ()
  (dolist (i *day1-in*)
    (dolist (j *day1-in*)
      (dolist (k *day1-in*)
        (when (= 2020 (+ i j k))
          (return-from day1-2_1 (* i j k)))))))
