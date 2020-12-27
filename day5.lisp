(defpackage :day5
  (:use :cl :arrows :fiveam))

(in-package :day5)

(defun input-to-list ()
  (uiop:read-file-lines "input/day5.txt"))

(defun divide-ids-rec (ids min max lower-key upper-key)
  (let ((head (car ids))
        (tail (cdr ids)))
    (cond
      ((eq nil head) min)
      ((eq nil tail) (cond
                       ((eql head lower-key) min)
                       ((eql head upper-key) max)))
      (t (let* ((half (/ (1+ (- max min)) 2))
                (lower (cons min (+ min (1- half))))
                (upper (cons (+ min half) max)))
           (format t "lower: ~a, upper: ~a~%" lower upper)
           (cond
             ((eql head lower-key)
              (divide-ids-rec tail (car lower) (cdr lower) lower-key upper-key))
             ((eql head upper-key)
              (divide-ids-rec tail (car upper) (cdr upper) lower-key upper-key))))))))

(defun calc-row (rowids min max)
  (divide-ids-rec rowids min max #\F #\B))

(defun calc-col (colids min max)
  (divide-ids-rec colids min max #\L #\R))

(defun calc-seat-id (row col)
  (+ (* row 8) col))

(defun rowids ()
  (mapcar (lambda (bpass)
            (let ((row (calc-row (coerce (str:substring 0 7 bpass) 'list) 0 127))
                  (col (calc-col (coerce (str:substring 7 10 bpass) 'list) 0 7)))
              (calc-seat-id row col)))
          (input-to-list)))

(defun day5 ()
  (reduce #'max (rowids)))

(defun day5-2 ()
  (loop :for i :from 81 :to 855
        :if (null (member i *rowids* :test #'=))
          :do (print i)))

;; ------------- tests -------------

(def-suite day5-tests)
(in-suite day5-tests)

(test test-calc-row
  (is (= 0 (calc-row nil 0 127)))
  (is (= 0 (calc-row '(#\F) 0 127)))
  (is (= 127 (calc-row '(#\B) 0 127)))
  (is (= 63 (calc-row '(#\F #\B) 0 127)))
  (is (= 0 (calc-row '(#\F #\F) 0 127)))
  (is (= 64 (calc-row '(#\B #\F) 0 127)))
  (is (= 127 (calc-row '(#\B #\B) 0 127)))
  (is (= 32 (calc-row '(#\F #\B #\F) 0 127)))
  (is (= 96 (calc-row '(#\B #\B #\F) 0 127)))
  (is (= 44 (calc-row '(#\F #\B #\F #\B #\B #\F #\F) 0 127)))
  (is (= 70 (calc-row (coerce "BFFFBBF" 'list) 0 127)))
  (is (= 14 (calc-row (coerce "FFFBBBF" 'list) 0 127))))

(test test-calc-col
  (is (= 0 (calc-col nil 0 7)))
  (is (= 0 (calc-col '(#\L) 0 7)))
  (is (= 7 (calc-col '(#\R) 0 7)))
  (is (= 7 (calc-col '(#\R #\R #\R) 0 7)))
  (is (= 4 (calc-col '(#\R #\L #\L) 0 7))))

(test test-seat-id
  (is (= 567 (calc-seat-id 70 7))))
