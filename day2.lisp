(defpackage :day2
  (:use :cl))

(in-package :day2)

(defun input-to-list ()
  (uiop:read-file-lines #P"~/Development/MySources/advent2020/input/day2.txt"))

(defun day2-doline (line-data apply-fun)
  (let* ((sep-line (str:split " " line-data))
         (col1-str (str:split "-" (first sep-line)))
         (col1 (cons (parse-integer (first col1-str))
                     (parse-integer (second col1-str))))
         (col2 (str:substring 0 1 (second sep-line)))
         (col3 (third sep-line)))
    (apply apply-fun (list col1 col2 col3))))

(defun day2 (all-data)
  (length (mapcan (lambda (x) (if (eq t x) (list x)))
                  (mapcar
                   (lambda (line)
                     (day2-doline
                      line
                      (lambda (col1 col2 col3)
                        (let ((occ-len (str:count-substring col2 col3)))
                          (if (and (>= occ-len (car col1)) (<= occ-len (cdr col1)))
                              t
                              nil)))))
                   all-data))))

(defun test-day2-example ()
    (let* ((test-data '("1-3 a: abcde" "1-3 b: cdefg" "2-9 c: ccccccccc"))
           (result (day2 test-data)))
      (assert (= 2 result))
      result))

(defun test-day2-real ()
  (let ((result (day2 (input-to-list))))
      result))


(defun day2_2 (all-data)
  (length (mapcan (lambda (x) (if (eq t x) (list x)))
                  (mapcar
                   (lambda (line)
                     (day2-doline
                      line
                      (lambda (col1 col2 col3)
                        (let ((str-1 (str:s-nth (1- (car col1)) col3))
                              (str-2 (str:s-nth (1- (cdr col1)) col3)))
                        (cond
                          ((and (string= col2 str-1)
                                (string= col2 str-2))
                           nil)
                          ((or (string= col2 str-1)
                               (string= col2 str-2))
                           t)
                          (t nil))))))
                   all-data))))

(defun test-day2_2-example ()
  (let* ((test-data '("1-3 a: abcde" "1-3 b: cdefg" "2-9 c: ccccccccc"))
         (result (day2_2 test-data)))
    (assert (= 1 result))
    result))

(defun test-day2_2-real ()
  (let* ((result (day2_2 (input-to-list))))
    result))
