(defpackage :day6
  (:use :cl :arrows :fiveam))

(in-package :day6)

(defun input-to-list (input)
  (labels ((split-into-lines (s) (ppcre:split "\\n\\n" s))
           (parse-to-groups (items)
             (mapcar (lambda (group)
                       (-<> group
                            (ppcre:regex-replace-all "\\n" <> "")
                            (coerce 'list)
                            (remove-duplicates)))
                     items)))
    (-> input
        (split-into-lines)
        (parse-to-groups))))

(defun day6 ()
  (->> (input-to-list (uiop:read-file-string "input/day6.txt"))
       (mapcar #'length)
       (reduce #'+)))



(defun input-to-list-2 (input)
  (labels ((split-into-lines (s) (ppcre:split "\\n\\n" s))
           (parse-to-groups (items)
             (mapcar (lambda (group)
                       (->> group
                            (ppcre:split "\\n")
                            (mapcar (lambda (l) (coerce l 'list)))
                            (mapcar #'remove-duplicates)))
                     items)))
    (-> input
        (split-into-lines)
        (parse-to-groups))))

(defun count-yes-qs-2 (subgroup)
  (->> (loop :for q :in (first subgroup)
             :collect
             (every (lambda (x)
                      (member q x :test #'eql))
                    subgroup))
       (remove-if #'null)
       (length)))

(defun map-all-yes-qs-2 (groups)
  (mapcar (lambda (group)
            (count-yes-qs-2 group))
          groups))

(defun day6-2 ()
  (->> (input-to-list-2 (uiop:read-file-string "input/day6.txt"))
       (map-all-yes-qs-2)
       (reduce #'+)))

;; ------------- tests -------------

(def-suite day6-tests)
(in-suite day6-tests)

(defvar *test-input* "abc

a
b
c

ab
ac

a
a
a
a

b")

(test test-parse-input-to-groups
  (let ((input (input-to-list (uiop:read-file-string "input/day6.txt"))))
    (is (listp input))
    (is (= 487 (length input)))
    (is (every #'listp input))))

(test test-day6-test-input
  (let ((input (input-to-list *test-input*)))
    (is (= 11 (reduce #'+
                      (->> input
                           (mapcar #'length)))))))

(test test-day6-2-test-input
  (let ((input (input-to-list-2 *test-input*)))
    (print input)
    (is (= 1 (count-yes-qs-2 '((#\a) (#\a) (#\a)))))
    (is (= 1 (car (map-all-yes-qs-2 '(((#\a) (#\a) (#\a)))))))
    (is (= 6 (reduce #'+
                     (->> input
                          (map-all-yes-qs-2)
                          (print)))))
    ))

;;(run! 'test-parse-input-to-groups)
;;(run! 'test-day6-test-input)
;;(run! 'test-day6-2-test-input)
