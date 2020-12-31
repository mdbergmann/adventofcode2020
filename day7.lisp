(defpackage :day7
  (:use :cl :arrows :fiveam))

(in-package :day7)

(defstruct bag
  (name "" :type string)
  (sub-bags '() :type list))

(defun get-sub-bag (name lst)
  (assoc name lst :test #'string=))

(defun input-to-list (input)
  (uiop:read-file-lines input))

(defun parse-bag-from-line (line)
  (flet ((parse-main (line-part)
           (multiple-value-bind (match groups)
               (ppcre:scan-to-strings
                "^([a-z\\s]+)\\sbags\\scontain\\s(\\w+)\\s([a-z\\s]+)\\sbag"
                line-part)
             (declare (ignore match))
             (destructuring-bind (base-bag other-bag-count other-bag-name)
                 (coerce groups 'list)
               (cond
                 ((string= "no" other-bag-count)
                  (values base-bag nil))
                 (t
                  (values base-bag
                          (list (cons other-bag-name
                                      (parse-integer other-bag-count)))))))))
         (parse-add (line-part)
           (multiple-value-bind (match groups)
               (ppcre:scan-to-strings
                "(\\w{1,2})\\s([a-z\\s]+)\\sbag"
                line-part)
             (declare (ignore match))
             (destructuring-bind (bag-count bag-name)
                 (coerce groups 'list)
               (list (cons bag-name
                           (parse-integer bag-count)))))))
           
          (loop :for line-part :in (ppcre:split ", " line)
                :for i :from 0
                :with bag = (make-bag)
                :do
                   (if (= 0 i)
                       (multiple-value-bind (bag-name subbag) (parse-main line-part)
                         (setf (bag-name bag) bag-name)
                         (if (not (null subbag))
                             (setf (bag-sub-bags bag)
                                   (append (bag-sub-bags bag) subbag))))
                       (setf (bag-sub-bags bag)
                             (append (bag-sub-bags bag) (parse-add line-part))))
                :finally (return bag))))

(defun parse-input (bags)
  (mapcar #'parse-bag-from-line bags))

(defun find-distinct-containing-bags-of (bag-name bags)
  (length
   (remove-duplicates
    (loop :with search-for-bags = (list bag-name)
          :with bags-found = t
          :with collected-bags = '()
          :while bags-found
          :do
             (let ((found-bags
                     (->> (loop :for bag :in bags
                                :append
                                (loop :for bag-name :in search-for-bags
                                      :collect
                                      (if (not (null (get-sub-bag bag-name (bag-sub-bags bag))))
                                          bag)))
                          (remove-if #'null)
                          (remove-duplicates))))
               (setf search-for-bags (mapcar #'bag-name found-bags))
               (setf collected-bags
                     (append collected-bags found-bags))
               (setf bags-found (> (length found-bags) 0)))
          :finally (return collected-bags)))))

(defun day7 ()
  (let ((bags (parse-input (uiop:read-file-lines "input/day7.txt"))))
    (find-distinct-containing-bags-of "shiny gold" bags)))

(defun find-bag-paths (bag-name all-bags)
  (labels ((find-bag (bag-name bags)
             (find bag-name bags :test #'string= :key #'bag-name))
           (collect-bag-paths (bags)
             (loop :for bag :in bags
                   :and bag-paths = '()
                   :with combined-paths = '()
                   :do
                      (labels ((record-bag (bag)
                                 (format t "~%recording: ~a~%" bag)
                                 (setf bag-paths (append bag-paths (list bag))))
                               (collect-path-rec (bag-lst)
                                 (format t "~%bags: ~a~%" bag-lst)
                                 (format t "bag-paths: ~a~%" bag-paths)
                                 (cond
                                   ((null bag-lst) '())
                                   (t (let* ((head
                                               (car bag-lst))
                                             (sub-bag
                                               (find-bag (car head) all-bags))
                                             (tail
                                               (cdr bag-lst)))
                                        (record-bag head)
                                        (collect-path-rec (bag-sub-bags sub-bag))
                                        (format t "Collect tail bags...~%")
                                        (collect-path-rec tail))))))
                        (record-bag bag)
                        (collect-path-rec
                         (bag-sub-bags
                          (find-bag (car bag) all-bags)))
                        (setf combined-paths
                              (append combined-paths (list bag-paths)))
                        (format t "combined-paths: ~a~%" combined-paths))
                   :finally (return combined-paths)))
           (mult-and-add-path-components (path)
             (format t "path: ~a~%" path)
             (if (null path)
                 0
                 (+ (reduce #'* path)
                    (mult-and-add-path-components (cdr path))))))
    (->> (find-bag bag-name all-bags)
         (bag-sub-bags)
         (print)
         (collect-bag-paths)
         (print)
         (mapcar #'print)
         (mapcar (lambda (bag-path)
                   (mult-and-add-path-components
                    (mapcar #'cdr bag-path))))
         (reduce #'+)
         )))

(defun day7-2 ()
  (let ((bags (parse-input (uiop:read-file-lines "input/day7.txt"))))
    (find-bag-paths "shiny gold" bags)))

;; ------------- tests -------------

(def-suite day7-tests)
(in-suite day7-tests)

(defvar *test-input* "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.")

(test parse-bag-test--no-other
  (let* ((line "faded blue bags contain no other bags.")
         (parsed-bag (parse-bag-from-line line)))
    (print parsed-bag)
    (is (typep parsed-bag 'bag))
    (is (string= "faded blue" (bag-name parsed-bag)))
    (is (null (bag-sub-bags parsed-bag)))))

(test parse-bag-test--one-other
  (let* ((line "bright white bags contain 1 shiny gold bag.")
         (parsed-bag (parse-bag-from-line line)))
    (print parsed-bag)
    (print (bag-sub-bags parsed-bag))
    (is (typep parsed-bag 'bag))
    (is (string= "bright white" (bag-name parsed-bag)))
    (is (= 1 (length (bag-sub-bags parsed-bag))))
    (is (equalp (cons "shiny gold" 1) (get-sub-bag "shiny gold" (bag-sub-bags parsed-bag))))
    ))

(test parse-bag-test--two-other
  (let* ((line "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.")
         (parsed-bag (parse-bag-from-line line)))
    (print parsed-bag)
    (is (typep parsed-bag 'bag))
    (is (string= "muted yellow" (bag-name parsed-bag)))
    (is (equalp (cons "shiny gold" 2) (first (bag-sub-bags parsed-bag))))
    (is (equalp (cons "faded blue" 9) (second (bag-sub-bags parsed-bag))))))

(test parse-full--test-data
  (let ((bags (parse-input (ppcre:split "\\n" *test-input*))))
    (is (= 9 (length bags)))))

(test day7-1-example
  (format t "~%")
  (let ((bags (parse-input (ppcre:split "\\n" *test-input*))))
    (is (= 4 (find-distinct-containing-bags-of "shiny gold" bags)))))

;;(defparameter *test-input-2* "shiny gold bags contain 2 dark red bags
(defparameter *test-input-2* "shiny gold bags contain 2 dark red bags, 2 dotted blue bag.
dark red bags contain 2 dark orange bags.
dark orange bags contain 2 dark yellow bags.
dark yellow bags contain 2 dark green bags.
dark green bags contain 2 dark blue bags.
dark blue bags contain 2 dark violet bags.
dark violet bags contain no other bags.
dotted blue bags contain 1 dashed yellow bag, 2 vibrant plum bag.
dashed yellow bags contain no other bags.
vibrant plum bags contain no other bags.")

(test day7-2-example
  (format t "~%")
  (let ((bags (parse-input (ppcre:split "\\n" *test-input-2*))))
    (is (= 134 (find-bag-paths "shiny gold" bags)))))

(run! 'parse-bag-test--no-other)
(run! 'parse-bag-test--one-other)
(run! 'parse-bag-test--two-other)
(run! 'parse-full--test-data)
(run! 'day7-1-example)
(run! 'day7-2-example)
