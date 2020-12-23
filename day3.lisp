(ql:quickload :arrows)

(defpackage :day3
  (:use :cl :arrows))

(in-package :day3)

(defun input-to-list (input)
  (uiop:read-file-lines input))

(defun convert-input2array ()
  (let ((input-as-lines (input-to-list "input/day3.txt")))
    (coerce input-as-lines 'array)))

(defun walk (current-place step)
  (cons (+ (car step) (car current-place))
        (+ (cdr step) (cdr current-place))))

(defun position-value (place data)
  (elt (elt data (cdr place)) (car place)))

(defun plot-map-positions (width height step)
  "collects a list of 'places' through the map"
  (loop :repeat (1- height)
        :with start = '(0 . 0)
        :for new-place = (walk (if (null new-place) start new-place) step)
        :if (>= (car new-place) width)
          ;; if we're beyond the width, remap to 0
          :do (setf new-place (cons (abs (- width (car new-place))) (cdr new-place)))
        :while (< (cdr new-place) height)
        :collect new-place))

(defun map-course (course data)
  (coerce (mapcar (lambda (pos)
                    (position-value pos data))
                  course) 'string))

(defun calculate-trees (course-data)
  (length (remove-if (lambda (c) (string= "." c)) course-data)))

(defun process-input-data (input-data step)
  (let ((map-size (cons (length (elt input-data 0)) (length input-data))))
    (-> (plot-map-positions (car map-size) (cdr map-size) step)
        (map-course input-data)
        (calculate-trees))))

(defun real-process-input-data ()
  (process-input-data (convert-input2array) '(3 . 1)))

(defun real-process-input-data-2 ()
  (let* ((input-data (convert-input2array))
         (result (reduce #'* (mapcar (lambda (step) (process-input-data input-data step))
                                     '((1 . 1)
                                       (3 . 1)
                                       (5 . 1)
                                       (7 . 1)
                                       (1 . 2)))
                         :initial-value 1)))
    result))

;; tests

;; test data
(defparameter *test-data*
  '("..##......."
    "#...#...#.."
    ".#....#..#."
    "..#.#...#.#"
    ".#...##..#."
    "..#.##....."
    ".#.#.#....#"
    ".#........#"
    "#.##...#..."
    "#...##....#"
    ".#..#...#.#"))

(defun test-process-input-data ()
  (let ((result (process-input-data *test-data* '(3 . 1))))
    (assert (= 7 result))
    result))

(defun test-process-input-data-2 ()
  (let ((result (reduce #'*
                        (mapcar (lambda (step)
                                  (process-input-data *test-data* step))
                                '((1 . 1)
                                  (3 . 1)
                                  (5 . 1)
                                  (7 . 1)
                                  (1 . 2)))
                        :initial-value 1)))
    (assert (= 336 result))
    result))

(defun test-convert-input ()
  (let ((input (convert-input2array)))
    (assert (arrayp input) nil "Input is no array!")
    (assert (arrayp (elt input 0)) nil "Line is no array")
    (assert (string= "." (elt (elt input 0) 0)) nil "Line is no array"))
  t)
