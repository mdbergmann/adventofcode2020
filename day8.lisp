(defpackage :day8
  (:use :cl :arrows :fiveam))

(in-package :day8)

(defun input-to-vector (input)
  (-<> input
      (ppcre:split "\\n" <>)
      (mapcar (lambda (line)
                (ppcre:split " " line)) <>)
      (mapcar (lambda (op)
                (cons (first op)
                      (parse-integer (second op)))) <>)
      (coerce <> 'vector)))

(defparameter *executed-lines* '())
(defparameter *accu* 0)

(defmethod execute-op :before (op line)
  (declare (ignore op))
  (if (member line *executed-lines*)
      (progn
        (format t "Accu: ~a~%" *accu*)
        (error "Line already executed!"))))

(defmethod execute-op (op line)
  (cond
    ((string= "acc" (car op))
     (setf *accu* (+ *accu* (cdr op))))
    ((string= "jmp" (car op))
     (return-from execute-op (+ line (cdr op)))))
  (1+ line))

(defmethod execute-op :after (op line)
  (declare (ignore op))
  (setf *executed-lines* (append (list line) *executed-lines*)))

(defun execute (ops line)
  (let ((op (elt ops line)))
    (execute ops (execute-op op line))))

(defun day8-example ()
  (let ((input (input-to-vector (uiop:read-file-string "input/day8.txt"))))
    (execute input 0)))

;; ------------- tests -------------

(def-suite day8-tests)
(in-suite day8-tests)

(defparameter *test-input*
"nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6")

(def-fixture test-setup ()
  (setf *executed-lines* '())
  (setf *accu* 0)
  (let ((input (input-to-vector *test-input*)))
    (&body)))

(test parse-input-to-vector
  (with-fixture test-setup ()
    (is (= 9 (length input)))
    (is (string= "nop" (car (elt input 0))))
    (is (= 0 (cdr (elt input 0))))
    (is (string= "acc" (car (elt input (1- (length input))))))
    (is (= 6 (cdr (elt input (1- (length input))))))))

(test record-executed-line
  (with-fixture test-setup ()
    (execute-op (elt input 0) 0)
    (is (= 1 (length *executed-lines*)))))

(test dont-execute-when-already-executed--dump-accu
  (with-fixture test-setup ()
    (execute-op (elt input 0) 0)
    (is (= 1 (length *executed-lines*)))
    (handler-case 
        (progn
          (execute-op (elt input 0) 0)
          (fail))
      (error (c) (format t "~a~%" c)))
    (is (= 1 (length *executed-lines*)))))

(test execute-op--acc
  (with-fixture test-setup ()
    (is (= 2 (execute-op (cons "acc" 1) 1)))
    (is (= 3 (execute-op (cons "acc" -1) 2)))
    (is (= 0 *accu*))))

(test execute-op--jmp
  (with-fixture test-setup ()
    (is (= 5 (execute-op (cons "jmp" 4) 1)))
    (is (= 1 (execute-op (cons "jmp" -4) 5)))))

(test day8-example
  (with-fixture test-setup ()
    (day8-example)))

(run! 'parse-input-to-vector)
(run! 'record-executed-line)
(run! 'dont-execute-when-already-executed--dump-accu)
(run! 'execute-op--acc)
(run! 'execute-op--jmp)
(run! 'day8-example)
