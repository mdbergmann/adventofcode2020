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

(defun has-executed-line (line)
  (member line *executed-lines*))

(defmethod execute-op :before (op line)
  (declare (ignore op))
  (if (has-executed-line line)
      (progn
        (format t "Accu: ~a~%" *accu*)
        (error "Line already executed!"))))

(defmethod execute-op (op line)
  ;;(format t "executing: ~a~%" op)
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
  (if (< line (length ops))
      (let ((op (elt ops line)))
        (execute ops (execute-op op line)))))

(defun execute-2 (ops line)
  (loop :for i :from (1- (length ops)) :downto 0
        :for op-lst = (copy-seq ops)
        :for back-op = (elt ops i)
        :do
           (setf *executed-lines* '())
           (setf *accu* 0)
           ;;(format t "back-op: ~a~%" back-op)
           (handler-case
               (progn
                 (cond
                   ((string= "jmp" (car back-op))
                    (setf (aref op-lst i)
                          (cons "nop" (cdr back-op)))
                    (format t "replaced 'jmp' with 'nop' at ~a~%" i))
                   ((string= "nop" (car back-op))
                    (setf (aref op-lst i)
                          (cons "jmp" (cdr back-op)))
                    (format t "replaced 'nop' with 'jmp' at ~a~%" i)))
                 (execute op-lst line)
                 (return-from execute-2 t))
             (error (c)
               (format t "~a~%" c)))))

(defun day8-1 ()
  (setf *executed-lines* '())
  (setf *accu* 0)
  (let ((input (input-to-vector (uiop:read-file-string "input/day8.txt"))))
    (execute input 0)))

(defun day8-2 ()
  (setf *executed-lines* '())
  (setf *accu* 0)
  (let ((input (input-to-vector (uiop:read-file-string "input/day8.txt"))))
    (execute-2 input 0)
    (format t "Ended. Accu: ~a~%" *accu*)))

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

(test day8-1-example
  (with-fixture test-setup ()
    (handler-case
        (execute input 0)
      (error (c)
        (format t "err: ~a~%" c)))
    (format t "Accu: ~a~%" *accu*)
    (is-true t)))

(test day8-2-example
  (with-fixture test-setup ()
    (handler-case
        (execute-2 input 0)
      (error (c)
        (format t "err: ~a~%" c)))
    (format t "Accu: ~a~%" *accu*)
    (is (= 8 *accu*))))

(run! 'parse-input-to-vector)
(run! 'record-executed-line)
(run! 'dont-execute-when-already-executed--dump-accu)
(run! 'execute-op--acc)
(run! 'execute-op--jmp)
(run! 'day8-1-example)
(run! 'day8-2-example)
