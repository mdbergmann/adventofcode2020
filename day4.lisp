(in-package :cl-user)
(defpackage :day4
  (:use :cl :arrows :fiveam))

(in-package :day4)

(defparameter *required-props*
  '("byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"))

(defun input-to-alist (input)
  (labels ((split-into-lines (s) (ppcre:split "\\n\\n" s))
           (props-to-acons (props)
             (mapcar (lambda (prop-item)
                       (let ((comps (ppcre:split ":" prop-item)))
                         (cons (first comps) (second comps))))
                     props))
           (to-alist (lst)
             (mapcar (lambda (item)
                       (let ((props (ppcre:split "\\s" item)))
                         (props-to-acons props)))
                     lst)))
    (-> (uiop:read-file-string input)
        (split-into-lines)
        (to-alist))))

(defun get-aprop (prop list)
  (assoc prop list :test #'equalp))

(defun valid-year-p (year min max)
  (and (>= year min) (<= year max)))

(defun valid-4dig-year-p (year-prop min max)
  (-> year-prop
      (cdr)
      (parse-integer)
      (valid-year-p min max)))

(defun valid-byr-p (pass)
  (-> (get-aprop "byr" pass)
      (valid-4dig-year-p 1920 2002)))

(defun valid-iyr-p (pass)
  (-> (get-aprop "iyr" pass)
      (valid-4dig-year-p 2010 2020)))

(defun valid-eyr-p (pass)
  (-> (get-aprop "eyr" pass)
      (valid-4dig-year-p 2020 2030)))

(defun valid-hgt-p (pass)
  (flet ((scan-groups (hgt)
           (multiple-value-bind (match groups)
               (ppcre:scan-to-strings "(\\d{2,3})(.{2})" hgt)
             (if (null match)
                 nil
                 groups)))
         (in-range-p (scan-groups)
           (when (null scan-groups)
             (return-from valid-hgt-p nil))
           (let* ((group1 (elt scan-groups 0))
                  (group2 (elt scan-groups 1))
                  (height (parse-integer group1))
                  (metric group2))
             (cond
               ((string= "in" metric)
                (if (and (>= height 59) (<= height 76))
                    t
                    nil))
               ((string= "cm" metric)
                (if (and (>= height 150) (<= height 193))
                    t
                    nil))
               (t nil)))))
    (-> (get-aprop "hgt" pass)
        (cdr)
        (scan-groups)
        (in-range-p))))

(defun valid-hcl-p (pass)
  (flet ((scan-valid-p (hcl) (ppcre:scan "#[0-9,a-f]{6}" hcl)))
    (-> (get-aprop "hcl" pass)
        (cdr)
        (scan-valid-p))))

(defun valid-ecl-p (pass)
  (-> (get-aprop "ecl" pass)
      (cdr)
      (member '("amb" "blu" "brn" "gry" "grn" "hzl" "oth") :test #'string=)))

(defun valid-pid-p (pass)
  (flet ((scan-valid-p (pid) (ppcre:scan "^[0-9]{9}$" pid)))
    (-> (get-aprop "pid" pass)
        (cdr)
        (scan-valid-p))))

(defun valid-pass-p (pass)
  (and (every (lambda (prop)
                (member prop (mapcar #'car pass) :test #'equalp))
              *required-props*)
       (valid-pid-p pass)
       (valid-byr-p pass)
       (valid-ecl-p pass)
       (valid-eyr-p pass)
       (valid-hcl-p pass)
       (valid-iyr-p pass)
       (valid-hgt-p pass)))

(defun day4-1 ()
  (-<> (input-to-alist "input/day4.txt")
       (mapcar #'valid-pass-p <>)
       (remove-if #'null <>)
       (length)))

;; -------- tests -----------

(def-suite day4-tests)
(in-suite day4-tests)

(test test-input
  (let ((input-list (input-to-alist "input/day4.txt")))
    (is (listp input-list))
    (print (first input-list))
    (is (listp (first input-list)))
    (print (get-aprop "hgt" (first input-list)))
    (is (string= "177cm" (cdr (get-aprop "hgt" (first input-list)))))
    (is (valid-pass-p (first input-list)))))

(test test-hgt-p
  (is-false (valid-hgt-p '(("hgt" . "58"))))
  (is-false (valid-hgt-p '(("hgt" . "58in"))))
  (is (valid-hgt-p '(("hgt" . "59in"))))
  (is-false (valid-hgt-p '(("hgt" . "77in"))))
  (is-false (valid-hgt-p '(("hgt" . "149"))))
  (is-false (valid-hgt-p '(("hgt" . "149cm"))))
  (is (valid-hgt-p '(("hgt" . "150cm"))))
  (is (valid-hgt-p '(("hgt" . "193cm"))))
  (is-false (valid-hgt-p '(("hgt" . "194cm")))))

(test test-byr-p
  (is-false (valid-byr-p '(("byr" . "1919"))))
  (is-false (valid-byr-p '(("byr" . "2003"))))
  (is (valid-byr-p '(("byr" . "1920"))))
  (is (valid-byr-p '(("byr" . "2002"))))
  (is-false (valid-byr-p '(("byr" . "200")))))

(test test-iyr-p
  (is-false (valid-iyr-p '(("iyr" . "2009"))))
  (is-false (valid-iyr-p '(("iyr" . "2021"))))
  (is (valid-iyr-p '(("iyr" . "2010"))))
  (is (valid-iyr-p '(("iyr" . "2020"))))
  (is-false (valid-iyr-p '(("iyr" . "200")))))

(test test-eyr-p
  (is-false (valid-eyr-p '(("eyr" . "2019"))))
  (is-false (valid-eyr-p '(("eyr" . "2031"))))
  (is (valid-eyr-p '(("eyr" . "2020"))))
  (is (valid-eyr-p '(("eyr" . "2030"))))
  (is-false (valid-eyr-p '(("eyr" . "200")))))

(test test-hct-p
  (is (valid-hcl-p '(("hcl" . "#123abc"))))
  (is-false (valid-hcl-p '(("hcl" . "#123abz"))))
  (is-false (valid-hcl-p '(("hcl" . "123abc")))))

(test test-ecl-p
  (is (valid-ecl-p '(("ecl" . "brn"))))
  (is (valid-ecl-p '(("ecl" . "blu"))))
  (is (valid-ecl-p '(("ecl" . "amb"))))
  (is (valid-ecl-p '(("ecl" . "gry"))))
  (is (valid-ecl-p '(("ecl" . "grn"))))
  (is (valid-ecl-p '(("ecl" . "hzl"))))
  (is (valid-ecl-p '(("ecl" . "oth"))))
  (is-false (valid-ecl-p '(("ecl" . "foo")))))

(test test-pid-p
  (is (valid-pid-p '(("pid" . "000000001"))))
  (is-false (valid-pid-p '(("pid" . "0123456789")))))
