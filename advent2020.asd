(defsystem "advent2020"
  :version "0.0.1"
  :author "Manfred Bergmann"
  :license "MIT"
  :description "Advent of code 2020"
  :depends-on ("str"
               "alexandria"
               "cl-ppcre"
               "arrows"
               "fiveam"
               "gtwiwtg")
  :components ((:file "day1")
               (:file "day2")
               (:file "day3")
               (:file "day4")
               ))
