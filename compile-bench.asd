(asdf:defsystem #:compile-bench
  :description "Utilities for benchmarking compilers"
  :author "Bike <aeshtaer@gmail.com>"
  :license "Public Domain"
  :depends-on (#:alexandria)
  :components
  ((:file "packages")
   (:file "compile-bench" :depends-on ("packages"))
   (:file "macros" :depends-on ("packages"))
   (:file "arrays" :depends-on ("compile-bench" "packages"))
   (:file "crc40" :depends-on ("compile-bench" "packages"))
   (:file "gabriel" :depends-on ("compile-bench" "packages"))
   (:file "richards" :depends-on ("compile-bench" "packages"))))
