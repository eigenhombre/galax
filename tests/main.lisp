(defpackage galax/tests/main
  (:use :cl
        :galax
        :rove))
(in-package :galax/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :galax)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
