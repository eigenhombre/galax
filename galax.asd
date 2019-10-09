(defsystem "galax"
  :version "0.1.0"
  :author ""
  :license ""
  :build-operation "program-op"
  :build-pathname "galax"
  :entry-point "galax:main"
  :depends-on ("beast"
               "cl-oju"
               "arrow-macros"
               "syllab"
               "cl-utilities"
               "trivialtests")
  :components ((:module "src"
                        :components
                        ((:file "package")
                         (:file "util" :depends-on ("package"))
                         (:file "neighbors" :depends-on ("package"))
                         (:file "lex" :depends-on ("package" "util"))
                         (:file "main" :depends-on ("package"
                                                    "neighbors"
                                                    "lex"
                                                    "util")))))
  :description ""
  :in-order-to ((test-op (test-op "galax/tests"))))

(defsystem "galax/tests"
  :author ""
  :license ""
  :depends-on ("galax"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for galax"
  :perform (test-op (op c) (symbol-call :rove :run c)))
