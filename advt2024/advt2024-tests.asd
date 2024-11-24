(in-package :asdf-user)
(defsystem "advt2024-tests"
  :description "Test suite for the advt2024 system"
  :author "Paul Warnes <pwarnes@gmail.com>"
  :version "0.0.1"
  :depends-on (:advt2024
               :fiveam)
  :license "BSD"
  :serial t
  :components ((:module "tests"
                        :serial t
                        :components ((:file "packages")
                                     (:file "test-advt2024"))))

  ;; The following would not return the right exit code on error, but still 0.
  ;; :perform (test-op (op _) (symbol-call :fiveam :run-all-tests))
  )
