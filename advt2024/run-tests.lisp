
(load "advt2024.asd")
(load "advt2024-tests.asd")

(ql:quickload "advt2024-tests")

(in-package :advt2024-tests)

(uiop:quit (if (run-all-tests) 0 1))
