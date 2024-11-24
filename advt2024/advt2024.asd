(in-package :asdf-user)

(defsystem "advt2024"
  :author "Paul Warnes <pwarnes@gmail.com>"
  :version "0.0.1"
  :license "MIT"
  :description "advent of code 2024"
  :homepage ""
  :bug-tracker ""
  :source-control (:git "")

  ;; Dependencies.
  :depends-on (
               #:str
               #:cl-ppcre)

  ;; Project stucture.
  :serial t
  :components ((:module "src"
                        :serial t
                        :components ((:file "packages")
                                     (:file "utilities")
                                     (:file "day01")
                                     (:file "advt2024"))))

  ;; Build a binary:
  ;; don't change this line.
  :build-operation "program-op"
  ;; binary name: adapt.
  :build-pathname "advt2024"
  ;; entry point: here "main" is an exported symbol. Otherwise, use a double ::
  :entry-point "advt2024:main")
