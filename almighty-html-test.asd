(defsystem "almighty-html-test"
  :class :package-inferred-system
  :pathname "tests"
  :depends-on ("rove"
               "almighty-html-test/utils"
               "almighty-html-test/element"
               "almighty-html-test/dsl")
  :perform (test-op (o c) (symbol-call :rove :run c :style :dot)))
