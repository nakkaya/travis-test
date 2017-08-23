
(require 'modules.module-e)

(deftest simple-module-test
  (is (= 1  (modules.module-e/foo))))

(run-all-tests)
