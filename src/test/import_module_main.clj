
(require '[modules.module-a :as mod-a]
         '[modules.module-b :as mod-b])

(deftest module-test-load-as
  (is (= 10  (mod-a/helper-a)))
  (is (= 1   (mod-a/helper-b)))
  (is (= 10  ((mod-a/ten-fn))))
  (is (= 11  ((mod-b/eleven-fn))))
  (is (= 1   (mod-a/helper-c)))
  (is (= 42  (mod-b/macro-call)))
  (is (= :b  (:a (mod-a/some-d-list))))
  (is (= 42  (mod-b/native-single-argument 42))))

(require 'modules.module-a
         'modules.module-b)

(require '[modules.module-c :as mod-c]
         'modules.module-d)

(deftest module-test-load
  (is (= 10  (modules.module-a/helper-a)))
  (is (= 1   (modules.module-a/helper-b)))
  (is (= 10  ((modules.module-a/ten-fn))))
  (is (= 11  ((modules.module-b/eleven-fn))))
  (is (= 1   (modules.module-a/helper-c)))
  (is (= 42  (modules.module-b/macro-call)))
  (is (= 25  (cxx " __result = obj<number>(dummy_native_fn());")))
  (is (= 2   (cxx "__result = obj<number>((number_t)std::sqrt(4));"))))

(run-all-tests)
