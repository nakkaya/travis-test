
(ns compiler
  (:refer-clojure :exclude [compile])
  (:use [ferret.core] :reload)
  (:use [clojure.test]
        [clojure.java.shell]
        [clojure.tools [logging :only [log warn info]]]))

(set-log-level! :info)

(defn exec-check [& cmd]
  (let [ret (with-sh-dir "./" (apply sh cmd))]
    (if (not= 0 (:exit ret))
      (do (warn (str "exec-check failure" cmd))
          (warn (:err ret))
          (System/exit 1))
      (:out ret))))

(defn static-check [f]
  (exec-check
   "cppcheck" "-q" "--std=c++11" "--template=gcc" "--enable=all" "--error-exitcode=1" f))


(def cxx-opts-common ["-std=c++11"
                      "-pedantic"
                      "-Werror"
                      "-Wall"
                      "-Wextra"
                      "-Wconversion"
                      "-Wpointer-arith"
                      "-Wmissing-braces"
                      "-Woverloaded-virtual"
                      "-Wuninitialized"
                      "-Winit-self"
                      "-pthread"])

(def cxx-llvm-opts ["-fsanitize=leak" "-fsanitize=undefined"])

(def cxx [["/usr/bin/g++"         (concat cxx-opts-common ["-fno-rtti"])]
          ["/usr/bin/g++"         (concat cxx-opts-common ["-O3"])]
          ["/usr/bin/clang++-3.6" (concat cxx-opts-common cxx-llvm-opts)]
          ["/usr/bin/clang++-3.6" (concat cxx-opts-common cxx-llvm-opts ["-O3"])]])

(defn test-file-aux [f]
  (info (str "testing " f))
  (let [f-name (gensym)
        file (str f-name ".cpp")
        specs (-> ((build-specs f {}))
                  (assoc :output-path "./")
                  (assoc :base-name   f-name))]
    (compile->cpp (read-clojure-file f) specs)
    (static-check file)
    (let [output (reduce (fn[h [cxx flags]]
                           (if (file-exists cxx)
                             (do (compile->binary
                                  (assoc specs
                                         :compiler cxx
                                         :compiler-options (concat flags ["-o" f-name])))
                                 (let [output (exec-check (str "./" f-name) "1" "2")]
                                   (when (not (empty? output))
                                     (println )
                                     (println output))
                                   (conj h output)))
                             h))
                         [] cxx)]
      (with-sh-dir "./"
        (sh "rm" "-f" file)
        (sh "rm" "-f" (str f-name)))
      (filter #(not (empty? %)) output))))

(defn test-file [f]
  (binding [clojure.test/report
            (fn [ev]
              (inc-report-counter (-> ev :type))
              (when (= (-> ev :type) :fail)
                (warn (str "test file failed => " f))))]
    (is (empty? (test-file-aux f)))))

(deftest test-ferret
  (test-file "test/simple_module_main.clj")
  (test-file "test/import_module_main.clj")
  (test-file "test/import_module_empty_aux_a.clj")
  (test-file "test/import_module_empty_aux_b.clj")
  (test-file "test/runtime.clj")
  (test-file "test/memory_pool.clj"))

(defn exec-form [form & [opts]]
  (let [options (compile-options {:compiler-options cxx-opts-common})
        options (merge options opts)]
    (compile->cpp form options)
    (compile->binary options)
    (static-check "solution.cpp")
    (let [out (exec-check "./a.out" "1" "2")]
      (with-sh-dir "./"
        (sh "rm" "-rf" "a.out")
        (sh "rm" "-rf" "solution.cpp"))
      out)))

(defn report-test-failure [event]
  (let [actual (with-out-str
                 (clojure.pprint/pprint (:actual event)))
        expected (with-out-str
                   (clojure.pprint/pprint (:expected event)))]
    (let [type (-> event :type)]
      (inc-report-counter type)
      (when (= type :fail)
        (warn (str "test failure" \newline \newline actual \newline expected))))))

(defmacro compare-output [res & body]
  `(binding [clojure.test/report report-test-failure]
     (is (= ~res (exec-form (quote ~body))))))

(deftest compiler-core
  (let [program (compile '((defn one-plus-one []
                             (+ 1 1))

                           (while true
                             (+ 1 1))) {})]
    ;;while shoud use one-plus-one in its body
    ;;check lambda-already-defined?
    (is (= 2 (count (select-form program (fn [f] (= 'one_plus_one f))))))
    ;;test shake-concat
    (is (= '((defn c [] 1)
             (defn b [] (c))
             (defn a [] (b))
             (a))
           (shake-concat '((defn no-call-a [])
                           (defnative no-call-b [] (on "" ""))
                           (defn c [] 1)
                           (defn b [] (c))
                           (defn a [] (b)))
                         '((a)))))
    (is (= '((defn y [])
             (let [a 1]
               (defn b []))
             (println (b) (y)))
           (shake-concat '((defn x [] )
                           (defn y [] )
                           (let [a 1]
                             (defn b [] )
                             (defn c [] a)))
                         '((println (b) (y))))))
    (is (= '((defn p-create []) (defn p-update []))
           (take 2 (shake-concat '((defn p-create [])
                                   (defn p-update [])
                                   (defmacro pc [& options]
                                     `(~'let [controller# (~'p-create)]
                                       (~'fn [input#] (~'p-update)))))
                                 '((pc))))))
    (is (= '(defn new-lazy-seq [f] )
           (first (shake-concat '((defn new-lazy-seq [f] )
                                  (defmacro lazy-seq [& body]
                                    `(~'new-lazy-seq (~'fn [] ~@body)))
                                  (defn range
                                    ([high]
                                     (range 0 high))
                                    ([low high]
                                     (if (< low high)
                                       (cons low (lazy-seq
                                                  (range (inc low) high)))))))
                                '((range 10))))))))
(deftest testing-unit-test
  (is (= "" (exec-form '((assert (= 2 1) (print "fail"))) {:release true})))
  (compare-output "" (run-all-tests))
  (compare-output
   (str "fail in (= 2 3) \n"
        " expected 2.0000 \n"
        "      got 3.0000\n"
        "fail (not (some-true))\n"
        "fail in (= 5 (apply + (list 1 2 3))) \n"
        " expected 5.0000 \n"
        "      got 6.0000\n"
        "fail in (= 3 (some-fn)) \n"
        " expected 3.0000 \n" 
        "      got 2.0000\n")
   
   (defn some-true [] true)

   (deftest some-test
     (is (= 2 3))
     (is (= 2 2))
     (is (not (some-true)))
     (is (some-true)))

   (defn some-fn [] 2)

   (deftest some-other-test
     (is (= 5 (apply + (list 1 2 3))))
     (is (= 6 (apply + (list 1 2 3))))
     (is (= 3 (some-fn)))
     (is (= 2 (some-fn))))

   (run-all-tests)))
