
(require '[modules.module-c :as mod-c])

(defn helper-b []
  (mod-c/helper-c))

(defn eleven-fn []
  (mod-c/eleven-fn))

(defnative native-single-argument [x]
  (on "defined FERRET_STD_LIB"
      ("utility") ;; dummy include
      "__result = obj<number>(number::to<int>(x));"))

(defnative macro-aux []
  (on "defined FERRET_STD_LIB"
      "__result = obj<number>((number_t)42);"))

(defmacro macro-call []
  `(~'do (~'macro-aux)))
