
(configure-runtime! FERRET_PROGRAM_MAIN "ferret::program_no_exec()")

(defn helper-a [] 10)

(defmacro ten-fn [] `(~'fn [] 10))

(defmacro helper-b []
  (reduce (fn [a b] (+ a b)) (list 1 2 3))
  1)

(defn helper-c []
  (helper-b))

(defn update-aux []
  )

(def update-data
  (fn-throttler update-aux 1000 :second :blocking))

(defn some-d-list [] {:a :b :c :d})
