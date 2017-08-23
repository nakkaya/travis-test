
(defmacro -> [x & forms]
  (loop [x x, forms forms]
    (if forms
      (let [form (first forms)
            threaded (if (seq? form)
                       `(~(first form) ~x ~@(next form))
                       (list form x))]
        (recur threaded (next forms)))
      x)))
(defmacro ->> [x & forms]
  (loop [x x, forms forms]
    (if forms
      (let [form (first forms)
            threaded (if (seq? form)
                       `(~(first form) ~@(next form)  ~x)
                       (list form x))]
        (recur threaded (next forms)))
      x)))
(defmacro defn [name & body]
  `(~'def ~name (~'fn ~@body)))
(defmacro fn [& body]
  (if (vector? (first body))
    (fn->unique-args body)
    ;; handle multi arity function
    (let [fns   (map #(fn->unique-args %) body)
          arity (->> (map first body)
                     (map (fn* [args] (filter #(not (= % '&)) args)))
                     (map #(count %)))
          fns   (->> (interleave arity fns)
                     (partition 2)
                     (sort-by first))
          fns   (if (->> fns last second second      ;; last arity arguments
                         (take-last 2) first (= '&)) ;; check &
                  (let [switch        (drop-last 1 fns)
                        [[_ default]] (take-last 1 fns)]
                    `(~'fn-multi-arity ~switch ~default))
                  `(~'fn-multi-arity ~fns))]
      `(~'fn* () ~fns))))
(defmacro cxx [& body]
  (let [body (apply str body)]
    `((~'fn [] ~body))))
(defmacro defnative [name args & form]
  (let [includes (->> (filter #(seq? (nth % 2)) form)
                      (map #(cons (nth % 1) (apply list (nth % 2))))
                      (map (fn [form]
                             (let [[guard & headers] form]
                               (str "\n#if " guard " \n"
                                    (apply str (map #(str "#include \"" % "\"\n") headers))
                                    "#endif\n"))))
                      (map #(list 'native-declare %)))
        body (->> (map #(vector (second %) (last %)) form)
                  (map #(str "\n#if " (first %) " \n"
                             (second %)
                             "\n#endif\n"))
                  (apply str))
        pre-ample (->> (map #(vector (second %) (drop-last (drop 3 %))) form)
                       (remove #(empty? (second %)))
                       (map #(str "\n#if " (first %) " \n"
                                  (apply str (map (fn [line] (str line "\n")) (second %)))
                                  "\n#endif\n"))
                       (map #(list 'native-declare %)))]
    `(~'def ~name (~'fn ~args ~@includes ~@pre-ample  ~body))))
(defobject seekable_i "ferret/obj/seekable_i.h")
(defobject lambda_i "ferret/obj/lambda_i.h")
(defobject deref_i "ferret/obj/deref_i.h")
(defn deref [a]
  "__result = a.cast<deref_i>()->deref();")
(defobject boolean "ferret/obj/boolean_o.h")
(defobject pointer "ferret/obj/pointer_o.h")
(defobject value "ferret/obj/value_o.h")
(defobject number "ferret/obj/number_o.h")
(defobject empty_sequence "ferret/obj/empty_sequence_o.h")
(defobject sequence "ferret/obj/sequence_o.h")
(defobject lazy_sequence "ferret/obj/lazy_sequence_o.h")
(defn new-lazy-seq [f]
  "__result = obj<lazy_sequence>(f);")

(defmacro lazy-seq [& body]
  `(~'new-lazy-seq (~'fn [] ~@body)))
(defobject d-list "ferret/obj/d_list_o.h")

(defn new-d-list-aux []
  "__result = obj<d_list>();")

(defmacro new-d-list [& args]
  `(~'-> (~'new-d-list-aux)
         ~@(map (fn [v]
                  (let [[k v] v]
                    (list 'assoc k v)))
                (partition 2 args))))

(defn assoc [m k v]
  "__result = m.cast<d_list>()->assoc(k,v);")

(defn dissoc [m k]
  "__result = m.cast<d_list>()->dissoc(k);")

(defn get [m & args]
  "__result = m.cast<d_list>()->val_at(args);")

(defn vals [m]
  "__result = m.cast<d_list>()->vals();")

(defn keys [m]
  "__result = m.cast<d_list>()->keys();")
(defobject keyword "ferret/obj/keyword_o.h")
(defobject string "ferret/obj/string_o.h")
(defmacro new-string [& ss]
  (let [s (apply str ss)]
    `((~'fn [] ~(str "__result = obj<string>(\"" s "\");")))))
(defobject atomic "ferret/obj/atomic_o.h")
(defn atom [x]
  "__result = obj<atomic>(x)")

(defn swap! [a f & args]
  "__result = a.cast<atomic>()->swap(f,args);")

(defn reset! [a newval]
  (swap! a (fn [old curr] curr) newval))
(defobject async "ferret/obj/async_o.h")

(defmacro future [& body]
  `(~'_future_ (~'fn [] ~@body)))

(defn _future_ [f] "__result = obj<async>(f);")

(defn future-done? [f] "__result = obj<boolean>(f.cast<async>()->is_ready());")
(defobject delayed "ferret/obj/delayed_o.h")

(defn _delay_ [f]
  "__result = obj<delayed>(f)")

(defmacro delay [& body]
  `(~'_delay_ (~'fn [] ~@body)))

(defn delay? [d]
  "__result = obj<boolean>(d.is_type(runtime::type::delayed));")

(defn force [d] @d)
(defn first [x]
  "__result = runtime::first(x);")

(defn second [x]
  "__result = runtime::first(runtime::rest(x));")

(defn nil? [x] "__result = obj<boolean>(x.is_nil())")
(defn reduce
  ([f [sf & sr]]
   "var acc = run(f, runtime::first(sr), sf);
    var r = runtime::rest(sr);
    for (auto const& i : runtime::range(r))
     acc = run(f, acc, i);
    __result = acc;")
  ([f acc coll]
   "__result = acc;
    for (auto const& i : runtime::range(coll))
     __result = run(f, __result, i);"))
(defn list [& xs] "if (xs.is_nil())
                     __result = runtime::list();
                   else
                     __result = xs;")
(defn list? [x] "__result = obj<boolean>(x.is_type(runtime::type::sequence));")
(defn empty? [x]
  (if (nil? x)
    true
    (= (list ) x)))
(defn rest [x] "var r = runtime::rest(x);
                if (r.is_nil())
                  return runtime::list();
                else 
                  __result = r;")
(defn nth [coll index] "__result = runtime::nth(coll,number::to<number_t>(index));")
(defn nthrest [coll n]
  "__result = runtime::nthrest(coll,number::to<number_t>(n));")
(defn cons [x seq] "__result = runtime::cons(x, seq);")
(defn apply [f args] "__result = f.cast<lambda_i>()->invoke(args);")
(defn conj [coll & xs]
  (reduce (fn[h v] (cons v h)) (if (nil? coll) (list) coll) xs))
(defn reverse [s]
  (reduce (fn[h v] (cons v h)) (list) s))
(defn = [& args]
  "var curr = runtime::first(args);
   for(auto const& it : runtime::range(runtime::rest(args))){
    if (curr != it)
      return cached::false_t;
    curr = it;
   }
   __result = cached::true_t;")
(defmacro not= [& test]
  `(~'not (~'= ~@test)))
(defn <
  ([] true)
  ([x] true)
  ([a b]
   "__result = a.cast<number>()->is_smaller(b);")
  ([a b & more]
   (if (< a b)
     (apply < (cons b more))
     false)))
(defn >
  ([] true)
  ([x] true)
  ([a b]
   "__result = a.cast<number>()->is_bigger(b);")
  ([a b & more]
   (if (> a b)
     (apply > (cons b more))
     false)))
(defn >=
  ([] true)
  ([x] true)
  ([a b]
   "__result = a.cast<number>()->is_bigger_equal(b);")
  ([a b & more]
   (if (>= a b)
     (apply >= (cons b more))
     false)))
(defn <=
  ([] true)
  ([x] true)
  ([a b]
   "__result = a.cast<number>()->is_smaller_equal(b);")
  ([a b & more]
   (if (<= a b)
     (apply <= (cons b more))
     false)))
(defmacro and
  ([] true)
  ([x] x)
  ([x & next]
   `(~'if ~x (~'and ~@next) false)))
(defmacro or
  ([] nil)
  ([x] x)
  ([x & next]
   `(~'if ~x ~x (~'or ~@next))))
(defn not [x]
  "if (x)
     return cached::false_t;
   __result = cached::true_t;")
(defn true? [x]
  "if (x)
     return cached::true_t;
   __result = cached::false_t;")
(defn false? [x]
  "if (!x)
     return cached::true_t;
   __result = cached::false_t;")
(defmacro when [test & body]
  `(~'if ~test (~'do ~@body)))
(defmacro cond [& clauses]
  (when clauses
    `(~'if ~(first clauses)
      ~(if (next clauses)
         (second clauses)
         (throw (IllegalArgumentException.
                 "cond requires an even number of forms")))
      (~'cond ~@(next (next clauses))))))
(defn _while_ [pred fn]
  "while(run(pred))
     run(fn);")

(defmacro while [test & body]
  `(~'_while_ (~'fn [] ~test) (~'fn [] ~@body)))
(defmacro forever [& body]
  `(~'while true ~@body))
(defmacro if-let
  ([bindings then]
   `(~'if-let ~bindings ~then nil))
  ([bindings then else & oldform]
   (let [form (bindings 0) tst (bindings 1)]
     `(~'let* [temp# ~tst]
              (~'if temp#
                (~'let* [~form temp#]
                        ~then)
                ~else)))))
(defmacro when-let
  [bindings & body]
  (let [form (bindings 0) tst (bindings 1)]
    `(~'let* [temp# ~tst]
             (~'when temp#
               (~'let* [~form temp#]
                       ~@body)))))
(defn zero? [x]
  (= x 0))
(defn pos? [x]
  (> x 0))
(defn neg? [x]
  (< x 0))
(defn +
  ([] 0)
  ([x] x)
  ([h v]
   "__result = h.cast<number>()->add(v);")
  ([x y & more]
   (reduce + (+ x y) more)))
(defn -
  ([x]
   (* -1 x))
  ([h v]
   "__result = h.cast<number>()->sub(v);")
  ([x y & more]
   (reduce - (- x y) more)))
(defn *
  ([] 1)
  ([x] x)
  ([h v]
   "__result = h.cast<number>()->mul(v);")
  ([x y & more]
   (reduce * (* x y) more)))
(defn /
  ([x]
   (apply / (list 1 x)))
  ([h v]
   "__result = h.cast<number>()->div(v);")
  ([x y & more]
   (reduce / (/ x y) more)))
(defn inc [x]
  (+ x 1))
(defn dec [x]
  (- x 1))
(defn count [s]
  (if (or (nil? s)
          (empty? s))
    0
    (reduce inc 0 s)))
(defn min
  ([x] x)
  ([x & r]
   (reduce (fn[h v]
             (if (< h v)
               h v))
           x r)))

(defn max
  ([x] x)
  ([x & r]
   (reduce (fn[h v]
             (if (> h v)
               h v))
           x r)))
(defn rem [num div]
  "__result = obj<number>((number::to<number_t>(num) % number::to<number_t>(div)));")
(defn mod [num div] 
  (let [m (rem num div)] 
    (if (or (zero? m) (= (pos? num) (pos? div)))
      m 
      (+ m div))))
(defn floor [x] "__result = obj<number>(number::to<number_t>(x));")
(defn scale [x in-min in-max out-min out-max]
  (+ (/ (* (- x in-min) (- out-max out-min)) (- in-max in-min)) out-min))
(defn clamp [x min max]
  (cond
    (> x max) max
    (< x min) min
    true x))
(defn bit-and [x y] "__result = obj<number>((number::to<number_t>(x) & number::to<number_t>(y)));")
(defn bit-not [x] "__result = obj<number>(~number::to<number_t>(x));")
(defn bit-or [x y] "__result = obj<number>((number::to<number_t>(x) | number::to<number_t>(y) ));")
(defn bit-xor [x y] "__result = obj<number>((number::to<number_t>(x) ^ number::to<number_t>(y) ));")
(defn bit-shift-left [x n] "__result = obj<number>((number::to<number_t>(x) << number::to<number_t>(n) ));")
(defn bit-shift-right [x n] "__result = obj<number>((number::to<number_t>(x) >> number::to<number_t>(n) ));")
(defn number-split [n]
  "number_t val = number::to<number_t>(n);
   unsigned char *p = (unsigned char*)&val;
   __result = runtime::list();
   for(size_t i = 0; i < sizeof(number_t); i++)
     __result = runtime::cons(obj<number>((number_t)p[i]),__result);")
(defn number-combine [s]
  "number_t res = 0;
   for(size_t i = 0; i < sizeof(number_t); i++){
    size_t idx = (sizeof(number_t) - i - 1);
    var obj = runtime::nth(s,(number_t)idx);
    number_t val = number::to<number_t>(obj);
    res |= val << (i * 8);
   }
   __result = obj<number>(res);")
(defn sqrt [s]
  "__result = obj<number>((real_t)::sqrt(number::to<real_t>(s)));")
(defn pow [b e]
  "__result = obj<number>((real_t)::pow(number::to<real_t>(b), number::to<real_t>(e)));")
(defn cos [s]
  "__result = obj<number>((real_t)::cos(number::to<real_t>(s)));")
(defn sin [s]
  "__result = obj<number>((real_t)::sin(number::to<real_t>(s)));")
(defn asin [x]
  "__result = obj<number>((real_t)::asin(number::to<real_t>(x)));")
(defn atan2 [x y]
  "__result = obj<number>((real_t)::atan2(number::to<real_t>(x),number::to<real_t>(y)));")
(defn log [x]
  "__result = obj<number>((real_t)::log(number::to<real_t>(x)));")
(defn log10 [x]
  "__result = obj<number>((real_t)::log10(number::to<real_t>(x)));")
(defn to-degrees [x]
  "__result = obj<number>((real_t) (number::to<real_t>(x) * 180.0 / 1_pi) );")
(defn to-radians [x]
  "__result = obj<number>((real_t) (number::to<real_t>(x) * 1_pi / 180.0) );")
(defn thread [f]
  "__result = obj<async>(f);")
(defnative get-char []
  (on "defined FERRET_STD_LIB"
      "__result = obj<number>(getchar());"))
(defnative sleep [t]
  (on "defined FERRET_STD_LIB"
      "auto duration = ::std::chrono::milliseconds(number::to<number_t>(t));
       ::std::this_thread::sleep_for(duration);")
  (on "defined FERRET_HARDWARE_ARDUINO"
      "::delay(number::to<number_t>(t));"))
(defn println [& more]
  (when more
    (apply print more))
  (newline))
(defnative print [& more]
  (on "!defined(FERRET_DISABLE_STD_OUT)"
      "if (more.is_nil())
         return nil();
       var f = runtime::first(more);
       f.stream_console();
       var r = runtime::rest(more);
       for(auto const& it : runtime::range(r)){
        runtime::print(\" \");
        it.stream_console();
       }"))
(defnative newline []
  (on "!defined(FERRET_DISABLE_STD_OUT)"
      "runtime::print(\"\\n\");"))
(defnative sh [cmd]
  (on "defined FERRET_STD_LIB"
      ("memory")
      "::std::shared_ptr<FILE> pipe(popen(string::to<std::string>(cmd).c_str(), \"r\"), pclose);
       if (!pipe) 
          __result = nil();
       char buffer[128];
       ::std::string result = \"\";
       while (!feof(pipe.get()))
        if (fgets(buffer, 128, pipe.get()) != NULL)
         result += buffer;
       __result = obj<string>(result);"))
(defn system-exit [code]
  "::std::exit(number::to<number_t>(code));")
(defn system-abort [code]
  "::std::abort();")
(defn xor-stream-encoder [write]
  (fn [seq]
    (let [length (count seq)
          checksum (reduce bit-xor length seq)]
      (write 0X06)
      (write 0X85)
      (write length)
      (doseq [s seq] 
        (write s))
      (write checksum))))
(defn xor-stream-header-ready [read in-waiting]
  (and (>= (in-waiting) 3) (= (read) 0X06) (= (read) 0X85)))

(defn xor-stream-payload-ready [payload-size in-waiting]
  (>= (in-waiting) (inc (deref payload-size))))

(defn xor-stream-decoder-goto [] true)

(defn xor-stream-decoder [read in-waiting handler]
  (let [payload-size (atom nil)]
    (state-machine 
     (states
      (sync-header)
      (reset-payload    (reset! payload-size (read)))
      (wait-payload)
      (handle-payload   (let [payload (atom (list))]
                          (dotimes [_ (deref payload-size)]
                            (swap! payload conj (read)))
                          (when (= (read) (reduce bit-xor (deref payload-size) (deref payload)))
                            (swap! payload reverse)
                            (handler (deref payload))))))
     (transitions
      (sync-header     #(xor-stream-header-ready read in-waiting)           reset-payload)
      (reset-payload   xor-stream-decoder-goto                              wait-payload)
      (wait-payload    #(xor-stream-payload-ready payload-size in-waiting)  handle-payload)
      (handle-payload  xor-stream-decoder-goto                              sync-header)))))
(defmacro doseq [binding & body]
  `(~'_doseq_ ~(second binding)
              (~'fn [~(first binding)] ~@body)))

(defn _doseq_ [seq f] "for(auto const& it : runtime::range(seq)) run(f,it);")
(defmacro dotimes [binding & body]
  `(~'_dotimes_ ~(second binding)
                (~'fn [~(first binding)] ~@body)))

(defn _dotimes_ [t f] "for(number_t i = 0; i < number::to<number_t>(t); i++) run(f,obj<number>(i));")
(defn map [f col]
  (if (not (empty? col))
    (cons (f (first col))
          (lazy-seq (map f (rest col))))))
(defn range
  ([high]
   (range 0 high))
  ([low high]
   (if (< low high)
     (cons low (lazy-seq
                (range (inc low) high))))))
(defn take [n coll]
  (if (not (empty? coll))
    (if (> n 0)
      (cons (first coll)
            (lazy-seq (take (- n 1) (rest coll)))))))
(defn take-while [pred s]
  (if (and (not (empty? s))
           (pred (first s)))
    (cons (first s) (lazy-seq (take-while pred (rest s))))))
(defn drop [n coll]
  (if (and (pos? n)
           (not (empty? coll)))
    (drop (dec n) (rest coll))
    coll))
(defn concat
  ([]
   (list))
  ([x]
   (if (not (empty? x))
     (cons (first x) (lazy-seq (concat (rest x))))))
  ([x y]
   (if (not (empty? x))
     (cons (first x) (lazy-seq (concat (rest x) y)))
     (concat y))))
(defn filter [pred coll]
  (if (not (empty? coll))
    (let [[f & r] coll]
      (if (pred f)
        (cons f (filter pred r))
        (filter pred r)))
    coll))
(defn repeatedly
  ([f] (cons (f) (lazy-seq (repeatedly f))))
  ([n f] (take n (repeatedly f))))
(defn partition
  ([n coll]
   (partition n n coll))
  ([n step coll]
   (lazy-seq
    (if (not (empty? coll))
      (let [p (take n coll)]
        (when (= n (count p))
          (cons p (partition n step (nthrest coll step))))))))
  ([n step pad coll]
   (lazy-seq
    (if (not (empty? coll))
      (let [p (take n coll)]
        (if (= n (count p))
          (cons p (partition n step pad (nthrest coll step)))
          (list (take n (concat p pad)))))))))
(defnative millis []
  (on "defined FERRET_STD_LIB"
      "auto now = ::std::chrono::system_clock::now();
       auto epoch = now.time_since_epoch();
       auto time = ::std::chrono::duration_cast<::std::chrono::milliseconds>(epoch).count();
       __result = obj<number>(time);")
  (on "defined FERRET_HARDWARE_ARDUINO"
      "__result = obj<number>(::millis());"))
(defnative micros []
  (on "defined FERRET_STD_LIB"
      "auto now = ::std::chrono::high_resolution_clock::now();
       auto epoch = now.time_since_epoch();
       auto time = ::std::chrono::duration_cast<::std::chrono::microseconds>(epoch).count();
       __result = obj<number>(time);")
  (on "defined FERRET_HARDWARE_ARDUINO"
      "__result = obj<number>(::micros());"))
(defnative sleep-micros [t]
  (on "defined FERRET_STD_LIB"
      "auto duration = ::std::chrono::microseconds(number::to<number_t>(t));
       ::std::this_thread::sleep_for(duration);")
  (on "defined FERRET_HARDWARE_ARDUINO"
      "::delayMicroseconds(number::to<real_t>(t));"))
(defobject elapsed_micros "ferret/obj/elapsed_micros_o.h")

(defn new-elapsed-micros []
  "__result = obj<elapsed_micros>();")

(defn elapsed-micros? [t r]
  "__result = obj<boolean>(t.cast<elapsed_micros>()->is_elapsed(number::to<real_t>(r)));")

(defn elapsed-micros-now [t]
  "__result = obj<number>(t.cast<elapsed_micros>()->elapsed());")

(defn elapsed-micros-reset [t]
  "t.cast<elapsed_micros>()->reset()")
(defn time-fn [f]
  (let [start (millis)]
    (f)
    (- (millis) start)))
(defn benchmark [f n]
  (let [values (map (fn [_] (time-fn f)) (range n))]
    (floor (/ (apply + values) n))))
(defn fn-throttler-aux-blocking [timer f rate]
  (fn [& args]
    (let [wait (- rate (elapsed-micros-now timer))]
      (elapsed-micros-reset timer)
      (sleep-micros wait)
      (apply f args))))

(defn fn-throttler-aux-non-blocking [timer f rate]
  (fn [& args]
    (when (elapsed-micros? timer rate)
      (elapsed-micros-reset timer)
      (apply f args))))

(defmacro fn-throttler [f rate unit policy]
  (let [unit->ms {:microsecond 1 :millisecond 1000
                  :second 1000000 :minute 60000000
                  :hour 3600000000 :day 86400000000
                  :month 2678400000000}
        rate (/ (unit->ms unit) rate)]
    (if (= policy :blocking)
      `(~'fn-throttler-aux-blocking     (~'new-elapsed-micros) ~f ~rate)
      `(~'fn-throttler-aux-non-blocking (~'new-elapsed-micros) ~f ~rate))))
(defnative rand-aux []
  (on "defined FERRET_STD_LIB"
      ("random")
      "::std::random_device ferret_random_device;
       ::std::mt19937_64 ferret_random_generator(ferret_random_device());
       ::std::uniform_real_distribution<ferret::real_t> ferret_random_distribution(0.0,1.0);"
      "__result = obj<number>(ferret_random_distribution(ferret_random_generator));"))

(defn rand
  ([]
   (rand-aux))
  ([x]
   (* x (rand-aux))))
(defn rand-int
  [x]
  (floor (rand x)))
(defn identity [x] x)
(defmacro doto
  [x & forms]
  (let [gx (gensym)]
    `(let [~gx ~x]
       ~@(map (fn [f]
                (if (seq? f)
                  `(~(first f) ~gx ~@(next f))
                  `(~f ~gx)))
              forms)
       ~gx)))
(defmacro state-machine [[_ & states] [_ & transitions]]
  (let [states (reduce (fn [h v]
                         (let [[name & body] v]
                           (conj h name `(~'fn [] ~@body))))
                       [] states)
        transitions (->> transitions
                         (map (fn [v]
                                (let [[state & conds] v
                                      conds (->> (partition 2 conds)
                                                 (reduce (fn [h v]
                                                           (let [[check state] v]
                                                             (conj h `(~check) state))) []))]
                                  `((~'= ~'state ~state) (~'cond ~@conds true ~state)))))
                         (reduce (fn [h v]
                                   (let [[check transition] v]
                                     (conj h check transition)))
                                 ['cond]))]
    `(let [~@states
           machine-state# (~'atom ~(first states))]
       (~'fn []
        (~'let [ret# ((~'deref machine-state#))]
         (~'swap! machine-state# (~'fn [~'state] (~@transitions)))
         ret#)))))
(defobject pid_controller "ferret/obj/pid_controller_o.h")
(defn pid-controller-create [kp ki kd in-min in-max out-min out-max continuous]
  "__result = obj<pid_controller<real_t>>(kp, ki, kd, 
                                         in_min, in_max, out_min, out_max, 
                                         continuous);")

(defn pid-controller-set-point [controller sp]
  "controller.cast<pid_controller<real_t>>()->set_setpoint(sp);")

(defn pid-controller-update [controller input]
  "__result = controller.cast<pid_controller<real_t>>()->update(input)")

(defmacro pid-controller [& options]
  (let [defaults {:kp 0 :ki 0 :kd 0 :set-point 0 :bounds [-1 1 -1 1] :continuous false}
        options (merge defaults (apply hash-map options))
        {:keys [container kp ki kd set-point bounds continuous]} options
        [in-min in-max out-min out-max] bounds]

    (if (or (< in-max in-min)
            (< out-max out-min))
      (do (println "pid-controller invalid bounds")
          (System/exit 1)))
    
    (if (symbol? set-point)
      `(~'let [pid# (~'pid-controller-create
                     ~kp ~ki ~kd ~in-min ~in-max ~out-min ~out-max ~continuous)]
        (~'pid-controller-set-point pid# (~set-point))
        (~'fn [input#]
         (~'pid-controller-set-point pid# (~set-point))
         (~'pid-controller-update pid# input#)))
      `(~'let [pid# (~'pid-controller-create
                     ~kp ~ki ~kd ~in-min ~in-max ~out-min ~out-max ~continuous)]
        (~'pid-controller-set-point pid# ~set-point)
        (~'fn [input#]
         (~'pid-controller-update pid# input#))))))
(defn moving-average-filter [alpha average data]
  (+ (* alpha data)
     (* (- 1.0 alpha) average)))
(defmacro pin-mode [pin mode]
  (let [pin (if (number? pin)
              pin
              (str "number::to<number_t>(" (symbol-conversion pin) ")"))
        mode (-> mode name .toUpperCase)]
    `(~'cxx ~(str "::pinMode(" pin ", " mode ");"))))
(defnative digital-write [pin val]
  (on "defined FERRET_HARDWARE_ARDUINO"
      "::digitalWrite(number::to<number_t>(pin), number::to<number_t>(val));"))
(defnative digital-read [pin]
  (on "defined FERRET_HARDWARE_ARDUINO"
      "__result = obj<number>(::digitalRead(number::to<number_t>(pin)));"))
(defnative analog-write [pin val]
  (on "defined FERRET_HARDWARE_ARDUINO"
      "::analogWrite(number::to<number_t>(pin),number::to<number_t>(val));"))
(defnative analog-read [pin]
  (on "defined FERRET_HARDWARE_ARDUINO"
      "__result = obj<number>((number_t)::analogRead(number::to<number_t>(pin)));"))
(defnative analog-write-resolution [bit]
  (on "defined FERRET_HARDWARE_ARDUINO"
      "::analogWriteResolution(number::to<number_t>(bit));"))
(defnative analog-read-resolution [bit]
  (on "defined FERRET_HARDWARE_ARDUINO"
      "::analogReadResolution(number::to<number_t>(bit));"))
(defnative tone [pin freq]
  (on "defined FERRET_HARDWARE_ARDUINO"
      "::tone(number::to<number_t>(pin), number::to<number_t>(freq));"))
(defnative no-tone [pin]
  (on "defined FERRET_HARDWARE_ARDUINO"
      "::noTone(number::to<number_t>(pin));"))
(defmacro attach-interrupt [callback pin mode]
  (let [pin (if (number? pin)
              pin
              (str "number::to<number_t>(" (symbol-conversion pin) ")"))
        mode (-> mode name .toUpperCase)
        cb-sym (gensym)]
    `(~'do
      (~'def ~cb-sym ~callback)
      (~'cxx
       ~(str "::pinMode(" pin " , INPUT_PULLUP);\n"
             "auto int_pin = digitalPinToInterrupt(" pin ");\n"
             "::attachInterrupt(int_pin, [](){ run(" cb-sym ");}, " mode ");")))))
(defmacro no-interrupt [& body]
  `(~'no-interrupt-aux  (~'fn [] ~@body)))

(defn no-interrupt-aux [f]
  "noInterrupts();
   __result = run(f);
   interrupts();")
(defmacro detach-interrupt [pin]
  (let [pin (if (number? pin)
              pin
              (str "number::to<number_t>(" (symbol-conversion pin) ")"))]
    `(~'cxx
      ~(str "::detachInterrupt(digitalPinToInterrupt(" pin "));"))))
(defnative spi-begin []
  (on "defined FERRET_HARDWARE_ARDUINO"
      ("SPI.h")
      "SPI.begin();"))

(defn spi-end []
  "SPI.end();")
(defmacro spi-settings [max-speed data-order data-mode]
  (let [speed      (* max-speed 1000000)
        data-order (if (= data-order :msb-first)
                     "MSBFIRST"
                     "LSBFIRST")
        data-mode  (condp = data-mode
                     :mode-0 "SPI_MODE0"
                     :mode-1 "SPI_MODE1"
                     :mode-2 "SPI_MODE2"
                     :mode-3 "SPI_MODE3")]
    `(~'cxx
      ~(str "__result = obj<value<SPISettings>>(" speed "," data-order "," data-mode ");"))))
(defn with-spi-aux [conf f]
  "SPI.beginTransaction(value<SPISettings>::to_reference(conf));
   __result = run(f);
   SPI.endTransaction();")

(defmacro with-spi [conf & body]
  `(~'with-spi-aux ~conf (~'fn [] ~@body)))

(defn spi-write [val]
  "__result = obj<number>(SPI.transfer(number::to<int>(val)));")
(defn assert-aux [f msg]
  (when (not (f))
    (println "Assertion Failed =>" msg)
    (system-abort)))

(defn assert-aux-callback [f callback]
  (when (not (f)) (callback)))

(defmacro assert
  ([exp]
   `(~'assert-aux (~'fn [] ~exp) ~(-> exp pr-str (clojure.string/escape {\\ "\\\\"}))))
  ([exp callback]
   `(~'assert-aux-callback (~'fn [] ~exp) (~'fn [] ~callback))))
(defn is-aux-expect [ex-fb form-fn form-str]
  (let [expect (ex-fb)
        got  (form-fn)]
    (when (not=  expect got)
      (println "fail in" form-str "\n expected" expect "\n      got" got))))

(defn is-aux [f msg]
  (when (not (f))
    (println "fail" msg)))

(defmacro is [form]
  (let [check-op (first form)
        form-str (-> form pr-str (clojure.string/escape {\\ "\\\\"}))]

    (cond (= check-op '=)
          (let [[_ expected form] form]
            `(~'is-aux-expect (~'fn [] ~expected) (~'fn [] ~form) ~form-str))
          
          :default `(~'is-aux (~'fn [] ~form) ~form-str))))

(defmacro deftest [name & exprs]
  (defonce fir-unit-tests (atom []))
  (swap! fir-unit-tests conj name)
  `(def ~name (~'fn [] ~@exprs)))

(defmacro run-all-tests []
  (if (bound? #'fir-unit-tests)
    `(~'do ~@(map #(list %) @fir-unit-tests))
    `(~'do )))
(defn pr-object-sizes []
  (println "Object Sizes")
  (println "\tvar:\t\t\t" (cxx "__result = obj<number>(sizeof(var));"))
  (println "\tobject:\t\t\t" (cxx "__result = obj<number>(sizeof(object_t));"))
  (println "\tpointer:\t\t" (cxx "__result = obj<number>(sizeof(pointer));"))
  (println "\tnumber:\t\t\t" (cxx "__result = obj<number>(sizeof(number));"))
  (println "\tkeyword:\t\t" (cxx "__result = obj<number>(sizeof(keyword));"))
  (println "\tempty_sequence:\t\t" (cxx "__result = obj<number>(sizeof(empty_sequence));"))
  (println "\tsequence:\t\t" (cxx "__result = obj<number>(sizeof(sequence));"))
  (println "\tlazy_sequence:\t\t" (cxx "__result = obj<number>(sizeof(lazy_sequence));"))
  (println "\tstring:\t\t\t" (cxx "__result = obj<number>(sizeof(string));"))
  (println "\tboolean:\t\t" (cxx "__result = obj<number>(sizeof(boolean));"))
  (println "\tlambda_i:\t\t" (cxx "__result = obj<number>(sizeof(lambda_i));"))
  (println "\tatom:\t\t\t" (cxx "__result = obj<number>(sizeof(atomic));"))
  (println "\telapsed_micros:\t\t" (cxx "__result = obj<number>(sizeof(elapsed_micros));"))
  (println "\tpid_controller<real_t>:\t"
           (cxx "__result = obj<number>(sizeof(pid_controller<real_t>));")))
(defnative memory-pool-free-space []
  (on "defined FERRET_MEMORY_POOL_SIZE"
      "size_t acc = 0;
       for(size_t i = 0; i < FERRET_MEMORY_POOL_PAGE_COUNT; i++)
         if(mem::allocator::program_memory.used.get(i) == false)
           acc++;
       __result = obj<number>((acc*sizeof(FERRET_MEMORY_POOL_PAGE_TYPE)));"))
(defnative lock-memory []
  (on "defined FERRET_STD_LIB"
      ("sys/mman.h")
      "mlockall(MCL_CURRENT | MCL_FUTURE);"))
(defmacro configure-runtime! [& body]
  `(~'native-define ~(->> (partition 2 body)
                          (map #(str "#define " (first %) " " (second %) "\n"))
                          (list))))
(defmacro configure-ferret! [& body]
  `(~'native-define ~(str "// build-conf-begin\n"
                          "//" (str (apply hash-map body)) "\n"
                          "// build-conf-end\n")))
