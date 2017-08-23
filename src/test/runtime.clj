
(native-declare "class data{ 
                   int x;
                 public: 
                    
                   explicit data(int _x) : x(_x) {} 

                   int  content() { return x; }
                   void inc() { x++; }
                 };")

(defn make-data [x]
  "__result = obj<value<data>>(number::to<int>(x))")

(defn get-data [x]
  "__result = obj<number>((number_t) value<data>::to_value(x).content());")

(defn inc-data [x]
  "data & d = value<data>::to_reference(x);
   d.inc();")
(defn my-sort [f seq]
  "std_vector vec = sequence::to<std_vector>(seq);
   std::sort(vec.begin(), vec.end(), [f](var a, var b) { return run(f,a,b); });
   __result = sequence::from<std_vector>(vec);")
(defn my-find [item seq]
  "std_vector vec = sequence::to<std_vector>(seq);
   std_vector::iterator it = find (vec.begin(), vec.end(), item);

   if(it != vec.end())
     __result = cached::true_t;")
(def make-adder
  (fn [n]
    (fn [x] (+ x n))))

(def adder
  (make-adder 1))

(def fibo
  (fn [n]
    (if (< n 2)
      1
      (+ (fibo (- n 1))
         (fibo (- n 2))))))

(native-declare "ferret::number_t i = 0;")

(defn inc-int []
  "__result =  obj<number>(i++);")

(def nested-multi-call (do (fn
                             ([]    0)
                             ([x]   1)
                             ([x y] 2))))

(deftest special-forms-test
  (is (= 1                @(atom 1)))
  (is (= 3                (#(+ 1 2))))
  (is (= 11               ((fn [n] (+ n 1)) 10)))
  (is (= 3               (((fn [n] (fn [n] n)) 3) 3)))
  (let [args (list "1" "2")]
    (is (= args (rest *command-line-args*))))

  (let [a 1]
    (is (= 1 a)))

  (let [a 1
        a 3]
    (is (= 3 a)))
  
  (let [a 1
        b 2]
    (is (= 3 (+ a b))))

  (let [a 1
        b 2
        c 3]
    (is (= 6 (+ a b c))))

  (let [a 1
        b 2]
    (let []
      (is (= 3 (+ a b)))))

  (is (= 0 (nested-multi-call)))
  (is (= 1 (nested-multi-call 1)))
  (is (= 2 (nested-multi-call 1 2)))
  
  (is (= 10 (adder 9)))
  (is (= 89 (fibo 10)))
  (is (= 0 (inc-int)))
  (is (= 1 (inc-int))))
(defn destructure-test-1 [[a b c]]
  (list a b c))

(defn destructure-test-2 [[a [b] c]]
  b)

(defn destructure-test-3 [[a [_ b] c]]
  b)

(defn destructure-test-4 [& a]
  a)

(defn destructure-test-5 []
  (let [[a b c] (list 1 2 3)]
    (list a b c)))

(defn destructure-test-6 []
  (let [[_ _ a] (list 1 2 3)]
    a))

(defn destructure-test-7 [a b & [c d]]
  (list c d))

(deftest destructuring-test
  (is (= 3                   (count (destructure-test-1 (list 1 2 3)))))
  (is (= 2                   (destructure-test-2 (list 1 (list 2) 3))))
  (is (= 3                   (destructure-test-3 (list 1 (list 2 3) 3))))
  (is (= (list (list 1 2 3)) (destructure-test-4 (list 1 2 3))))

  (let [a (list 1 2 3 4)
        [b c & r] a]

    (is (= 1          b))
    (is (= 2          c))
    (is (= (list 3 4) r)))

  (let [a 1 b 2
        [c & r] (list 4 5)]
    
    (is (= 1 a))
    (is (= 2 b))
    (is (= 4 c))
    (is (= (list 5) r)))

  (let [[a & r] (list 1 2 3)
        rr (rest r)]
    (is (= (list 3) rr)))
  
  (is (= (list 1 2 3) (destructure-test-5)))
  (is (= 3            (destructure-test-6)))
  (is (= (list 3 4)   (destructure-test-7 1 2 3 4)))

  (let [[a & b :as all-list]   (list 1 2 3)
        [c     :as other-list] all-list]
    (is (= 1            a))
    (is (= (list 2 3)   b))
    (is (= (list 1 2 3) all-list))
    (is (= 1            c))
    (is (= (list 1 2 3) other-list)))
  
  (let [[_ _ a] (list 1 2 3)
        [_ b] (list 4 5 6)]
    (is (= 3 a))
    (is (= 5 b)))

  (let [a (list 1 2 3)
        [b c d e f g] a]
    (is (= 1   b))
    (is (= 2   c))
    (is (= 3   d))
    (is (= nil e))
    (is (= nil f))
    (is (= nil g))))
(deftest conditionals-test
  (is (= 2   (if 1 2)))
  (is (= 1   (if (zero? 0) 1 -1)))
  (is (= -1  (if (zero? 1) 1 -1)))
  (is (= 2   (when true 2)))
  (is (= 2   (if nil 1 2)))
  (is (= nil (if-let [a nil] a)))
  (is (= 5   (if-let [a 5] a)))
  (is (= 2   (if-let [[_ a] (list 1 2)] a)))
  (is (= nil (when-let [a nil] a)))
  (is (= 5   (when-let [a 5] a)))
  (is (= 2   (when-let [[_ a] (list 1 2)] a)))

  (is (= 1     (when (< 2 3) 1)))
  (is (= true  (let [a 1] (and (> a 0) (< a 10)))))
  (is (= false (let [a 11] (and (> a 0) (< a 10)))))
  (is (= true  (and true  (identity true))))
  (is (= false (and true  (identity false))))
  (is (= true  (or  true  (identity false))))
  (is (= false (or  false (identity false)))))
(defn pos-neg-or-zero [n]
  (cond
    (< n 0) -1
    (> n 0)  1
    :else    0))

(deftest logical-operators-test
  (is (= true  (< 2)))
  (is (= true  (< 2 3 4 5)))
  (is (= true  (> 2)))
  (is (= false (> 2 3 4 5)))
  (is (= true  (> 6 5 4 3)))
  (is (= true  (>= 2)))
  (is (= true  (>= 5 4 3 2 2 2)))
  (is (= false (>= 5 1 3 2 2 2)))
  (is (= true  (<= 2)))
  (is (= true  (<= 2 2 3 4 5)))
  (is (= false (<= 2 2 1 3 4)))
  (is (= true  (= 2)))
  (is (= false (= 2 3)))
  (is (= true  (= 2 2 2 2)))
  (is (= true  (= 2 2.0 2)))
  (is (= false (= 2 2 2 2 3 5)))
  (is (= true  (= (list 1 2) (list 1 2))))
  (is (= false (= (list 1 2) (list 1 3))))
  (is (= true  (= true true)))
  (is (= false (not (= true true))))
  (is (= false (not 1)))

  (let [a (fn [x] (+ 1 x))
        b (fn [x] (inc x))]
    (is (= true  (= a a)))
    (is (= false (= a b)))
    (is (= true  (= nil ((fn [] )))))
    (is (= true  (= nil ((fn [x y] ) 1 2)))))

  (is (= -1  (pos-neg-or-zero -5)))
  (is (=  1  (pos-neg-or-zero  5)))
  (is (=  0  (pos-neg-or-zero  0)))

  (is (= true  (true? true)))
  (is (= false (true? false)))
  (is (= false (false? true)))
  (is (= true  (false? false)))
  (is (= false (= nil 1)))
  (is (= false (= 1 nil)))
  (is (= true  (= nil nil)))

  (is (= true  (pos? 1)))
  (is (= true  (pos? 0.2)))
  (is (= false (pos? 0)))
  (is (= false (neg? 1)))
  (is (= true  (neg? -1)))
  (is (= true  (zero? 0)))
  (is (= false (zero? 10)))
  (is (= true  (zero? (- 1 1))))
  (is (= true  (zero? (- 1.2 1.2))))
  (is (= true  (zero? (+ 1.2 -1.2)))))
(deftest math-test
  (is (= 0.6 (+ 0.3 0.3)))
  (is (= 0   (+ )))
  (is (= 1   (+ 1)))
  (is (= 10  (+ 1 2 3 4)))
  (is (= 10  (+ 1 2.0 3 4)))
  (is (= -1  (- 1)))
  (is (= 0   (- 4 2 2)))
  (is (= 0   (- 4 2 2.0)))
  (is (= 1   (* )))
  (is (= 8   (* 2 2 2)))
  (is (= 8   (* 2.0 2 2)))
  (is (= 1   (/ 1)))
  (is (= 0.5 (/ 2)))
  (is (= 1   (/ 4 2 2)))
  (is (= 1   (/ 4 2 2.0)))
  
  (is (= 1         (floor 1.1)))
  (is (= 1         (floor 1.5)))
  (is (= 1         (floor 1.9)))
  (is (= 0         (mod 2 2)))
  (is (= 0         (mod 4 2)))
  (is (= 1         (mod 5 2)))
  (is (= 1         (mod 8 7)))
  (is (= 1         (min 1)))
  (is (= 1         (min 2 1)))
  (is (= 1         (min 3 5 7 1)))
  (is (= 1         (max 1)))
  (is (= 2         (max 2 1)))
  (is (= 7         (max 3 5 7 1)))
  
  (is (= 100 (scale 10 0 10 0 100)))
  (is (= 50  (scale 5 0 10 0 100)))
  (is (= 0   (scale 0 0 10 0 100)))
  (is (= 5   (clamp 10 0 5)))
  (is (= 10  (clamp 10 0 20)))
  (is (= 0   (clamp 10 -10 0)))
  (is (= -10 (clamp -100 -10 0)))

  (is (= 0     (number-combine (number-split 0))))
  (is (= 512   (number-combine (number-split 512))))
  (is (= 1024  (number-combine (number-split 1024))))
  (is (= 2048  (number-combine (number-split 2048))))
  (is (= 32000 (number-combine (number-split 32000))))

  (is (= true (not (nil? (rand)))))
  (is (= true (not (nil? (rand 15)))))

  (is (= -5 (bit-not  4)))
  (is (= -1 (bit-not  0)))
  (is (= 7  (bit-or   4 3)))
  (is (= 1  (bit-or   0 1)))
  (is (= 0  (bit-and  4 3)))
  (is (= 0  (bit-and  0 1)))
  (is (= 0  (bit-xor  4 4)))
  (is (= 1  (bit-xor  1 0)))
  (is (= 8  (bit-shift-left 4 1)))
  (is (= 16 (bit-shift-left 4 2)))
  (is (= 2  (bit-shift-right 4 1)))
  (is (= 1  (bit-shift-right 4 2)))

  (is (= 32         (sqrt 1024)))
  (is (= 2          (sqrt 4)))
  (is (= 8          (pow 2 3)))
  (is (= 16         (pow 2 4)))
  (is (= 1          (cos 0)))
  (is (= -0.99999   (cos 3.145)))
  (is (= 0          (sin 0)))
  (is (= -0.00340   (sin 3.145)))
  (is (= 0.98279    (atan2 45 30)))
  (is (= 180.19522  (to-degrees 3.145)))
  (is (= 3.14159    (to-radians 180)))

  (is (= 2.30258    (log 10)))
  (is (= 2          (log10 100)))

  (let [a 1
        b 2]
    (+ 1 a)
    (+ b a)
    (is (= 1 a))
    (is (= 2 b))
    (* 2 a)
    (* b a)
    (is (= 1 a))
    (is (= 2 b))
    (/ 2 a)
    (/ b a)
    (is (= 1 a))
    (is (= 2 b))
    (- 2 a)
    (- b a)
    (is (= 1 a))
    (is (= 2 b))))
(deftest pid-controller-test
  (let [controller (pid-controller :kp 1
                                   :ki 0
                                   :kd 0
                                   :set-point 5
                                   :bounds [0 10 0 10]
                                   :continuous false)]

    (is (= 5 (controller 0)))
    (is (= 5 (controller 0))))

  (let [controller (pid-controller :kp 1
                                   :ki 1
                                   :kd 0
                                   :set-point 5
                                   :bounds [0 10 0 20]
                                   :continuous false)]

    (is (= 10 (controller 0)))
    (is (= 15 (controller 0)))
    (is (= 20 (controller 0)))
    (is (= 20 (controller 0))))

  (let [controller (pid-controller :kp 1
                                   :ki 0
                                   :kd 1
                                   :set-point 5
                                   :bounds [0 10 0 20]
                                   :continuous false)]

    (is (= 10 (controller 0)))
    (is (= 5 (controller 0))))

  (let [sp-fn (fn [] 5)
        controller (pid-controller :kp 1
                                   :ki 0
                                   :kd 1
                                   :set-point sp-fn
                                   :bounds [0 10 0 20]
                                   :continuous false)]

    (is (= 10 (controller 0)))
    (is (= 5 (controller 0)))))

(deftest state-machine-test
  (let [state (atom 0)
        machine (state-machine
                 (states
                  (off (swap! state inc) :off)
                  (on  (swap! state inc) :on))
                 (transitions
                  (off (fn [] true) on)
                  (on  (fn [] true) off)))]
    
    (is (= :off (machine)))
    (is (= :on  (machine)))
    
    (dotimes [_ 8]
      (machine))
    
    (is (= 10 (deref state))))

  (let [state (atom 0)
        machine (state-machine
                 (states
                  (a (swap! state inc))
                  (b (swap! state inc))
                  (c (swap! state inc))
                  (no-op (swap! state inc)))
                 (transitions
                  (a
                   (fn [] false) no-op
                   (fn [] true)  b)
                  (b
                   (fn [] true)  c)
                  (c
                   (fn [] false) no-op
                   (fn [] false) no-op
                   (fn [] true)  a
                   (fn [] false) no-op)))]
    (dotimes [_ 10]
      (machine))
    (is (= 10 (deref state))))

  (let [state (atom nil)
        machine (state-machine
                 (states
                  (a (swap! state conj 1))
                  (b (swap! state conj 2))
                  (c (swap! state conj 3))
                  (no-op ))
                 (transitions
                  (a
                   (fn [] true) b
                   (fn [] true) c
                   (fn [] true) no-op)
                  (b (fn [] true) no-op)
                  (c (fn [] true) no-op)
                  (no-op (fn [] true) no-op)))]
    (dotimes [_ 50]
      (machine))
    (is (= (list 2 1) (deref state))))

  (let [value (atom 0)
        machine (state-machine 
                 (states
                  (increment (swap! value inc))
                  (no-op ))
                 (transitions
                  (increment
                   (fn [] true) increment
                   (fn [] true) no-op)
                  (no-op
                   (fn [] true) no-op)))]
    (machine)
    (machine)
    (is (= 2 (deref value)))))
(deftest timing-test
  (let [now (millis)]
    (sleep 150)
    (is (>= (- (millis) now) 100)))
  (is (>= (time-fn (fn [] (sleep 150) (+ 1 1))) 100))
  (is (>= (benchmark (fn [] (sleep 20) (+ 1 1)) 10) 10)))

(defn ping [] true)

(deftest fn-throttler-test
  (let [throttled-ping (fn-throttler ping 1 :second :blocking)
        begin (millis)]
    (throttled-ping)
    (throttled-ping)
    (throttled-ping)
    (is (> (- (millis) begin) 2000))
    (is (throttled-ping)))


  (let [throttled-ping (fn-throttler ping 1 :second :non-blocking)
        begin (millis)]
    (throttled-ping)
    (throttled-ping)
    (throttled-ping)
    (is (nil? (throttled-ping)))
    (is (< (- (millis) begin) 1000))))
(def xor-stream-state (atom nil))

(defn xor-sample-read-stream [buf]
  (let [buffer (atom buf)]
    (list
     (fn []
       (let [f (first (deref buffer))]
         (swap! buffer rest)
         f))
     (fn []
       (count (deref buffer)))
     (fn [vals]
       (reset! xor-stream-state vals)))))

(defn xor-sample-write-stream []
  (let [buffer (atom (list))]
    (list
     (fn [v]
       (swap! buffer conj v))
     (fn []
       (reverse (deref buffer))))))

(deftest xor-stream-test
  (let [[writer get-buffer] (xor-sample-write-stream)
        encoder (xor-stream-encoder writer)
        data (list (list 1 2 3 4)
                   (list 5 6 7 8))]
    (doseq [d data] 
      (encoder d))

    (let [[read in-waiting handler] (xor-sample-read-stream (get-buffer))
          decoder (xor-stream-decoder read in-waiting handler)]
      (dotimes [i 4]
        (decoder))
      (is (= (list 1 2 3 4) @xor-stream-state))
      (dotimes [i 4]
        (decoder))
      (is (= (list 5 6 7 8) @xor-stream-state)))))
(deftest doto-test
  (let [st (atom )
        add (fn [s v]
              (swap! s conj v))]
    (doto st
      (add 1)
      (add 2)
      (add 3))
    (is (= (list 3 2 1) @st))))
(deftest ffi-test
  (is (= true  ((fn [a b] "__result = obj<boolean>((a == b))") (list 1 2) (list 1 2))))
  (is (= false ((fn [a b] "__result = obj<boolean>((a != b))") (list 1 2) (list 1 2))))
  (is (= true  ((fn [a b] "__result = obj<boolean>((a != b))") (list 1 2) 1)))
  (is (= false ((fn [a b] "__result = obj<boolean>((a == b))") 1          (list 1 2))))



  (is (=  nil   (my-find (list 5 5) (list (list 1 2)
                                          (list 2 3)
                                          (list 4 5)))))
  (is (=  true  (my-find (list 1 2) (list (list 1 2)
                                          (list 2 3)
                                          (list 4 5)))))
  (is (=  true  (my-find (list 4 5) (list (list 1 2)
                                          (list 2 3)
                                          (list 4 5)))))
  
  (is (= (list 1 2 3) (my-sort > (list 1 3 2))))
  (is (= (list 3 2 1) (my-sort < (list 1 3 2)))))
(deftest number-test
  (is (= 0.5       1/2))
  (is (= 0.33333   1/3))
  (is (= 3501      0xDAD))
  (is (= 2748      0xABC)))


(deftest fixed-real-test
  (is (= 25          (cxx "char n = 25;
                           auto x = ferret::fixed_real<32,8>(n);
                           n = (char)x;
                           __result = obj<number>((number_t)n)")))

  (is (= 25          (cxx "long n = 25;
                           auto x = ferret::fixed_real<64,8>(n);
                           char v = (char)x;
                           __result = obj<number>((number_t)v)")))

  (is (= 2500        (cxx "unsigned long n = 2500;
                           auto x = ferret::fixed_real<64,8>(n);
                           unsigned long v = (unsigned long)x;
                           __result = obj<number>((number_t)v)")))

  (is (= 1024        (cxx "int n = 1024;
                           auto x = ferret::fixed_real<32,8>(n);
                           n = (int)x;
                           __result = obj<number>((number_t)n)")))

  (is (= 10.25       (cxx "auto x = ferret::fixed_real<32,8>();
                           (void)x;
                           auto y = ferret::fixed_real<32,8>();
                           (void)y;
                           x = 10; y = 0.250;
                           __result = obj<number>((real_t)(x + y))")))

  (is (= true       (cxx "long n = std::numeric_limits<int>::max() + 1024L;
                           auto x = ferret::fixed_real<64,8>(n);
                           n = (long)x;
                           __result = obj<boolean>((n == ((long)std::numeric_limits<int>::max() + 1024L)))")))

  (is (= 6.25       (cxx "auto x = ferret::fixed_real<32,8>(0);
                          for(int i = 0; i < 100; i++)
                            x += ferret::fixed_real<32,8>(0.0625);
                          __result = obj<number>((double)x)")))
  
  (is (= 35.25       (cxx "auto x = ferret::fixed_real<32,8>(22.75);
                           auto y = ferret::fixed_real<32,8>(12.5);
                           __result = obj<number>((double)(x + y))")))

  (is (= (- 0.25)    (cxx "auto x = ferret::fixed_real<32,8>(22.75);
                           auto y = ferret::fixed_real<32,8>(22.5);
                           __result = obj<number>((double)(y - x))")))

  (is (= (- 0.0625)  (cxx "auto x = ferret::fixed_real<32,8>(-0.25);
                           auto y = ferret::fixed_real<32,8>(4);
                           __result = obj<number>((double)(x / y))")))

  (is (= 9.9375      (cxx "auto x = ferret::fixed_real<32,8>(-0.0625);
                           auto y = ferret::fixed_real<32,8>(-10);
                           __result = obj<number>((double)(x - y))")))

  (is (= (- 29.8125) (cxx "auto x = ferret::fixed_real<32,8>(9.9375);
                           auto y = ferret::fixed_real<32,8>(-3);
                           __result = obj<number>((double)(x * y))")))

  (is (= (- 30)      (cxx "auto x = ferret::fixed_real<32,8>(-29.8125);
                           auto y = ferret::fixed_real<32,8>(0.1875);
                           __result = obj<number>((double)(x - y))"))))
(deftest sequence-test
  (is (= true  (= (list ) (list ))))
  (is (= 0     (count (list ))))
  (is (nil?    (first (rest (rest (list))))))
  (is (= false (= (list )       (list 1 2 3))))
  (is (= false (= (list )       (list nil))))
  (is (= false (= (list 1 2 3)  (list 1 2))))
  (is (= false (= (list 1 2)    (list 1 2 3))))
  (is (= true  (= (list 1 2 3)  (list 1 2 3))))
  (is (= false (= (list 1 2 3)  (list 1 2 4))))
  (is (= false (= (list 1 1 3)  (list 1 2 3))))

  (is (= (list )            (rest (list ))))
  (is (= (list 1)           (cons 1 nil)))
  (is (= (list nil)         (cons nil nil)))
  (is (= 1                  (first (list 1 2 3 4))))
  (is (= 2                  (second (list 1 2 3 4))))
  (is (= (list 2 3 4)       (rest (list 1 2 3 4))))
  (is (= (list 3 4)         (rest (rest (list 1 2 3 4)))))
  (is (= (list 3 3 4)       (cons 3 (rest (rest (list 1 2 3 4))))))
  (is (= 3                  (first (cons 3 (rest (rest (list 1 2 3 4)))))))
  (is (= 4                  (count (list 1 2 3 4))))
  (is (= (list 4 3 2 1 1 2) (conj (list 1 2) 1 2 3 4)))
  (is (= (list 4 3 2 1)     (conj nil 1 2 3 4)))
  (is (= 21                 (reduce + (list 1 2 3 4 5 6))))
  (is (= 21                 (apply + (list 1 2 3 4 5 6))))

  (is (= 1   (nth (list 1 2 3) 0)))
  (is (= 2   (nth (list 1 2 3) 1)))
  (is (= 3   (nth (list 1 2 3) 2)))
  (is (= nil (nth (list 1 2 3) 10)))
  (is (= nil (nth (list 1 2 3) -10)))

  (is (= (list 0 1 2 3 4 5 6 7 8 9)  (nthrest (range 10) 0)))
  (is (= (list )                     (nthrest (range 10) 20)))
  (is (= (list 5 6 7 8 9)            (nthrest (range 10) 5)))
  
  (is (= (list 1 2 3 4) (drop 0 (list 1 2 3 4))))
  (is (= (list 2 3 4)   (drop 1 (list 1 2 3 4))))
  (is (= (list 3 4)     (drop 2 (list 1 2 3 4))))
  (is (= (list )        (drop 4 (list 1 2 3 4))))
  (is (= (list )        (drop 5 (list 1 2 3 4))))

  (is (= (list 6 5 4 3 2 1) (reverse (list 1 2 3 4 5 6))))
  (is (= (list 6 5 4 3 2)   (reduce (fn [h v] (conj h (inc v))) (list) (list 1 2 3 4 5))))
  (is (= (list 4 3 2 1 0)   (reduce (fn [h v] (conj h (dec v))) (list) (list 1 2 3 4 5))))

  
  (is (= 1 (first (repeatedly 3 (fn [] 1)))))
  (is (= 3 (count (repeatedly 3 (fn [] 1)))))
  (is (= 2 (->> (repeatedly 3 (fn [] 1)) (map inc) first)))
  (is (= 2 (->> (repeatedly (fn [] 1)) (take 3) (map inc) reverse first)))
  
  (is (= 2 (count (filter true? (list true false true false)))))
  (is (= 2 (count (filter false? (list true false true false)))))
  (is (= 3 (count (filter false? (list true false true false false)))))
  (is (= 2 (count (filter (fn [x] (not (false? x))) (list true false true false false)))))

  (let [sum (cxx "var alist = runtime::list(obj<number>(1),obj<number>(2),obj<number>(3));
                  number_t sum = 0;
                  for(auto const& it : runtime::range(alist)){
                    sum += number::to<number_t>(it);
                  }
                  __result = obj<number>(sum);")]
    (is (= 6 sum))))
(defn lazy-countdown [n]
  (if (>= n 0)
    (cons n (lazy-seq (lazy-countdown (- n 1))))))

(defn ints-from [n]
  (cons n (lazy-seq (ints-from (inc n)))))

(defn fib-seq
  ([]
   (fib-seq 0 1))
  ([a b]
   (lazy-seq
    (cons b (fib-seq b (+ a b))))))

(deftest lazy-seq-test
  (is (= false (= (range 10) (range 15))))
  (is (= false (= (range 15) (range 10))))
  (is (= true  (= (range 10) (range 10))))
  (is (= 10    (first (ints-from 10))))
  (is (= 11    (first (rest (ints-from 10)))))
  (is (= 12    (first (rest (rest (ints-from 10))))))
  (is (= 10    (first (lazy-countdown 10))))
  (is (= 9     (first (rest (lazy-countdown 10)))))
  (is (= 8     (first (rest (rest (lazy-countdown 10))))))
  (is (= 11    (count (lazy-countdown 10))))

  (is (= 2   (first (map inc (list 1 2 3)))))
  (is (= 0   (first (map dec (list 1 2 3)))))
  (is (= 4   (first (map (fn [x] (+ 3 x)) (list 1 2 3)))))
  (is (= 3   (count (map inc (list 1 2 3)))))
  (is (= 10  (apply + (range 5))))
  (is (= 5   (count (range 5))))
  (is (= 2   (first (take 2 (map inc (list 1 2 3))))))
  (is (= 3   (first (rest (take 2 (map inc (list 1 2 3)))))))
  (is (= 3   (count (take 20 (map inc (list 1 2 3))))))
  (is (= 1   (first (concat (list 1 2 3) (list 4 5 6)))))
  (is (= 4   (first (drop 3 (concat (list 1 2 3) (list 4 5 6))))))
  (is (= 21  (reduce + (concat (list 1 2 3) (list 4 5 6)))))

  (is (= (list -2 -1)          (take-while neg? (list -2 -1 0 1 2 3))))
  (is (= (list -2 -1 0 1 2)    (take-while #(< % 3) (list -2 -1 0 1 2 3))))
  (is (= (list -2 -1 0 1 2 3)  (take-while #(<= % 3) (list -2 -1 0 1 2 3))))
  (is (= (list -2 -1 0 1 2 3)  (take-while #(<= % 4) (list -2 -1 0 1 2 3))))
  
  (is (empty? (concat)))
  
  (= (list 1 1 2 3 5) (take 5 (fib-seq)))
  (= 12 (apply + (take 5 (fib-seq))))

  (is (= (list (list 0 1 2 3) (list 4 5 6 7))                              (partition 4 (range 10))))
  (is (= (list (list 0 1 2 3) (list 4 5 6 7))                              (partition 4 (range 8))))
  (is (= (list (list 0 1 2 3) (list 6 7 8 9) (list 12 13 14 15))           (partition 4 6 (range 20))))
  (is (= (list (list 0 1 2) (list 6 7 8) (list 12 13 14) (list 18 19 42))  (partition 3 6 (list 42) (range 20))))
  (is (= (list (list 0 1 2 3) (list 6 7 8 9) (list 12 13 14 15) (list 18 19 42 43)) (partition 4 6 (list 42 43 44 45) (range 20)))))
(deftest d-list-test
  (let [m (new-d-list 0 (list 0 1)
                      1 (list 1 2))
        mr {:a 1 :b 2}
        mn {1 2 3 4}]

    (is (= (list 1 0)                    (keys m)))
    (is (= (list (list 1 2) (list 0 1))  (vals m)))
    (is (= (list 1 2)                    (m 1)))
    (is (= m                             m))
    (is (= (list 0)                      (keys (dissoc m 1))))
    (is (= mr                            mr))
    (is (= (list :b :a)                  (keys mr)))
    (is (= (list 2  1)                   (vals mr)))
    (is (= 1                             (:a mr)))
    (is (= 1                             (get mr :a 10)))
    (is (= 10                            (get mr :c 10)))
    (is (= 1                             (mr :a)))
    (is (= 1                             (mr :a 10)))
    (is (= 10                            (mr :c 10)))
    (is (= 1                             (:a mr)))
    (is (= 1                             (:a mr 10)))
    (is (= 10                            (:c mr 10)))
    (is (= 6                             (->> mn
                                              (map second)
                                              (apply +))))))
(deftest lambda-test
  (let [f1 (fn [])
        f2 (fn [])
        m-func (fn
                 ([a] 1)
                 ([a b] 2)
                 ([a b & c] 3)
                 ([a b [c d] & e] 4))]
    (is (= true  (= f1 f1)))
    (is (= false (= f1 f2)))
    (is (= true  (= f1 (do f1))))
    (is (= false (= f2 (do f1))))
    (is (= 1 (m-func 1)))
    (is (= 2 (m-func 1 2)))
    (is (= 3 (m-func 1 2 3)))
    (is (= 4 (m-func 1 2 (list 3 3) 4)))))
(deftest pointer-test
  (let [a-ptr (cxx "__result = obj<pointer>(nullptr);")
        b-ptr (cxx "__result = obj<pointer>(new int);")
        gc    (fn [p] "delete pointer::to_pointer<int>(p);")]
    (is (= true  (= a-ptr a-ptr)))
    (is (= false (= a-ptr b-ptr)))
    (is (= true  (= b-ptr b-ptr)))
    (gc b-ptr)))

(deftest value-test
  (let [obj-42 (make-data 42)
        obj-24 (make-data 24)
        val-42 (get-data obj-42)
        val-24 (get-data obj-24)]
    (is (=    obj-42 obj-42))
    (is (not= obj-42 obj-24))
    (is (=    val-42 42))
    (is (=    val-24 24))
    (is (=    25     (do (inc-data obj-24) 
                         (get-data obj-24))))))
(deftest atom-test
  (let [a (atom nil)
        b (atom nil)]
    (is (= nil          (deref a)))
    (is (= 1            (do (reset! a 1) (deref a))))
    (is (= 2            (do (swap! a inc) (deref a))))
    (is (= (list 1 2 3) (do (reset! a (list 1 2 3)) (deref a))))
    (is (= 6            (do (swap! a (fn [l] (reduce + l))) (deref a))))
    (is (= true         (= a a)))
    (is (= false        (= a b)))
    (is (= false        (= a 3.14)))))
(deftest keyword-test
  (is (= true  (= :test  :test)))
  (is (= false (= :test  :other_test)))
  (is (= true  (= :space (cxx "__result = obj<keyword>(\":space\")")))))
(deftest string-test
  (let [s1       "Some String"
        s1-added "ASome String"
        s2       "Other String"
        s1-ret   (fn [] "__result = obj<string>(\"Some String\");")
        s1-eq    (fn [s] "__result = obj<boolean>((string::to<std::string>(s) == \"Some String\"))")
        s2       "Ali Topu At"
        s3       (fn [] "std::string s = \"Some String\";
                        __result = obj<string>(s);")]
    (is (= s2 (new-string "Ali Topu At")))
    (is (= false (= s1 s2)))
    (is (= true  (= s1 s1)))
    (is (= true  (= s1 (s3))))
    (is (= false (= s1 3.14)))
    (is (= true  (= s1 (s1-ret))))
    (is (= true  (s1-eq s1)))
    (is (= 99 \c))
    (is (= \S (first s1)))
    (is (= s1-added (cons 65 s1)))
    (is (= s1 (rest (cons 65 s1))))))
(deftest future-test
  (is (= 42    @(future (+ 41 1))))
  (is (= 42    @(future (sleep 100) (+ 40 2))))
  (is (= false  (future-done? (future (sleep 100) :false))))
  (is (= true   (let [f (future :true)]
                  (deref f)
                  (future-done? f))))
  (is (= 42    @(thread #(+ 41 1)))))

(deftest delay-test
  (let [d (delay   (+ 1 1))]
    (is (= true    (delay? d)))
    (is (= 2       @d))
    (is (= 2       @d))
    (is (= 42      (force (delay 42))))))    

(run-all-tests)
