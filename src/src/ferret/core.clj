
(ns ferret.core
  (:refer-clojure :exclude [compile])
  (:gen-class)
  (:use [clojure.java.io])
  (:require [clojure.set :as set]
            [fast-zip.core :as zip]
            [clojure.walk :as walk]
            [clojure.pprint :as pprint]
            [clojure.tools.cli :refer [parse-opts]]
            [watchtower.core :as watcher]
            [clojure.term.colors :as color])
  (:use [ferret [template :only [render-template]]]
        [clojure.java.shell]
        [clojure.java.io]
        [clojure.tools [logging :only [log warn info trace]]])
  (:import (org.apache.commons.io FileUtils)
           (java.io BufferedReader InputStreamReader)))

(defn os-name []
  (let [os (-> (System/getProperty "os.name") .toLowerCase)]
    (cond (.contains os "win")      :windows
          (.contains os "mac")      :mac
          (or (.contains os "nix")
              (.contains os "nux")
              (.contains os "aix")) :unix
              (.contains os "sunos")    :solaris)))

(defn read-file-from-url [f]
  (with-open [in (.getResourceAsStream (ClassLoader/getSystemClassLoader) f)
              rdr (BufferedReader. (InputStreamReader. in))]
    (apply str (interpose \newline (line-seq rdr)))))

(defn read-file [f & [options]]
  (try
    (read-file-from-url f)
    (catch Exception e-url
      (try
        (if (nil? options)
          (FileUtils/readFileToString (file f))
          (FileUtils/readFileToString (file (str (:path options) f))))
        (catch Exception e-path
          (warn "error reading =>" f)
          (System/exit 1))))))

(defn read-clojure-file [f]
  (read-string (str \( (read-file f) \))))

(defn write-to-file [f s]
  (FileUtils/writeStringToFile (file f) (.trim s)))

(defn escape-string [s]
  (org.apache.commons.lang.StringEscapeUtils/escapeJava s))

(defn file-path [file]
  (let [path (str (org.apache.commons.io.FilenameUtils/getPrefix file)
                  (org.apache.commons.io.FilenameUtils/getPath file))]
    (if (empty? path)
      "./"
      path)))

(def default-cpp-extension "cpp")

(defn file-extension [f]
  (org.apache.commons.io.FilenameUtils/getExtension f))

(defn file-base-name [f]
  (org.apache.commons.io.FilenameUtils/getBaseName f))

(defn file-exists [f]
  (.exists (file f)))

(defn make-file [p n e]
  (file (str p n "." e)))
(defn morph-form [tree pred f]
  (walk/prewalk (fn [x]
                  (if (pred x)
                    (f x)
                    x)) tree))

(defn remove-form [tree pred]
  (if (every? true? (map #(pred %) tree))
    (list )
    (loop [loc (zip/seq-zip tree)]
      (if (zip/end? loc)
        (zip/root loc)
        (recur
         (zip/next
          (if (pred (zip/node loc))
            (zip/remove loc)
            loc)))))))

(defn select-form [tree pred]
  (loop [loc (zip/seq-zip tree)
         nodes []]
    (if (zip/end? loc)
      nodes
      (recur
       (zip/next loc)
       (if (pred (zip/node loc))
         (conj nodes (zip/node loc))
         nodes)))))

(defn is-form? [s f]
  (and (seq? f)
       (= (first f) s)))

(defn is-form-fn? [& s]
  (fn [f]
    (some true? (map #(is-form? % f) s))))
(defn append-to! [r ks v]
  (let [cv (reduce (fn[h v] (v h)) @r ks)]
    (swap! r assoc-in ks (conj cv v))
    ""))
(def log-formatter (proxy [java.util.logging.Formatter] []
                     (format
                       [^java.util.logging.LogRecord record]
                       (let [level (-> record .getLevel .toString clojure.string/lower-case)
                             level (if (or (= :unix (os-name))
                                           (= :mac (os-name))
                                           (= :solaris (os-name)))
                                     (if (= level "warning")
                                       (color/red level)
                                       (color/green level))
                                     level)
                             now (.getTime (java.util.Calendar/getInstance))
                             frmtr (java.text.SimpleDateFormat. "HH:mm:ss")]
                         (str (.format frmtr now) " " level " " (.getMessage record) "\n")))))

(def log-handler (proxy [java.util.logging.Handler] []
                   (publish [^java.util.logging.LogRecord record]
                     (when (and (.isLoggable ^java.util.logging.Handler this record)
                                (instance? java.io.PrintWriter *out*))
                       (.print ^java.io.PrintWriter *out* 
                               (.format ^java.util.logging.Formatter log-formatter record))))
                   (flush [] (.flush ^java.io.PrintWriter *out*))
                   (close [] 
                     ;;(.close *out*)
                     )))

(.addHandler (java.util.logging.Logger/getLogger "") log-handler)

(let [^java.util.logging.LogManager$RootLogger logger (java.util.logging.Logger/getLogger "")]
  (doseq [^java.util.logging.Handler handler (.getHandlers logger)]
    (. handler setFormatter log-formatter)))

(defn set-log-level! [& [level]]
  (let [^java.util.logging.LogManager$RootLogger logger
        (java.util.logging.Logger/getLogger "")
        level (cond (nil? level) java.util.logging.Level/ALL
                    (= level :trace) java.util.logging.Level/FINEST
                    (= level :debug) java.util.logging.Level/FINE
                    (= level :info) java.util.logging.Level/INFO
                    (= level :warn) java.util.logging.Level/WARNING)]
    
    (.setLevel logger level)
    (doseq [^java.util.logging.Handler handler (.getHandlers logger)]
      (. handler setLevel level))))

(set-log-level! :info)
(defn expand-reader-macros [form]
  (-> form
      (morph-form (is-form-fn? 'clojure.core/deref)
                  (fn [f] `(~'deref ~@(rest f))))
      (morph-form map? (fn [x] (cons 'new-d-list (-> x seq flatten))))))
(defn macro-normalize [f]
  (morph-form f
              (is-form-fn? 'let)
              (fn [[_ bindings & body]]
                `(~'let* ~(apply list bindings) ~@body))))
(declare expand-macros-all)

(defn expand-macros [form]
  (let [core-macros (->> (read-clojure-file "ferret/runtime.clj")
                         (filter (is-form-fn? 'defmacro)))
        core-macro-symbols (into #{} (map second core-macros))
        form-macros (->> (filter (is-form-fn? 'defmacro) form)
                         (filter (fn [[_ name]]
                                   (not (core-macro-symbols name)))))
        form-macro-symbols (map second form-macros)
        form (remove-form form (is-form-fn? 'defmacro))
        temp-ns (gensym)
        macro-symbols (concat core-macro-symbols form-macro-symbols)]
    
    (create-ns temp-ns)
    (binding [*ns* (the-ns temp-ns)]
      (refer 'clojure.core :exclude (concat macro-symbols ['fn 'def]))
      (use '[ferret.core :only [symbol-conversion fn->unique-args]])
      
      (doseq [m (concat core-macros form-macros)]
        (eval m)))
    
    (let [form (-> form
                   (macro-normalize)
                   (expand-reader-macros)
                   (morph-form (apply is-form-fn? macro-symbols)
                               (fn [f]
                                 (binding [*ns* (the-ns temp-ns)]
                                   (walk/macroexpand-all f)))))]
      (remove-ns temp-ns)
      form)))

(defn expand-macros-all-aux [form]
  (loop [f form]
    (let [expanded (expand-macros f)]
      (if (= f expanded)
        expanded
        (recur expanded)))))

(def expand-macros-all (memoize expand-macros-all-aux))
(defn shake-concat
  ([header form]
   (let [shakeable? (fn [f]
                      (or (is-form? 'defn f)
                          (is-form? 'defnative f)))
         header-symbols (->> (select-form header seq?)
                             flatten
                             (filter symbol?)
                             (into #{}))
         header-fns (->> (select-form header shakeable?)
                         (map #(vector (second %) %))
                         (into {}))
         header-non-shakeable (remove-form header shakeable?)
         form-expanded (expand-macros-all (concat header-non-shakeable form))
         fns (atom #{})
         _ (shake-concat form-expanded header-fns fns header-non-shakeable)
         header-shaked (remove-form header (fn [f]
                                             (and (shakeable? f)
                                                  (not (@fns (second f))))))]
     (concat header-shaked form)))
  ([form built-in fns non-shakeable]
   (morph-form form symbol?
               #(do
                  (if-let [f (built-in %)]
                    (when (not (@fns %))
                      (swap! fns conj %)
                      (shake-concat (expand-macros-all (concat non-shakeable f))
                                    built-in fns non-shakeable))) %))))
(defn escape-fn-calls [form]
  (morph-form form
              (fn [f]
                (and (seq? f)
                     (is-form? 'fir-new-lambda (first f))))
              (fn [f]
                (let [[[_ & fn] & args] f]
                  `((~'fir-new-lambda-stack ~@fn) ~@args)))))
(defn escape-fn-dispatch [form]
  (morph-form form
              (is-form-fn? 'fn-multi-arity)
              (fn [f]
                (morph-form f
                            (is-form-fn? 'fir-new-lambda)
                            (fn [[_ & f]]
                              `(~'fir-new-lambda-stack ~@f))))))
(defn escape-lambdas [form]
  (let [stack-lambda-pred (fn [f]
                            (and (seq? f)
                                 (is-form? 'fir-new-lambda (first f))))
        heap-lambdas (->> (select-form form (fn [f]
                                              (and (seq? f)
                                                   (is-form? 'fir-new-lambda f))))
                          (map second)
                          (into #{}))
        stack-lambdas (->> (select-form form (fn [f]
                                               (and (seq? f)
                                                    (is-form? 'fir-new-lambda-stack f))))
                           (map second)
                           (into #{}))
        escapeable-lambdas (clojure.set/difference stack-lambdas heap-lambdas)]
    (morph-form form
                (fn [f]
                  (and (seq? f)
                       (= (first f) 'fir-lambda)
                       (escapeable-lambdas (second f))))
                (fn [[_ & f]]
                  `(~'fir-lambda-stack ~@f)))))
(defn import-modules-select-require [form]
  (let [norm-require (fn [f]
                       (if (symbol? f)
                         [f :as f]
                         f))]
    (->> (select-form form (is-form-fn? 'require))
         (reduce (fn[h v]
                   (if (= 2 (count v))
                     ;; require single module
                     (conj h (norm-require (->> v last last)))
                     ;; require multiple modules
                     (concat h (map #(norm-require (last %)) (rest v))))) [])
         (map (fn [[mod _ as]] [mod as]))
         (reduce (fn[h [mod as]]
                   (if (h mod)
                     (assoc h mod (conj (h mod) as))
                     (assoc h mod [as]))) {}))))
(defn import-modules-load-modules [package-list options]
  (->> package-list
       (reduce (fn[h [m aliases]]
                 (let [mod           (-> (str (:path options) (.replace (str m) "." "/") ".clj")
                                         (read-clojure-file)
                                         (remove-form (is-form-fn? 'configure-runtime!))
                                         (remove-form (is-form-fn? 'configure-ferret!)))
                       macro-symbols (->> (select-form mod (is-form-fn? 'defmacro))
                                          (map second)
                                          (into #{}))
                       def-symbols    (->> (select-form (expand-macros-all mod) (is-form-fn? 'def))
                                           (map second)
                                           (into #{}))
                       replace?        (set/union macro-symbols def-symbols)
                       mod             (morph-form
                                        mod #(and (symbol? %) (replace? %))
                                        #(symbol (str (.replace (str m) "." "_") "_" %)))]
                   (reduce (fn [h v] (conj h v)) h mod)))
               [])
       lazy-seq))
(defn import-modules-convert-alias-to-module [package-list form]
  (let [alias-to-mod (reduce (fn[h [mod aliases]]
                               (reduce (fn[h v] (assoc h v mod)) h aliases))
                             {} package-list)]
    (morph-form form symbol?
                (fn [f]
                  (if-let [[_ alias fn] (re-find #"(.*?)/(.*)" (str f))]
                    (if-let [mod-sym (alias-to-mod (symbol alias))]
                      (symbol (str (.replace (str mod-sym) "." "_") "_" fn))
                      f)
                    f)))))
(defn import-modules [form options]
  (let [package-list (import-modules-select-require form)
        form         (remove-form form (is-form-fn? 'require))
        modules      (import-modules-load-modules package-list options)
        form         (import-modules-convert-alias-to-module package-list form)]
    (shake-concat modules form)))

(defn import-modules-all [form options]
  (loop [f form]
    (let [expanded (import-modules f options)]
      (if (= f expanded)
        expanded
        (recur expanded)))))
(defn ferret-runtime [options form]
  (->> (-> form
           (import-modules-all options)
           (expand-reader-macros))
       (shake-concat (read-clojure-file "ferret/runtime.clj"))
       ;; tag form with the build info
       (cons `(~'native-define ~(try
                                  (let [version (read-file-from-url "build.info")]
                                    (str "// ferret-lisp build:" version))
                                  (catch Exception e
                                    (str "// ferret-lisp")))))))
(defn fn->unique-args [form]
  (let [valid-symbol? (fn [s] (and (symbol? s) (not= s '&) (not= s '_)))
        [args & body] form]
    (if (string?  (->> body
                       (filter #(not (is-form? 'native-declare %)))
                       first))
      `(~'fn* ~args ~@body)
      (let [unique-args (->> args
                             flatten
                             (filter valid-symbol?)
                             (map #(symbol (str % (gensym "__")))))
            replace? (->> (interleave (->> args
                                           flatten
                                           (filter valid-symbol?))
                                      unique-args)
                          (apply hash-map))
            unique-body (walk/prewalk
                         (fn [x]
                           (if (and (symbol? x)
                                    (replace? x))
                             (replace? x)
                             x)) body)
            args (->> (morph-form args #(replace? %) #(replace? %))
                      (into []))]
        `(~'fn* ~args ~@unique-body)))))
(defn let->fn [form]
  (-> form

      (morph-form (is-form-fn? 'let*)
                  (fn [[_ bindings & body]]
                    (if (empty? bindings)
                      `((~'fn () ~@body))
                      (apply
                       (fn close [[arg val] & more]
                         (if (empty? more)
                           `((~'fn [~arg] ~@body) ~val)
                           `((~'fn [~arg] ~(apply close more)) ~val)))
                       (partition 2 bindings)))))

      (morph-form (is-form-fn? 'fn)
                  (fn [[_ & body]]
                    (fn->unique-args body)))))
(defn do->fn [form]
  (morph-form form
              (is-form-fn? 'do)
              (fn [f] `((~'fn* () ~@(rest f))))))
(defn lambda-defined? [fns env args body]
  (if-let [fn-name (@fns (concat [env args] body))]
    (apply list 'fir-new-lambda fn-name env)))

(defn define-lambda [fns env args body]
  (let [n (gensym)]
    (swap! fns assoc (concat [env args] body) n)
    (apply list 'fir-new-lambda n env)))

(defn closure-conversion
  ([form]
   (let [fns  (atom {})
         form (closure-conversion form fns)
         fns  (map (fn [[body name]] (concat ['fir-lambda name] body)) @fns)]
     (concat form fns)))
  ([form fns & env]
   (morph-form form
               (is-form-fn? 'fn*)
               (fn [[_ args & body]]
                 (let [env  (if (nil? env) '() (first env))
                       body (closure-conversion body fns (concat args env))]
                   (if-let [n (lambda-defined? fns env args body)]
                     n
                     (define-lambda fns env args body)))))))
(defn escape-cpp-symbol [s]
  (clojure.string/escape
   (str s)
   {\- \_ \* "_star_" \+ "_plus_" \/ "_slash_"
    \< "_lt_" \> "_gt_" \= "_eq_" \? "_QMARK_"
    \! "_BANG_" \# "_"}))

(defn symbol-conversion [form]
  (let [c (comp #(symbol (escape-cpp-symbol %))
                #(cond (= 'not %) '_not_
                       :default %))]
    (morph-form form symbol? c)))
(defn remove-assertions [options form]
  (if (:release options)
    (do (info "option => release mode")
        (remove-form form (is-form-fn? 'assert)))
    form))
(defn select-def-fn [form]
  (->> (select-form form (is-form-fn? 'def))
       (filter (fn [[_ name val]]
                 (and (seq? val)
                      (= 'fir-new-lambda (first val)))))))

(defn replace-fn-call-sites-pure [form fn-defs fn-table]
  (let [no-global-fn (reduce (fn[h v]
                               (remove-form h (fn [f]
                                                (and (seq? f)
                                                     (= 'def (first f))
                                                     (every? true? (map = f v))))))
                             form fn-defs)        
        embeded-fn-calls (reduce (fn[h [name gensym]]
                                   (morph-form h  #(and (symbol? %)
                                                        (= % name))
                                               (fn [_] (list 'fir-new-lambda gensym))))
                                 no-global-fn fn-table)
        embed-fn-names (reduce (fn[h [name gensym]]
                                 (morph-form h #(and (symbol? %)
                                                     (= % gensym))
                                             (fn [_] (identity name))))
                               embeded-fn-calls fn-table)]
    embed-fn-names))

(defn replace-fn-call-sites [options form]
  (if (:global-functions options)
    form
    (let [pure-fn-defs (->> (select-def-fn form)
                            (filter #(= 2 (-> % last count))))
          pure-fn-table (map (fn [[_ name [_ gensym]]] [name gensym]) pure-fn-defs)
          form (replace-fn-call-sites-pure form pure-fn-defs pure-fn-table)
          closure-fn-defs (->> (select-def-fn form)
                               (filter #(not= 2 (-> % last count))))
          closure-fn-table (map (fn [[_ name [_ gensym]]] [name gensym]) closure-fn-defs)]
      (reduce (fn[h [name gensym]]
                (morph-form h #(and (symbol? %)
                                    (= % gensym))
                            (fn [_] (symbol (str name "_" gensym)))))
              form closure-fn-table))))
(defn escape-analysis [form]
  (->> (escape-fn-calls form)
       (escape-fn-dispatch)
       (escape-lambdas)))
(defn compile [form options]
  (->> (ferret-runtime options form)
       (remove-assertions options)
       (expand-macros-all)
       (let->fn)
       (do->fn)
       (closure-conversion)
       (replace-fn-call-sites options)
       (escape-analysis)
       (symbol-conversion)))
(defmulti emit (fn [_ form _]
                 (cond (is-form? 'fir_lambda form) 'fir_lambda
                       (is-form? 'fir_lambda_stack form) 'fir_lambda_stack
                       (is-form? 'fn_multi_arity form) 'fn_multi_arity
                       (is-form? 'fir_new_lambda form) 'fir_new_lambda
                       (is-form? 'fir_new_lambda_stack form) 'fir_new_lambda_stack
                       (is-form? 'defobject form) 'defobject
                       (is-form? 'native_header form) 'native_header
                       (is-form? 'native_declare form) 'native_declare
                       (is-form? 'native_define form) 'native_define
                       (is-form? 'if form) 'if
                       (is-form? 'def form) 'def
                       (symbol? form) :symbol
                       (keyword? form) :keyword
                       (number? form) :number
                       (nil? form) :nil
                       (char? form) :char
                       (string? form) :string
                       (or (true? form) (false? form)) :boolean
                       (seq? form) :invoke-lambda)))

(defn emit-ast
  [options ast state]
  (reduce (fn[h v]
            (conj h (emit options v state)))
          [] ast))
(defn emit-source [form options]
  (let [state (atom {:native-headers []
                     :native-declarations []
                     :objects []
                     :symbol-table #{}
                     :lambdas []
                     :native-defines []})
        ast (compile form options)
        body (emit-ast options ast state)]
    (when (:ast options)
      (pprint/pprint ast))
    (assoc @state :body body)))
(defmethod emit :symbol [_ form state] (str form))

(defmethod emit :string [_ form state]
  (str "obj<string>(\"" (escape-string form) "\",(number_t)" (count form) ")"))

(defmethod emit :boolean [_ form state]
  (if (true? form)
    (str "cached::true_t")
    (str "cached::false_t")))

(defmethod emit :nil [_ form state] "nil()")

(defmethod emit :keyword [_ form _]
  (str "obj<keyword>(" (reduce (fn[h v] (+ h (int v))) 0 (str form)) ")"))

(defmethod emit :char [_ form state] (str "obj<number>((number_t)" (int form) ")"))

(defmethod emit :number [_ form state] (str "obj<number>((real_t)" (double form) ")"))
(defmethod emit 'def [options [_ name & form] state]
  (append-to! state [:symbol-table] name)
  (str "(" name " = " (apply str (emit-ast options form state)) ")"))

(defmethod emit 'if [options [_ cond t f] state]
  (let [cond (emit options cond state)
        t (emit options t state)
        f (if (nil? f) "nil()" (emit options f state))]
    (apply str "(" cond " ? " t " : " f ")")))

(defn defobject [name f options]
  (let [def (read-file (first f) options)]
    (render-template
     "$if(embed_type)$
        namespace runtime {
          namespace type {
             const size_t $type$ = $type_val$;}}
        $endif$
      $body$"
     :embed_type  (.contains def (str "runtime::type::" name))
     :type        (str name)
     :type_val    (gensym "")
     :body        def)))

(defmethod emit 'defobject [options [_ name & spec] state]
  (append-to! state [:objects] (defobject name spec options)))

(defmethod emit 'native_header [_ [_ & declarations] state]
  (append-to! state [:native-headers] declarations))

(defmethod emit 'native_declare [_ [_ declaration] state]
  (append-to! state [:native-declarations] declaration))

(defmethod emit 'native_define [_ [_ define] state]
  (append-to! state [:native-defines] define))
(defn norm-lambda-env [env]
  (->> env
       (flatten)
       (filter #(and (not (= '& %))
                     (not (= '_ %))
                     (not (= :as %))))))

(defn new-lambda-heap [l]
  (let [n (second l)
        e (norm-lambda-env (drop 2 l))]
    (if (empty? e)
      (str "obj<" n ">()")
      (str "obj<" n ">(" (apply str (interpose \, e)) ")"))))

(defn new-lambda-stack [l]
  (let [n (second l)
        e (norm-lambda-env (drop 2 l))]
    (if (empty? e)
      (str n "()")
      (str n "(" (apply str (interpose \, e)) ")"))))

(defn invoke-lambda [n args]
  (if (empty? args)
    (str "run(" n ")")
    (str "run(" n ","  (apply str (interpose \, args))")")))
(declare destructure-arguments)

(defn destructure-nth-rest [parent pos]
  (reduce (fn[h v] (str v "(" h ")")) parent (repeat pos "runtime::rest")))

(defn destructure-nth [parent pos]
  (str "runtime::first(" (destructure-nth-rest parent pos) ")"))

(defn destructure-get [name parent key]
  (str "const var " name " = "
       parent ".cast<d_list>()->val_at(" (emit nil key nil) ");"))

(defn new-lambda-arg [name parent pos]
  (str "const var " name " = " (destructure-nth parent pos)))

(defn new-lambda-var-arg [name parent pos]
  (str "const var " name " = " (destructure-nth-rest parent pos)))

(defn destructure-associative [name parent pos]
  (let [tmp-name (gensym)]
    [(new-lambda-arg tmp-name parent pos)
     (map (fn [[s k]] (destructure-get s tmp-name k)) name)]))

(defn destructure-sequential [args parent]
  (reduce
   (fn [h [pos name]]
     (let [name (cond (symbol? name) (new-lambda-arg name parent pos)
                      (map?    name) (destructure-associative name parent pos)
                      (coll?   name) (destructure-arguments name (destructure-nth parent pos)))]
       (conj h name))) [] args))

(defn destructure-var-args [name parent pos]
  (cond (nil?     name)  []
        (symbol?  name)  (new-lambda-var-arg name parent pos)
        (coll?    name)  (let [tmp-name (gensym)]
                           [(new-lambda-var-arg tmp-name parent pos)
                            (destructure-arguments name tmp-name)])))

(defn destructure-as-arg [name parent]
  (if (symbol?     name)
    (new-lambda-var-arg name parent 0)
    []))

(defn destructure-arguments
  ([args]
   (->> (destructure-arguments args "_args_") flatten))
  ([args parent]
   (let [t-args         args
         args           (take-while #(and (not= % '&) (not= % :as)) t-args)
         var-args       (->> t-args (drop-while #(not= % '&)) second)
         as-arg         (->> t-args (drop-while #(not= % :as)) second)
         args-indexed   (->>  args
                              (map-indexed (fn [p v] [p v]))
                              (filter #(not= (second %) '_)))
         as-arg         (destructure-as-arg as-arg parent)
         var-args       (destructure-var-args var-args parent (count args))
         args           (destructure-sequential args-indexed parent)]
     [args var-args as-arg])))
(defmethod emit :invoke-lambda [options [fn & args] state]
  (invoke-lambda (emit options fn state) (emit-ast options args state)))

(defmethod emit 'fir_new_lambda [_ f state]
  (new-lambda-heap f))

(defmethod emit 'fir_new_lambda_stack [_ f state]
  (new-lambda-stack f))

(defn emit-lambda [options name env args body state]
  (let [native-declarations (filter #(is-form? 'native_declare %) body)
        body (filter #(not (is-form? 'native_declare %)) body)
        body (cond  (empty? body)
                    ["nil()"]
                    (and (= 1 (count body))
                         (seq? (first body))
                         (= 'fn_multi_arity (first (first body))))
                    [(emit options (first body) state) "nil()"]
                    (and (= 1 (count body))
                         (string? (first body)))
                    (let [body (first body)]
                      (if (.contains body "__result")
                        ["var __result" body "__result"]
                        [body "nil()"]))
                    :default (emit-ast options body state))
        env  (norm-lambda-env env)
        vars (destructure-arguments args)]
    (doseq [dec native-declarations] 
      (emit options dec state))
    {:name name :env env :args args :vars vars :body body}))

(defmethod emit 'fir_lambda [options [_ name env args & body] state]
  (append-to! state [:lambdas] (emit-lambda options name env args body state)))

(defmethod emit 'fir_lambda_stack [options [_ name env args & body] state]
  (append-to! state [:lambdas] (-> (emit-lambda options name env args body state)
                                   (assoc :stack true))))
(defmethod emit 'fn_multi_arity [_ [_ switch default] state]
  (let [default (if default
                  (new-lambda-stack default))
        switch  (map (fn [[s f]] {:fn (new-lambda-stack f) :case s}) switch)]
    (render-template
     "switch(runtime::count(_args_)) {

      $fns: {fn|
         case $fn.case$ :
           return $fn.fn$.invoke(_args_);
      };separator=\"\n\"$

      $if(default)$
         default:
           return $default$.invoke(_args_);
      $endif$
      }"
     :fns     switch
     :default default)))
(defn lambda-definitions [fns]
  (render-template
   "$fns: {fn|
      $if(!fn.stack)$
       class $fn.name$ final : public lambda_i{
      $else$
       class $fn.name$  \\{
      $endif$
        $fn.env:{const var $it$;} ;separator=\"\n\"$
      public:
        $if(fn.env)$
          explicit $fn.name$ ($fn.env:{var const & $it$} ;separator=\",\"$) :
            $fn.env:{$it$($it$)} ;separator=\",\"$ { }
        $endif$

        var invoke (var const & _args_) const $if(!fn.stack)$ final $endif$ ;
      };};separator=\"\n\n\"$"
   :fns fns))

(defn lambda-implementations [fns]
  (render-template
   "$fns: {fn|
      inline var $fn.name$::invoke (var const & _args_) const {
        (void)(_args_);
        $fn.vars:{$it$;} ;separator=\"\n\"$
   
        $trunc(fn.body):{$it$;} ;separator=\"\n\"$
        return $last(fn.body):{$it$;} ;separator=\"\n\"$
      }
     };separator=\"\n\n\"$"
   :fns fns))
(defn program-template [source options]
  (let [{:keys [body lambdas symbol-table native-headers objects
                native-declarations native-defines]} source
        native-headers (->> native-headers flatten (into #{}))]
    (render-template
     "
        $native_defines:{$it$} ;separator=\"\n\"$
        $native_headers:{#include \"$it$\"} ;separator=\"\n\"$

        $ferret_h$

        // Objects
        namespace ferret{
         $objects:{$it$} ;separator=\"\n\"$
        }

        // Symbols
        namespace ferret{
         $symbols:{var $it$;} ;separator=\"\n\"$
        }

        $native_declarations:{$it$} ;separator=\"\n\"$

        // Runtime Implementations
        $ferret_cpp$

        // Lambda Prototypes
        namespace ferret{
          $lambda_classes:{$it$} ;separator=\"\n\"$
        }

        // Command Line Arguments
        #if defined(FERRET_STD_LIB) &&                    \\
            !defined(FERRET_DISABLE_CLI_ARGS) &&   \\
            !defined(FERRET_DISABLE_STD_MAIN)
          ferret::var _star_command_line_args_star_;
        #endif

        // Lambda Implementations
        namespace ferret{
          $lambda_bodies:{$it$} ;separator=\"\n\"$
        }

        // Program Run
        namespace ferret{
         namespace program{
          void run(){
           $body:{$it$;} ;separator=\"\n\"$ 
          }
         }
        }

        $ferret_main$"
     :native_defines       native-defines
     :ferret_h             (read-file "ferret/runtime.h")
     :native_headers       native-headers
     :objects              objects
     :symbols              symbol-table
     :native_declarations  native-declarations
     :ferret_cpp           (read-file "ferret/runtime.cpp")
     :lambda_classes       (lambda-definitions lambdas)
     :lambda_bodies        (lambda-implementations lambdas)
     :body                 (filter #(not (empty? %)) body)
     :ferret_main          (read-file "ferret/main.cpp"))))
(defn compile-options [& [options]]
  (merge {:compiler "g++"
          :compiler-options ["-std=c++11"]
          :source-extension default-cpp-extension
          :base-name "solution"}
         options))

(defn cpp-file-name [options]
  (str (:output-path options) (:base-name options) "." (:source-extension options)))
(defn compile-options-parse-source [file]
  (try
    (let [program (slurp file)
          options (->> program
                       (re-seq #"(?s)build-conf-begin.*?//(.*?)// build-conf-end")
                       (map second)
                       (map #(.replaceAll % "//" ""))
                       (map #(.replaceAll % "\n" " "))
                       (map read-string))
          keys (->> options
                    (map #(keys %))
                    flatten
                    (into #{})
                    (into []))
          combine (fn [key]
                    (->> options
                         (reduce (fn[h v]
                                   (if (nil? (key v))
                                     h
                                     (apply merge (flatten [h (key v)])))) #{})
                         (into [])))]
      (compile-options
       (reduce (fn[h v]
                 (assoc h v (combine v))) {} keys)))
    (catch Exception e
      (compile-options {}))))
(defn build-specs [input args]
  (fn []
    (let [output (if (->> args :options :output)
                   (->> args :options :output)
                   input)
          output-path (file-path output)
          output-extension (if (->> args :options :output)
                             (file-extension output)
                             default-cpp-extension)
          base-name (file-base-name output)
          input-path (file-path input)
          default-compiled-file (make-file output-path base-name output-extension)
          default-options (compile-options-parse-source default-compiled-file)]
      
      (-> default-options
          (assoc :input-file input)
          (assoc :base-name base-name)
          (assoc :path input-path)
          (assoc :output-path output-path)
          (assoc :source-extension output-extension)
          (assoc :ast (->> args :options :ast))
          (assoc :compile-program (->> args :options :compile))
          (assoc :release (->> args :options :release))
          (assoc :format-code (not (->> args :options :disable-formatting)))
          (assoc :global-functions (->> args :options :global-functions))
          (assoc :extra-source-files
                 (cond (not (empty? (:arguments args)))
                       (:arguments args)
                       (not (empty? (:extra-source-files default-options)))
                       (:extra-source-files default-options)
                       :default []))))))
(defn compile->cpp [form options]
  (let [file-name (cpp-file-name options)
        source    (emit-source form options)
        program   (program-template source options)]
    (write-to-file file-name program)
    (info "compiled" "=>" file-name)
    true))
(defn cxx-compiler [options]
  (let [compiler    (if (System/getenv "CXX")
                      (System/getenv "CXX")
                      (:compiler options))
        env-options (if (System/getenv "CXXFLAGS")
                      (seq (.split (System/getenv "CXXFLAGS") " ")))
        options     (->> (:compiler-options options) (map str))]
    [compiler (concat options env-options)]))
(defn cxx-command [options]
  (if (:command options)
    (flatten ["/usr/bin/env" "sh" "-c" (:command options)])
    (let [[cxx cxx-options] (cxx-compiler options)
          source-files  (map #(let [extension (file-extension %)]
                                [(cond (= extension "c") ["-x" "c"]
                                       (= extension "c++") ["-x" "c++"]
                                       :default "")
                                 %])
                             (:extra-source-files options))]
      (flatten [cxx cxx-options source-files ["-x" "c++"] (cpp-file-name options)]))))
(defn compile->binary [options]
  (let [command (cxx-command options)]
    (info "building" "=>" (apply str (interpose " " command)))
    (let [build-dir (:output-path options)
          ret (try
                (with-sh-dir build-dir
                  (apply sh command))
                (catch Exception e
                  (warn (str "error executing C++ compiler."))
                  (warn (str "" (.getMessage e)))
                  (System/exit 1)))]
      (if (not= 0 (:exit ret))
        (do (warn "build error")
            (warn (:err ret))
            (System/exit 1)))
      true)))
(defn clang-format [options]
  (let [file (cpp-file-name options)
        source (try (with-sh-dir "./"
                      (sh "clang-format" "-style" "{Standard: Cpp11}" file))
                    (catch Exception e nil))]
    (if source
      (do (info "formatting code")
          (write-to-file file (:out source)))
      (trace "install clang-format for formatted output (optional)"))))

(defn build-solution [spec-fn]
  (let [{:keys [input-file compile-program format-code path]} (spec-fn)]
    (info "dir =>" path)
    (info "file =>" input-file)
    
    (compile->cpp (read-clojure-file input-file) (spec-fn))
    
    (when format-code
      (clang-format (spec-fn)))
    
    (when compile-program
      (compile->binary (spec-fn)))))
(def program-options [["-i" "--input FILE" "Input File" :default "./core.clj"]
                      ["-o" "--output FILE" "Output File"]
                      ["-c" "--compile" "Compile to Binary"]
                      ["-w" "--watch-input" "Automatically Recompile Input File on Change."]
                      [nil "--release" "Compile in Release Mode. Strip Debug Information."]
                      [nil "--disable-formatting" "Disables Solution Formatting Using clang-format."]
                      [nil "--global-functions" "Disables replace-fn-call-sites Optimization."]
                      [nil "--ast" "Print Intermediate AST."]
                      ["-h" "--help" "Print Help"]])
(defn -main [& args]
  (let [args (parse-opts args program-options)
        {:keys [help input watch-input]} (:options args)]

    (when help
      (try
        (let [version (read-file "build.info")]
          (print "ferret-lisp build:" version))
        (catch Exception e
          (print "ferret-lisp")))
      (println )
      (println )
      (println (:summary args))
      (System/exit 0))

    (when (not (file-exists input))
      (warn "no input file")
      (System/exit 1))

    (let [specs (build-specs input args)]
      (if (not watch-input)
        (build-solution specs)
        (do (watcher/watcher [input]
                             (watcher/rate 1000)
                             (watcher/on-change
                              (fn [_] (build-solution specs))))
            @(promise)))
      (shutdown-agents))))
