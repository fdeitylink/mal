(ns mal.step7-quote
  (:require [mal reader printer env core])
  (:gen-class))

(defn READ
  [str]
  (mal.reader/read-str str))

(declare EVAL)

(defn eval-ast
  [ast env]
  (cond
    (symbol? ast) (mal.env/env-get env ast)
    (seq? ast) (doall (map #(EVAL % env) ast))
    (vector? ast) (vec (doall (map #(EVAL % env) ast)))
    (map? ast) (let [evmap (into {} (doall (map (fn [[k v]] [k (EVAL v env)]) ast)))]
                 (if-let [k (some #(and (not (or (keyword? %) (string? %))) %) (keys evmap))]
                   (throw (Exception. (str "Map key '" k "' is not a string or keyword")))
                   evmap))
    :else ast))

(defn is-pair
  [x]
  (and (sequential? x) (boolean (seq x))))

(defn quasiquote
  [ast]
  (cond
    (not (is-pair ast)) (list 'quote ast)
    (= 'unquote (first ast)) (second ast)
    (and (is-pair (first ast)) (= 'splice-unquote (ffirst ast))) (list 'concat
                                                                       (second (first ast))
                                                                       (quasiquote (rest ast)))
    :else (list 'cons (quasiquote (first ast)) (quasiquote (rest ast)))))

(defn EVAL
  [ast env]
  (cond
    (not (seq? ast)) (eval-ast ast env)
    (empty? ast) ast
    :else (let [[fst snd thrd frth] ast]
            (condp = fst
              'def! (if (= 3 (count ast))
                      (mal.env/env-set env snd (EVAL thrd env))
                      (throw (Exception. "def! needs a symbol and value")))
              'quote (if (= 2 (count ast))
                        snd
                        (throw (Exception. "quote needs a single form")))
              'quasiquote (recur (quasiquote snd) env)
              'let* (if (even? (count snd))
                      (let [let-env (mal.env/env env)]
                        (doseq [[k v] (partition 2 snd)]
                          (mal.env/env-set let-env k (EVAL v let-env)))
                        (recur thrd let-env))
                      (throw (Exception. "Odd number of terms in let* bindings")))
              'do (do
                    (eval-ast (apply list (butlast (rest ast))) env)
                    (recur (last ast) env))
              'if (if (<= 3 (count ast) 4)
                    (recur (if (EVAL snd env) thrd frth) env)
                    (throw (Exception. "if-form needs test form, then form, and optional else form")))
              'fn* (with-meta
                     (fn [& args] (EVAL thrd (mal.env/env env snd (or args '()))))
                     {:fn-ast thrd
                      :params snd
                      :fn-env env})
              (let [[f & args] (eval-ast ast env)
                    {:keys [fn-ast params fn-env]} (meta f)]
                (if fn-ast
                  (recur fn-ast (mal.env/env fn-env params args))
                  (apply f args)))))))

(defn PRINT
  [exp]
  (mal.printer/pr-str exp))

(def repl-env (mal.env/env))

(defn rep
  [str]
  (PRINT (EVAL (READ str) repl-env)))

(doseq [[k v] mal.core/core-ns] (mal.env/env-set repl-env k v))
(mal.env/env-set repl-env 'eval #(EVAL % repl-env))

(rep "(def! not (fn* (bool) (if bool false true)))")
(rep "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \")\")))))")

(defn repl
  []
  (print "user> ")
  (flush)
  (loop [line (read-line)]
    (when line
      (when-not (re-seq #"^\s*(;.*)?$" line)
        (try
          (println (rep line))
          (catch Throwable e
            (println (str (.getSimpleName (class e)) ": " (.getMessage e))))))
      (print "user> ")
      (flush)
      (recur (read-line)))))

(defn -main
  [& args]
  (mal.env/env-set repl-env '*ARGV* (apply list (rest args)))
  (if (empty? args)
    (repl)
    (rep (str "(load-file \"" (first args) "\")"))))
