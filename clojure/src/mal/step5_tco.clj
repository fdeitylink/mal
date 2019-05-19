(ns mal.step5-tco
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
    (list? ast) (apply list (doall (map #(EVAL % env) ast)))
    (vector? ast) (vec (doall (map #(EVAL % env) ast)))
    (map? ast) (let [evmap (into {} (doall (map (fn [[k v]] [k (EVAL v env)]) ast)))]
                 (if-let [k (some #(and (not (or (keyword? %) (string? %))) %) (keys evmap))]
                   (throw (Exception. (str "Map key '" k "' is not a string or keyword")))
                   evmap))
    :else ast))

(defn EVAL
  [ast env]
  (cond
    (not (list? ast)) (eval-ast ast env)
    (empty? ast) ast
    :else (let [[fst snd thrd frth] ast]
            (condp = fst
              'def! (if (= 3 (count ast))
                      (mal.env/env-set env snd (EVAL thrd env))
                      (throw (Exception. "def!-form must have symbol and value")))
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
                    (throw (Exception. "if-form must have test form, then form, and optional else form")))
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
(rep "(def! not (fn* (bool) (if bool false true)))")

(defn -main
  [& args]
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
