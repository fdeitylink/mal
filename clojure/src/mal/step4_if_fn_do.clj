(ns mal.step4-if-fn-do
  (:require [mal reader printer env core])
  (:gen-class))

(def repl-env (mal.env/env))

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

(defn READ
  [str]
  (mal.reader/read-str str))

(defn EVAL
  [ast env]
  (cond
    (not (list? ast)) (eval-ast ast env)
    (empty? ast) ast
    :else (let [[fst snd thrd frth] ast]
            (condp = fst
              'def! (mal.env/env-set env snd (EVAL thrd env))
              'let* (if (even? (count snd))
                      (let [let-env (mal.env/env env)]
                        (doseq [[k v] (partition 2 snd)]
                          (mal.env/env-set let-env k (EVAL v let-env)))
                        (EVAL thrd let-env))
                      (throw (Exception. "Odd number of terms in let* bindings")))
              'do (last (eval-ast (rest ast) env))
              'if (if (>= (count ast) 3)
                    (if (<= (count ast) 4)
                      (if (EVAL snd env)
                        (EVAL thrd env)
                        (EVAL frth env))
                      (throw (Exception. "Extraneous forms in if-form")))
                    (throw (Exception. "Missing then-form in if-form")))
              'fn* (fn [& args]
                     (EVAL thrd (mal.env/env env snd (or args '()))))
              (let [[f & args] (eval-ast ast env)] (apply f args))))))

(defn PRINT
  [exp]
  (mal.printer/pr-str exp))

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
      (try
        (println (rep line))
        (catch Exception e
          (println (.getMessage e))))
      (print "user> ")
      (flush)
      (recur (read-line)))))
