(ns mal.step3-env
  (:require [mal reader printer env])
  (:gen-class))

(def repl-env (mal.env/env))
(mal.env/env-set repl-env '+ #(+ %1 %2))
(mal.env/env-set repl-env '- #(- %1 %2))
(mal.env/env-set repl-env '* #(* %1 %2))
(mal.env/env-set repl-env '/ #(int (/ %1 %2)))

(declare EVAL)

(defn eval-ast
  [ast env]
  (cond
    (symbol? ast) (mal.env/env-get env ast)
    (list? ast) (map #(EVAL % env) ast)
    (vector? ast) (vec (map #(EVAL % env) ast))
    (map? ast) (let [evmap (into {} (map (fn [[k v]] [k (EVAL v env)]) ast))]
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
    :else (let [[fst snd thrd] ast]
            (condp = fst
              'def! (mal.env/env-set env snd (EVAL thrd env))
              'let* (if (odd? (count snd))
                      (throw (Exception. "Odd number of terms in let* bindings"))
                      (let [let-env (mal.env/env env)]
                        (doseq [[k v] (partition 2 snd)]
                          (mal.env/env-set let-env k (EVAL v let-env)))
                        (EVAL thrd let-env)))
              (let [[f & args] (eval-ast ast env)] (apply f args))))))

(defn PRINT
  [str]
  (mal.printer/pr-str str))

(defn rep
  [str]
  (PRINT (EVAL (READ str) repl-env)))

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
