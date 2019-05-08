(ns mal.step2-eval
  (:require [mal reader printer])
  (:gen-class))

(def repl-env
  {'+ #(+ %1 %2)
   '- #(- %1 %2)
   '* #(* %1 %2)
   '/ #(int (/ %1 %2))})

(declare EVAL)

(defn eval-ast
  [ast env]
  (cond
    (symbol? ast) (if-let [val (get env ast)] val (throw (Exception. (str "Could not resolve symbol " ast))))
    (list? ast) (map #(EVAL % env) ast)
    :else ast))

(defn READ
  [str]
  (mal.reader/read-str str))

(defn EVAL
  [ast env]
  (cond
    (not (list? ast)) (eval-ast ast env)
    (empty? ast) ast
    :else (let [[f & args] (eval-ast ast env)] (apply f args))))

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
