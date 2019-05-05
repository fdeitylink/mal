(ns mal.step1-read-print
  (:require [mal reader printer])
  (:gen-class))

(defn READ
  [str]
  (mal.reader/read-str str))

(defn EVAL
  [str]
  str)

(defn PRINT
  [str]
  (mal.printer/print-str str))

(defn rep
  [str]
  (PRINT (EVAL (READ str))))

(defn -main
  [& args]
  (print "user> ")
  (flush)
  (loop [line (read-line)]
    (when line
      (println (rep line))
      (print "user> ")
      (flush)
      (recur (read-line)))))
