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
  (mal.printer/pr-str str))

(defn rep
  [str]
  (PRINT (EVAL (READ str))))

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
