(ns mal.step0-repl
  (:gen-class))

(defn READ
  [str]
  str)

(defn EVAL
  [str]
  str)

(defn PRINT
  [str]
  str)

(defn rep
  [str]
  (PRINT (EVAL (READ str))))

(defn -main
  [& args]
  (println "user> ")
  (loop [line (read-line)]
    (when line
      (println (rep line))
      (println "user> ")
      (recur (read-line)))))
