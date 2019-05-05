(ns mal.printer)

(defn print-str
  [form]
  (cond
    (symbol? form) (str form)
    (number? form) (str form)
    (vector? form) (str "(" (clojure.string/join " " (map pr-str form)) ")")))
