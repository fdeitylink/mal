(ns mal.printer
  (:refer-clojure :exclude [pr-str])
  (:require [clojure.string :as s]))

(defn escape
  [string]
  (-> string
      (s/replace "\n" "\\n")
      (s/replace "\"" "\\\"")
      (s/replace "\\" "\\\\")))

(declare pr-str)

(defn pr-seq
  [form print-readable start-tok end-tok]
  (str start-tok (clojure.string/join " " (map #(pr-str % print-readable) form)) end-tok))

(defn pr-str
  ([form] (pr-str form true))
  ([form print-readable]
   (cond
     (nil? form) "nil"

     (string? form) (if print-readable (str "\"" (escape form) "\"") form)

     (list? form) (pr-seq form print-readable "(" ")")
     (vector? form) (pr-seq form print-readable "[" "]")

     (map? form) (pr-seq (flatten (seq form)) print-readable "{" "}")

     (fn? form) "#<function>"

     :else (str form))))
