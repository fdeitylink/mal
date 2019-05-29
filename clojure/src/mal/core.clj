(ns mal.core
  (:require [mal reader printer]
            [clojure.string :as s]))

(def core-ns
  {'nil? nil?
   'true? true?
   'false? false?

   'symbol symbol
   'symbol? symbol?

   'keyword keyword
   'keyword? keyword?

   '+ +
   '- -
   '* *
   '/ #(int (apply / %&))

   '= =
   '< <
   '<= <=
   '> >
   '>= >=

   'list list
   'list? seq?

   'vector vector
   'vector? vector?

   'sequential? sequential?

   'hash-map hash-map
   'map? map?

   'empty? empty?
   'count count
   'cons cons
   'concat concat
   'assoc assoc
   'dissoc dissoc
   'nth nth
   'get get
   'first first
   'rest rest
   'keys #(apply list (keys %))
   'vals #(apply list (vals %))

   'map #(apply list (map %1 %2))

   'apply apply

   'read-string mal.reader/read-str

   'pr-str #(s/join " " (map mal.printer/pr-str %&))
   'str #(apply str (map (fn [form] (mal.printer/pr-str form false)) %&))
   'prn #(println (s/join " " (map mal.printer/pr-str %&)))
   'println #(println (s/join " " (map (fn [form] (mal.printer/pr-str form false)) %&)))

   'slurp slurp

   'atom atom
   'atom? (partial instance? clojure.lang.Atom)
   'deref deref
   'reset! reset!
   'swap! swap!

   'throw #(throw (ex-info "mal exception" {:data %}))})
