(ns mal.core
  (:require [mal reader printer]
            [clojure.string :as s]))

(def core-ns
  {'nil? nil?
   'true? true?
   'false? false?

   'string? string?
   'number? number?

   'symbol symbol
   'symbol? symbol?

   'keyword keyword
   'keyword? keyword?

   '+ +
   '- -
   '* *
   '/ #(int (apply / %&))
   'mod mod

   '= =
   '< <
   '<= <=
   '> >
   '>= >=

   'list list
   'list? seq?

   'vector vector
   'vector? vector?

   'seq #(seq (if (string? %) (map str %) %))

   'sequential? sequential?

   'hash-map hash-map
   'map? map?

   'empty? empty?
   'count count
   'cons cons
   'conj conj
   'concat concat
   'assoc assoc
   'dissoc dissoc
   'contains? contains?
   'nth nth
   'get get
   'first first
   'rest rest
   'keys #(apply list (keys %))
   'vals #(apply list (vals %))

   'map #(apply list (map %1 %2))

   'fn? #(and (fn? %) (not (:is-macro (meta %))))
   'macro? #(and (fn? %) (:is-macro (meta %)))

   'apply apply

   'read-string mal.reader/read-str

   'meta #(:mal-meta (meta %))
   'with-meta #(with-meta %1 (assoc (meta %1) :mal-meta %2))

   'pr-str #(s/join " " (map mal.printer/pr-str %&))
   'str #(apply str (map (fn [form] (mal.printer/pr-str form false)) %&))
   'prn #(println (s/join " " (map mal.printer/pr-str %&)))
   'println #(println (s/join " " (map (fn [form] (mal.printer/pr-str form false)) %&)))

   'throw #(throw (ex-info "mal exception" {:data %}))

   'atom atom
   'atom? (partial instance? clojure.lang.Atom)
   'deref deref
   'reset! reset!
   'swap! swap!

   'slurp slurp

   'readline #(do (println %) (read-line))

   'time-ms #(System/currentTimeMillis)})
