(ns mal.core
  (:require [mal reader printer]
            [clojure.string :as s]))

(def core-ns
  {'+ +
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
   'empty? empty?
   'count count
   'cons cons
   'concat concat

   'pr-str #(s/join " " (map mal.printer/pr-str %&))
   'str #(apply str (map (fn [form] (mal.printer/pr-str form false)) %&))
   'prn #(println (s/join " " (map mal.printer/pr-str %&)))
   'println #(println (s/join " " (map (fn [form] (mal.printer/pr-str form false)) %&)))

   'read-string mal.reader/read-str

   'slurp slurp

   'atom atom
   'atom? (partial instance? clojure.lang.Atom)
   'deref deref
   'reset! reset!
   'swap! swap!})
