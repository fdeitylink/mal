(ns mal.core
  (:require [mal printer]
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
   'list? list?
   'empty? empty?
   'count count

   'pr-str #(s/join " " (map mal.printer/pr-str %&))
   'str #(apply str (map (fn [form] (mal.printer/pr-str form false)) %&))
   'prn #(println (s/join " " (map mal.printer/pr-str %&)))
   'println #(println (s/join " " (map (fn [form] (mal.printer/pr-str form false)) %&)))})
