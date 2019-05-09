(ns mal.core
  (:require [mal printer]))

(def core-ns
  {'+ +
   '- -
   '* *
   '/ #(int (apply / %&))
   'prn #(println (mal.printer/pr-str %))
   'list list
   'list? list?
   'empty? empty?
   'count count
   '= =
   '< <
   '<= <=
   '> >
   '>= >=})
