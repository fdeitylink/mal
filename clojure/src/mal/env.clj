(ns mal.env)

(declare env-set)

(defn env
  ([] (env nil '() '()))
  ([outer] (env outer '() '()))
  ([binds exprs] (env nil binds exprs))
  ([outer binds exprs]
   (if (or (= (count binds) (count exprs))
           (and (<= (- (count binds) 2) (count exprs))
                (= (count (drop-while #(not= % '&) binds)) 2)))
     (let [env (atom {:outer outer :data {}})]
       (loop [b binds
              e (apply list exprs)]
         (cond
           (empty? b) env
           (= '& (first b)) (do (env-set env (second b) e) env)
           :else (do
                   (env-set env (first b) (first e))
                   (recur (rest b) (rest e))))))
     (throw (Exception. "Binding symbol and expression counts do not match and no rest arg specified")))))

(defn env-set
  [env k v]
  (if (symbol? k)
    (do (swap! env #(assoc-in % [:data k] v)) v)
    (throw (Exception. (str "'" k "' is not a symbol")))))

(defn env-lookup
  [env k]
  (let [{:keys [outer data]} @env]
    (if-let [entry (find data k)]
      entry
      (if outer
        (recur outer k)))))

(defn env-find
  [env k]
  (if-let [entry (env-lookup env k)]
    (val entry)))

(defn env-get
  [env k]
  (if-let [entry (env-lookup env k)]
    (val entry)
    (throw (Exception. (str "'" k "' not found")))))
