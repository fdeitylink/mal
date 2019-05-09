(ns mal.env)

(declare env-set)

(defn env
  ([] (env nil '() '()))
  ([outer] (env outer '() '()))
  ([binds exprs] (env nil binds exprs))
  ([outer binds exprs]
   (if (= (count binds) (count exprs))
     (let [env (atom {:outer outer :data {}})]
       (doall (map #(env-set env %1 %2) binds exprs))
       env)
     (throw (Exception. "Number of symbols and values to bind do not match")))))

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
    (throw (Exception. (str "Could not resolve symbol '" k "'")))))
