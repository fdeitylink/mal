(ns mal.reader)

(def tokenize-regex #"[\s,]*(~@|[\[\]{}()'`~^@]|\"(?:\\.|[^\\\"])*\"?|;.*|[^\s\[\]{}('\"`,;)]*)")
(def int-regex #"^-?\d+$")

(defn reader
  [tokens]
  {:tokens (vec tokens) :pos (atom 0)})

(defn reader-peek
  [{:keys [tokens pos]}]
  (get tokens @pos))

(defn reader-next
  [{:keys [tokens pos]}]
  (let [p @pos]
    (swap! pos inc)
    (get tokens p)))

(defn tokenize
  [str]
  (map second (re-seq tokenize-regex str)))

(declare read-form)

(defn read-list
  [rdr]
  (assert (= "(" (reader-next rdr)))
  (loop [lst []]
    (let [tok (reader-peek rdr)]
      (if (= tok ")")
        (do (reader-next rdr) lst)
        (recur (conj lst (read-form rdr)))))))

(defn read-atom
  [rdr]
  (let [tok (reader-next rdr)]
    (cond
      (re-seq int-regex tok) (Integer/parseInt tok)
      :else (symbol tok))))

(defn read-form
  [rdr]
  (let [tok (reader-peek rdr)]
    (if (= tok "(")
      (read-list rdr)
      (read-atom rdr))))

(defn read-str
  [str]
  (read-form (reader (tokenize str))))
