(ns mal.reader)

(def tokenize-regex #"[\s,]*(~@|[\[\]{}()'`~^@]|\"(?:\\.|[^\\\"])*\"?|;.*|[^\s\[\]{}('\"`,;)]*)")

(def int-regex #"^-?\d+$")
(def str-regex #"^\"((?:[\\].|[^\\\"])*)\"$")
(def bad-str-regex #"^\"")

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
  (filter #(not= \; (first %)) (map second (re-seq tokenize-regex str))))

(defn unescape
  [string]
  (loop [fst (first string)
         remaining (rest string)
         result ""]
    (condp = fst
      nil result
      \\ (recur (second remaining)
                (drop 2 remaining)
                (str result (condp = (first remaining)
                              \\ \\
                              \" \"
                              \n \newline
                              (throw (Exception. "'\\' must be followed with '\\', '\"', or 'n'")))))
      (recur (first remaining) (rest remaining) (str result fst)))))

(declare read-form)

(defn read-seq
  [rdr start-tok end-tok]
  (assert (= start-tok (reader-next rdr)))
  (loop [lst []]
    (let [tok (reader-peek rdr)]
      (condp = tok
        end-tok (do (reader-next rdr) lst)
        nil (throw (Exception. (str "Expected '" end-tok "', reached end of input")))
        (recur (conj lst (read-form rdr)))))))

(defn read-atom
  [rdr]
  (let [tok (reader-next rdr)]
    (cond
      (re-seq int-regex tok) (Integer/parseInt tok)

      (re-seq str-regex tok) (unescape (second (re-find str-regex tok)))
      (re-seq bad-str-regex tok) (throw (Exception. (str "Expected '\"', reached end of input")))

      (= \: (first tok)) (keyword (subs tok 1))

      (= "nil" tok) nil
      (= "true" tok) true
      (= "false" tok) false

      :else (symbol tok))))

(defn read-form
  [rdr]
  (let [tok (reader-peek rdr)]
    (condp = tok
      "'" (do (reader-next rdr) (list 'quote (read-form rdr)))
      "`" (do (reader-next rdr) (list 'quasiquote (read-form rdr)))
      "~" (do (reader-next rdr) (list 'unquote (read-form rdr)))
      "~@" (do (reader-next rdr) (list 'splice-unquote (read-form rdr)))

      "^" (do (reader-next rdr) (let [map (read-form rdr)]
                                  (list 'with-meta (read-form rdr) map)))

      "@" (do (reader-next rdr) (list 'deref (read-form rdr)))

      "(" (apply list (read-seq rdr "(" ")"))
      ")" (throw (Exception. "Unexpected ')'"))

      "[" (vec (read-seq rdr "[" "]"))
      "]" (throw (Exception. "Unexpected ']'"))

      "{" (apply hash-map (read-seq rdr "{" "}"))
      "}" (throw (Exception. "Unexpected '}'"))

      (read-atom rdr))))

(defn read-str
  [str]
  (read-form (reader (tokenize str))))
