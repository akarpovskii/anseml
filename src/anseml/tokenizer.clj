(ns anseml.tokenizer)

(defn- tkn-parenthesis [input]
  (some-> (case (first input)
            \( :open-paren
            \) :close-paren
            \{ :open-curly
            \} :close-curly
            nil)
          (vector (str (first input)))))

(defn- tkn-whitespace [input]
  (if-let [m (re-find #"^[ \t\n]+" input)]
    [:whitespace m]))

(defn- tkn-integer [input]
  (if-let [m (re-find #"^[-+]?[0-9]+" input)]
    [:integer m]))

(defn- tkn-float [input]
  (if-let [m (re-find #"^[-+]?[0-9]*\.[0-9]+([eE][-+]?[0-9]+)?" input)]
    [:float (first m)]))

(defn- tkn-string [input]
  (if-let [m (re-find #"^\"[^\"]*\"" input)]
    [:string m]))

; See https://clojure.org/reference/reader#_symbols and https://clojure.org/reference/reader#_literals
(defn- tkn-keyword [input]
  (if-let [m (re-find #"^:[\w\*\+\!\-\'\?\<\>\=\/\:]+" input)]
    [:keyword m]))

(defn tokenize [input]
  (if (seq input)
    (if-let [[type val] (or (tkn-parenthesis input)
                            (tkn-keyword input)
                            (tkn-float input)
                            (tkn-integer input)
                            (tkn-string input)
                            (tkn-whitespace input))]
      (lazy-seq (cons [type val] (tokenize (subs input (count val)))))
      (throw (ex-info "Unexpected token" {:input input})))))
