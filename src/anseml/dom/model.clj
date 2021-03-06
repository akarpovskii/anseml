(ns anseml.dom.model)

(defn- single-element?
  "Checks whether the collection contains a single element."
  [coll]
  (= 1 (bounded-count 2 coll)))

(defn- element-desc?
  "Checks whether the argument is a valid element description.
  Does not do a deep introspection: checks only main parts."
  [desc]
  (or (keyword? desc)
      (and (seq? desc)
           (keyword? (first desc)))))

(defn- supported-primitive?
  "Checks whether the value type is supported.
  Currently supported types: string, integer, float."
  [value]
  (let [supported-primitives (list string? integer? float?)]
    ((apply some-fn supported-primitives) value)))

(declare create-element)

(defn- process-element-value
  "Recursively builds the value of an element."
  [value]
  {:pre [(seq? value)]}
  (apply list
         (map (fn [v]
                (cond (element-desc? v) (create-element v)
                      (supported-primitive? v) v
                      :else (throw (IllegalArgumentException. (str "Unsupported primitive \"" v "\".")))))
              value)))

(defn- valid-attrs? [attrs]
  (every? (fn [[k v]]
            (and (keyword? k) (supported-primitive? v)))
          (if (map? attrs) attrs (apply array-map attrs))))

(defn create-element [desc]
  (let [desc (if (keyword? desc) (list desc) desc)
        [name attrs & value] desc
        has-attrs? (and (map? attrs) (valid-attrs? attrs))
        value (if has-attrs? value (conj value attrs))
        value (cond (empty? value) ()
                    (single-element? value) (if (nil? (first value))
                                              ()
                                              value)
                    :else value)
        attrs (if has-attrs? attrs {})]
    (if-not (keyword? name)
      (throw (IllegalArgumentException. "The document must contain the only root element.")))
    (array-map
      ::tag name
      ::attrs attrs
      ::value (process-element-value value))))


; public functions

(defn element? [elem]
  (and (map? elem)
       (keyword? (::tag elem))
       (map? (::attrs elem))
       (list? (::value elem))))

(defn get-tag [elem]
  {:pre [(element? elem)]}
  (::tag elem))

(defn set-tag [elem tag]
  {:pre [(element? elem)
         (keyword? tag)]}
  (with-meta (assoc elem ::tag tag) elem))

(defn get-attrs
  ([elem]
   {:pre [(element? elem)]}
   (::attrs elem))
  ([elem attr]
   {:pre [(element? elem)
          (keyword? attr)]}
   (attr (get-attrs elem)))
  ([elem attr & attrs]
   {:pre [(element? elem)
          (keyword? attr)]}
   (select-keys (get-attrs elem) (cons attr attrs))))

(defn set-attrs
  ([elem attrs]
   {:pre [(element? elem)
          (map? attrs)]}
   (if-not (valid-attrs? attrs)
     (throw (IllegalArgumentException. "Attributes contains unsupported types.")))
   (with-meta (assoc elem ::attrs attrs) (meta elem)))
  ([elem key val & kvs]
   {:pre [(element? elem)]}
   (if-not (and (valid-attrs? (list key val))
                (valid-attrs? kvs))
     (throw (IllegalArgumentException. "Attributes contains unsupported types.")))
   (with-meta (assoc elem ::attrs (apply hash-map key val kvs)) (meta elem))))

(defn add-attrs
  ([elem attrs]
   {:pre [(element? elem)
          (map? attrs)]}
   (with-meta (set-attrs elem (merge (get-attrs elem) attrs)) (meta elem)))
  ([elem key val & kvs]
   {:pre [(element? elem)]}
   (with-meta (set-attrs elem (apply assoc (get-attrs elem) key val kvs)) (meta elem))))

(defn remove-attrs
  ([elem attr]
   {:pre [(element? elem)
          (keyword? attr)]}
   (with-meta (set-attrs elem (dissoc (get-attrs elem) attr)) (meta elem)))
  ([elem attr & attrs]
   {:pre [(element? elem)
          (apply every? keyword? attr attrs)]}
   (with-meta (set-attrs elem (apply dissoc (get-attrs elem) attr attrs)) (meta elem))))

(defn get-value [elem]
  {:pre [(element? elem)]}
  (::value elem))

(defn set-value [elem value]
  {:pre [(element? elem)]}
  (letfn [(check-value [value]
            (cond (element? value) true
                  (supported-primitive? value) true
                  (list? value) (every? (some-fn element? supported-primitive?) value)
                  :else false))]
    (if-not (check-value value)
      (throw (IllegalArgumentException. "Value contains unsupported types."))))
  (with-meta (assoc elem ::value (if (list? value)
                                   value
                                   (list value))) (meta elem)))
