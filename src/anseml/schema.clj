(ns anseml.schema
  (:require [anseml.model :refer :all]
            [anseml.common :refer :all]))

; scm-schema
; scm-string, scm-float, scm-integer
; scm-element, scm-attribute
; scm-alternative, scm-sequence, scm-all, scm-attribute

(defn- scm-string? [[x & xs]]
  ;(println "string " x " | " xs)
  (if (string? x)
    (list xs)))

(defn- scm-integer? [[x & xs]]
  ;(println "integer " x " | " xs)
  (if (integer? x)
    (list xs)))

(defn- scm-float? [[x & xs]]
  ;(println "float " x " | " xs)
  (if (float? x)
    (list xs)))

(def ^:private primitives
  {:scm-string  scm-string?
   :scm-integer scm-integer?
   :scm-float   scm-float?})

(defn- primitive? [v]
  (primitives v))

(defn- error [msg & msgs]
  (throw (IllegalArgumentException. (str (apply str msg msgs)))))

(declare build-sequence)
(declare build-alternative)

(defn- build-sub-predicate [schema predicates]
  (case (get-tag schema)
    :scm-sequence (build-sequence schema predicates)
    :scm-alternative (build-alternative schema predicates)
    (or ((get-tag schema) predicates) (error "Undefined predicate meet in description " schema "."))))

(defn- build-alternative [schema predicates]
  {:pre [(= :scm-alternative (get-tag schema))]}
  (reduce (fn [pred v]
            (let [p (build-sub-predicate v predicates)]
              #(let [res1 (pred %)
                     res2 (p %)]
                 (seq (concat res1 res2)))))
          (constantly nil)
          (get-value schema)))

(defn- build-sequence [schema predicates]
  {:pre [(= :scm-sequence (get-tag schema))]}
  (let [ps (map #(build-sub-predicate % predicates)
                (get-value schema))]
    (fn [value]
      (reduce (fn [values p]
                (let [res (mapcat p values)]
                  (seq res)))
              (list value)
              ps)
      )))

(defn- build-attribute [schema predicates]
  {:pre [(= :scm-attribute (get-tag schema))]}
  (let [ps (map #(or ((get-tag %) predicates)
                     (error "Undefined predicate meet in description " schema "."))
                (get-value schema))]
    (fn [[_ & xs :as v]]
      (if (every? #(% v) ps)
        (list xs)))))

; TODO: Allow empty attribute definitions? Maybe make a default predicate, something like "not-nil"?
(defn- build-def-attribute [schema predicates]
  {:pre [(= :scm-attribute (get-tag schema))]}
  (let [tag (get-attrs schema :tag)]
    (if-not (and tag (string? tag))
      (error "Definition must contain a :tag <string> attribute.")))
  (let [value (get-value schema)]
    (if (or (empty? value) (next value))
      (error "Attribute definition must contain single value")))
  (let [attr (keyword (get-attrs schema :tag))
        value (first (get-value schema))
        value-tag (get-tag value)
        pred (cond
               (= :scm-alternative value-tag) (if (every? (comp primitive? get-tag) (get-value value))
                                                (build-alternative value predicates)
                                                (error "Unsupported value meet in the attribute definition " schema "."))
               (primitive? value-tag) (value-tag primitives)
               :else (error "Unsupported value meet in the attribute definition " schema "."))]
    (fn [[x & xs]]
      (if (and (element? x)
               (get-attrs x attr)
               (pred (list (get-attrs x attr))))
        (list xs)))))

(defn- build-def-element [schema attributes elements]
  {:pre [(= :scm-element (get-tag schema))]}
  (let [tag (get-attrs schema :tag)]
    (if-not (and tag (string? tag))
      (error "Definition must contain a :tag <string> attribute.")))
  (let [value (get-value schema)]
    (if (or (nnext value))
      (error "Element definition may contain no more than two values: one for value and one for attributes")))
  (let [tag (keyword (get-attrs schema :tag))
        value (get-value schema)
        attrs (first value)
        value (second value)
        [attrs value] (cond
                        value (if (= :scm-attribute (get-tag value))
                                [value attrs]
                                [attrs value])
                        attrs (if (= :scm-attribute (get-tag attrs))
                                [attrs value]
                                [value attrs])
                        :else [attrs value])
        attrs (and attrs (build-attribute attrs attributes))
        value-tag (and value (get-tag value))
        value (and value
                   (cond
                     (= :scm-alternative value-tag) (build-alternative value elements)
                     (= :scm-sequence value-tag) (build-sequence value elements)
                     (primitive? value-tag) (value-tag primitives)
                     :else (or (value-tag elements) (error "Undefined predicate meet in description " schema "."))))
        value (and value (comp value get-value))]           ;NB! value accepts a single element
    (fn [[x & xs]]
      (if (and (element? x)
               (= tag (get-tag x))
               (or (not attrs)
                   (attrs (list x)))
               (or (not value)
                   (value x)))
        (list xs)))))

(defn build-schema [schema]
  {:pre [(= :scm-schema (get-tag schema))]}
  (let [attributes (atom {})
        elements (atom {})]
    (doseq [def (get-value schema)]
      (let [tag (get-tag def)
            type (keyword (get-attrs def :tag))]
        (case tag
          :scm-attribute (if (type @attributes)
                           (error "Attribute redefinition " schema ".")
                           (swap! attributes assoc type (build-def-attribute def primitives)))
          :scm-element (if (type @elements)
                         (error "Element redefinition " schema ".")
                         (swap! elements assoc type (build-def-element def @attributes @elements)))
          (error "Expected element or attribute definition, got " def "."))))
    {::schema true
     :attributes @attributes
     :elements   @elements}))

(defn schema? [schema]
  (::schema schema))

(defn validate [schema root]
  {:pre [(schema? schema)
         (element? root)]}
  (let [tag (get-tag root)
        pred (tag (:elements schema))]
    (= '(nil)
       (and pred
            (pred [root])))))

;(let [schema (build-schema (create-element
;                             '(:scm-schema
;                                (:scm-attribute {:tag "width"}
;                                  :scm-string)
;                                (:scm-attribute {:tag "height"}
;                                  :scm-integer)
;                                (:scm-element {:tag "root"}
;                                  :scm-string
;                                  (:scm-attribute
;                                    :width
;                                    :height)))))]
;  (println (validate schema (create-element
;                              '(:root {:width  "1"
;                                       :height 2}
;                                 "bla-bla")))))
;
;(let [width (build-def-attribute (create-element
;                                  '(:scm-attribute {:tag "width"}
;                                     :scm-string))
;                                primitives)
;      height (build-def-attribute (create-element
;                                    '(:scm-attribute {:tag "height"}
;                                       :scm-integer))
;                                  primitives)
;      pred (build-attribute (create-element '(:scm-attribute
;                                               :width
;                                               :height))
;                            {:width width :height height})]
;  (println (pred [(create-element '(:root {:width "1"
;                                           :height 2}))])))
;
;(let [pred (build-sequence (create-element '(:scm-sequence
;                                              (:scm-alternative
;                                                (:scm-sequence
;                                                  (:scm-integer)
;                                                  (:scm-float))
;                                                (:scm-string))
;                                              (:scm-sequence
;                                                (:scm-float)
;                                                (:scm-integer))))
;                           {:scm-integer scm-integer?
;                            :scm-float   scm-float?
;                            :scm-string  scm-string?})]
;  (println (pred ["bla-bla" 2.1 5])))


;(def schema-example
;  (create-element
;    '(:scm-schema
;       (:scm-element {:tag "name"}
;         :scm-string)
;
;       (:scm-element {:tag "number"}
;         (:scm-alternative
;           :scm-float
;           :scm-integer))
;
;       (:scm-attribute {:tag "time"}
;         :scm-string)
;
;       (:scm-attribute {:tag "id"}
;         :scm-integer)
;
;       (:scm-element {:tag "dict"}
;         (:scm-sequence
;           :name
;           :number)
;         (:scm-attribute
;           :id
;           :time)))))
;
;(def elem (create-element '(:dict {:id   42,
;                                   :time "time"}
;                             (:name "name")
;                             (:number 42.0))))
;
;(def ex-name (create-element '(:name "name")))
;
;(def ex-number (create-element '(:number 42.0)))
;
;(let [scm ((build-schema schema-example) 0)]
;  (println ((:dict scm) elem)))
;
;
