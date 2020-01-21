(ns anseml.sax.schema
  (:require [anseml.common.schema :as c]
            [anseml.sax.parser :refer :all]))

(def ^:private scm-string?
  (fn [[x & xs]]
    (if (and (= :elem-primitive (x 0)) (string? (x 1)))
      (list xs))))

(def ^:private scm-integer?
  (fn [[x & xs]]
    (if (and (= :elem-primitive (x 0)) (integer? (x 1)))
      (list xs))))

(def ^:private scm-float?
  (fn [[x & xs]]
    (if (and (= :elem-primitive (x 0)) (float? (x 1)))
      (list xs))))

(def ^:private scm-check-attr
  (fn [attr predicate]
    (fn [[x & xs]]
      (if (and ((x 2) attr)
               (predicate (list ((x 2) attr))))
        (list xs)))))

(def ^:private scm-check-element
  (fn [tag attrs-pred value-pred]
    (fn [[x & xs]]
      (and (= :elem-started (x 0))
           (= tag (x 1))
           (or (not attrs-pred)
               (attrs-pred (list x)))
           (or (not value-pred)
               (let [xs (value-pred xs)
                     res (mapcat (fn [[x & xs]]
                                   (if (= :elem-ended (x 0))
                                     (list xs))) xs)]
                 (seq res)))))))

(def ^:private scm-get-tag
  (fn [root]
    ((first root) 1)))


(defn build-schema [schema]
  (binding [c/scm-string? scm-string?
            c/scm-integer? scm-integer?
            c/scm-float? scm-float?
            c/scm-check-attr scm-check-attr
            c/scm-check-element scm-check-element
            c/scm-get-tag scm-get-tag]
    (c/build-schema schema)))

(defn validate [schema root]
  (binding [c/scm-string? scm-string?
            c/scm-integer? scm-integer?
            c/scm-float? scm-float?
            c/scm-check-attr scm-check-attr
            c/scm-check-element scm-check-element
            c/scm-get-tag scm-get-tag]
    (c/validate schema root)))

;(let [schema (build-schema (create-element
;                             '(:scm-schema
;                                (:scm-attribute {:tag "width"}
;                                  (:scm-alternative
;                                    :scm-string
;                                    :scm-integer))
;                                (:scm-attribute {:tag "height"}
;                                  :scm-integer)
;                                (:scm-element {:tag "inner"}
;                                  :scm-string)
;                                (:scm-element {:tag "root"}
;                                  (:scm-sequence
;                                    :scm-string
;                                    :inner)
;                                  (:scm-attribute
;                                    :width
;                                    :height)))))]
;  (println (validate schema (parse-lazy-seq
;                              (BufferedReader.
;                                (StringReader.
;                                  "(:root {:width \"1\" :height 2}\n \"bla-bla\"\n (:inner\n \"bla-bla\"))"))))))
