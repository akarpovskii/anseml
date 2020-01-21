(ns anseml.dom.schema
  (:require [anseml.dom.model :refer :all]
            [anseml.common.schema :as c]))

(defn build-schema [schema]
  (c/build-schema schema))

(defn validate [schema root]
  (c/validate schema root))

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
;  (println (validate schema (create-element
;                              '(:root {:width  "1"
;                                       :height 2}
;                                 "bla-bla"
;                                 (:inner
;                                   "bla-bla"))))))

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
