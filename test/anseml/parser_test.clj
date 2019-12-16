(ns anseml.parser-test
  (:require [clojure.test :refer :all]
            [anseml.dom-parser :refer :all]
            [anseml.writer :refer :all]
            [anseml.model :refer :all])
  (:import (java.io StringReader BufferedReader)))

(deftest parser-test
  (testing "Valid documents"
    (are [elem-desc]
      (let [elem (create-element elem-desc)
            parsed (parse (BufferedReader. (StringReader. (with-out-str (write elem)))))]
        (= elem parsed))
      `(:root)
      `(:root
         "value")
      `(:root
         "value"
         123)
      `(:root
         {:attr "attr-val"}
         "value")
      `(:root
         ~(array-map :attr "attr-val"
                     :attr2 1.5
                     :attr3 1.5e10)
         "value")
      `(:root
         (:inner))
      `(:root
         (:inner
           {:attr "inner"}))
      `(:root
         (:inner
           {:attr "inner"}
           "inner-val"))
      `(:root
         (:inner
           {:attr "inner"}
           "inner-val")))))

; TODO: test invalid inputs

