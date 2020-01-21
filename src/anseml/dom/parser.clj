(ns anseml.dom.parser
  (:require [anseml.sax.parser :as sax]
            [anseml.dom.model :refer :all])
  (:import (java.io BufferedReader)))

(defn parse [rdr]
  {:pre [(instance? BufferedReader rdr)]}
  (let [elem-stack (atom [[]])]
    (binding [sax/*element-start* (fn [tag attrs]
                                    (swap! elem-stack conj [tag attrs]))
              sax/*element-end* (fn [tag]
                                  (assert (= tag (first (peek @elem-stack))) (str "The element " tag " ended before it was started"))
                                  (let [complete (into '() (reverse (peek @elem-stack)))]
                                    (swap! elem-stack pop)
                                    (swap! elem-stack #(assoc % (- (count %) 1) (conj (peek %) complete)))))
              sax/*element-primitive* (fn [value]
                                        (swap! elem-stack #(assoc % (- (count %) 1) (conj (peek %) value))))]
      (sax/parse rdr)
      (create-element (peek (peek @elem-stack))))))

;(println (parse (BufferedReader. (StringReader.
;                                   "(:root\n  (:inner0\n     {:attr1 \"val1\"\n      :attr2 \"val2\"}\n     \"body\")\n  (:inner1\n     {:attr1 \"val1\"\n      :attr2 \"val2\"}\n     \"body\"))"))))
