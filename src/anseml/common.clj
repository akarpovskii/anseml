(ns anseml.common
  (:require [anseml.model :refer :all]
            [clojure.zip :as z]))

(defn element-zipper [elem]
  (z/zipper
    element?
    (fn [x] (seq (filter element? (get-value x))))
    (fn [node children] (set-value node (reverse (into '() children))))
    elem))