(ns anseml.query-test
  (:require [clojure.test :refer :all]
            [anseml.dom.model :refer :all]
            [anseml.dom.query :refer :all]))

(def ^:private doc-sample
  (create-element
    `(:root
       {:attr1 "attr1-val"
        :attr2 "attr2-val"}
       "value"
       (:inner1
         {:tag "tag1"}
         (:root "inner1-root1"))
       (:inner1
         {:tag   "tag2"
          :width 1}
         (:root "inner1-root2"))
       (:inner2
         (:node-a
           (:node-b)))
       (:node-b "value"))))

(defn- equal [res exp]
  (let [res (map first res)
        eq (= res exp)]
    ;(println res)
    ;(println exp)
    ;(println eq)
    eq))

(deftest query-test
  (testing "Absolute path queries"
    (are [res exp]
      (equal res exp)
      (-> doc-sample (query ":root")) (list doc-sample)
      (-> doc-sample (query "/:root")) (list doc-sample)
      (-> doc-sample (query "/:root/:inner1")) (->> (get-value doc-sample) (next) (take 2))
      (-> doc-sample (query "/:root/:inner1/../:inner2")) (->> (get-value doc-sample) (drop 3) (take 1))))
  (testing "Relative path queries"
    (are [res exp]
      (equal res exp)
      (-> doc-sample (query "./")) (list doc-sample)
      (-> doc-sample (query "./:root")) '()
      (-> doc-sample (query "/:root/:inner1") (first) (query "./:root")) (->> (get-value doc-sample) (second) (get-value))
      (-> doc-sample (query "/:root/:inner1") (first) (query "./../:inner2")) (->> (get-value doc-sample) (drop 3) (take 1))))
  (testing "//"
    (are [res exp]
      (equal res exp)
      (-> doc-sample (query "//:inner1")) (->> (get-value doc-sample) (next) (take 2))
      (-> doc-sample (query "//:inner2")) (->> (get-value doc-sample) (drop 3) (take 1))
      (-> doc-sample (query "//:root")) (list doc-sample
                                              (->> (get-value doc-sample) (second) (get-value) (first))
                                              (->> (get-value doc-sample) (next) (second) (get-value) (first)))
      (-> doc-sample (query "/:root/:inner2//:node-b")) (->> (get-value doc-sample) (drop-last 1) (last) (get-value) (first) (get-value))
      (-> doc-sample (query "/:root/:inner2/..//:node-b")) (list (->> (get-value doc-sample) (drop-last 1) (last) (get-value) (first) (get-value) (first))
                                                                 (->> (get-value doc-sample) (last)))))
  (testing "Wildcards"
    (are [res exp]
      (equal res exp)
      (-> doc-sample (query "*")) (list doc-sample)
      (-> doc-sample (query "/*")) (list doc-sample)
      (-> doc-sample (query "/:root/*/:node-a")) (->> (get-value doc-sample) (drop 3) (first) (get-value))))
  (testing "With attributes"
    (are [res exp]
      (equal res exp)
      (-> doc-sample (query "//:root @(not (nil? :attr1))")) (list doc-sample)
      (-> doc-sample (query "//:inner1 @(= :width 1)/")) (-> (get-value doc-sample) (nth 2) (list)))))
