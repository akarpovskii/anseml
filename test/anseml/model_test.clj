(ns anseml.model-test
  (:require [clojure.test :refer :all]
            [anseml.model :refer :all]))

(deftest create-element-test
  (testing "Creation of a root element"
    (let [elem (create-element (:root
                                 {:attr "attr-value"}
                                 "body"))]
      (is (element? elem))
      (is (= (get-tag elem) :root))
      (is (= 1 (count (get-attrs elem))))
      (is (= (get-attrs elem :attr) "attr-value"))
      (is (= (get-value elem) (list "body")))))

  (testing "Nested elements"
    (let [elem (create-element (:root
                                 {:attr "attr-value"}
                                 :inner))]
      (is (element? elem))
      (is (= (get-tag elem) :root))
      (is (= 1 (count (get-attrs elem))))
      (is (= (get-attrs elem :attr) "attr-value"))
      (is (= 1 (count (get-value elem))))
      (is (element? (first (get-value elem))))
      (let [inner (first (get-value elem))]
        (is (= (get-tag inner) :inner))
        (is (empty? (get-attrs inner)))
        (is (empty? (get-value inner))))))

  (testing "List of values"
    (let [elem (create-element (:root
                                 {:attr "attr-value"}
                                 123 (:inner) "value"))]
      (is (element? elem))
      (is (= (get-tag elem) :root))
      (is (= 1 (count (get-attrs elem))))
      (is (= (get-attrs elem :attr) "attr-value"))
      (is (= 3 (count (get-value elem))))
      (let [[val0 val1 val2] (get-value elem)]
        (is (= 123 val0))
        (is (element? val1))
        (is (= (get-tag val1) :inner))
        (is (empty? (get-attrs val1)))
        (is (empty? (get-value val1)))
        (is (= "value" val2)))))

  (testing "Invalid arguments"
    (is (thrown? IllegalArgumentException (create-element ())))
    (is (thrown? IllegalArgumentException (create-element (123))))
    (is (thrown? IllegalArgumentException (create-element (:tag ["unsupported value"]))))
    (is (thrown? IllegalArgumentException (create-element (:tag {"unsupported" "key"}))))
    (is (thrown? IllegalArgumentException (create-element (:tag {:unsupported :value}))))))


(deftest setters-test
  (let [elem (create-element (:root
                               {:attr "attr-val"}
                               "value1"
                               2
                               (:inner
                                 "inner-value")))]

    (testing "Set tag"
      (let [attrs-before (get-attrs elem)
            value-before (get-value elem)
            new-elem (set-tag elem :not-root)]
        (is (= :not-root (get-tag new-elem)))
        (is (= attrs-before (get-attrs new-elem)))
        (is (= value-before (get-value new-elem)))))

    (testing "Set attrs"
      (let [tag-before (get-tag elem)
            value-before (get-value elem)
            attrs {:new-attr "new-val"}
            new-elem (set-attrs elem attrs)]
        (is (= tag-before (get-tag new-elem)))
        (is (= attrs (get-attrs new-elem)))
        (is (= value-before (get-value new-elem))))

      (let [attrs {"incorrect" :attrs}]
        (is (thrown? IllegalArgumentException (set-attrs elem attrs))))

      (let [tag-before (get-tag elem)
            value-before (get-value elem)
            attrs [:new-attr1 "new-val1" :new-attr2 "new-val2"]
            new-elem (apply set-attrs elem attrs)]
        (is (= tag-before (get-tag new-elem)))
        (is (= (apply hash-map attrs) (get-attrs new-elem)))
        (is (= value-before (get-value new-elem))))

      (let [tag-before (get-tag elem)
            value-before (get-value elem)
            attr :new-attr
            attr-val "new-val"
            new-elem (set-attrs elem attr attr-val)]
        (is (= tag-before (get-tag new-elem)))
        (is (= (hash-map attr attr-val) (get-attrs new-elem)))
        (is (= value-before (get-value new-elem)))))

    (testing "Add attrs"
      (let [tag-before (get-tag elem)
            value-before (get-value elem)
            attrs-before (get-attrs elem)
            attrs {:new-attr "new-val"}
            new-elem (add-attrs elem attrs)]
        (is (= tag-before (get-tag new-elem)))
        (is (= (merge attrs-before attrs) (get-attrs new-elem)))
        (is (= value-before (get-value new-elem))))

      (let [attrs {"incorrect" :attrs}]
        (is (thrown? IllegalArgumentException (add-attrs elem attrs))))

      (let [tag-before (get-tag elem)
            value-before (get-value elem)
            attrs-before (get-attrs elem)
            attrs [:new-attr1 "new-val1" :new-attr2 "new-val2"]
            new-elem (apply add-attrs elem attrs)]
        (is (= tag-before (get-tag new-elem)))
        (is (= (apply assoc attrs-before attrs) (get-attrs new-elem)))
        (is (= value-before (get-value new-elem))))

      (let [tag-before (get-tag elem)
            value-before (get-value elem)
            attrs-before (get-attrs elem)
            attr :new-attr
            attr-val "new-val"
            new-elem (add-attrs elem attr attr-val)]
        (is (= tag-before (get-tag new-elem)))
        (is (= (assoc attrs-before attr attr-val) (get-attrs new-elem)))
        (is (= value-before (get-value new-elem)))))

    (testing "Remove attrs"
      (let [tag-before (get-tag elem)
            value-before (get-value elem)
            new-elem (remove-attrs elem :attr)]
        (is (= tag-before (get-tag new-elem)))
        (is (empty? (get-attrs new-elem)))
        (is (= value-before (get-value new-elem))))

      (let [tag-before (get-tag elem)
            value-before (get-value elem)
            attrs-before (get-attrs elem)
            new-elem (remove-attrs elem :not-existing)]
        (is (= tag-before (get-tag new-elem)))
        (is (= attrs-before (get-attrs new-elem)))
        (is (= value-before (get-value new-elem))))

      (is (thrown? AssertionError (remove-attrs elem "wron-type"))))

    (testing "Set value"
      (let [tag-before (get-tag elem)
            attrs-before (get-attrs elem)
            new-elem (set-value elem "new-val")]
        (is (= tag-before (get-tag new-elem)))
        (is (= attrs-before (get-attrs new-elem)))
        (is (= (list "new-val") (get-value new-elem))))

      (let [tag-before (get-tag elem)
            attrs-before (get-attrs elem)
            new-elem (set-value elem (create-element :new-child))]
        (is (= tag-before (get-tag new-elem)))
        (is (= attrs-before (get-attrs new-elem)))
        (is (= (list (create-element :new-child)) (get-value new-elem))))

      (let [tag-before (get-tag elem)
            attrs-before (get-attrs elem)
            new-elem (set-value elem (list 123 4.5))]
        (is (= tag-before (get-tag new-elem)))
        (is (= attrs-before (get-attrs new-elem)))
        (is (= (list 123 4.5) (get-value new-elem))))

      (is (thrown? IllegalArgumentException (set-value elem ["unsupported"]))))
    ))
