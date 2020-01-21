(ns anseml.writer-test
  (:require [clojure.test :refer :all]
            [anseml.dom.writer :refer :all]
            [anseml.dom.model :refer :all]))

(deftest writer-test
  (letfn [(replace-newlines [str]
            (clojure.string/replace str #"\n" (with-out-str (newline))))
          (write-with-params [elem & {:keys [tab-size use-tabs indent-size]
                                      :or   {tab-size 2 use-tabs false indent-size 1}}]
            (with-out-str (binding [*print-tab-size* tab-size
                                    *print-indent-use-tabs* use-tabs
                                    *print-indent-size* indent-size]
                            (write elem))))]
    (testing "Basic writer"
      (are [elem expected]
        (= (replace-newlines expected)
           (write-with-params elem))
        (create-element `(:root)) "(:root)\n"
        (create-element `(:root
                           "value")) "(:root\n  \"value\")\n"
        (create-element `(:root
                           "value"
                           123)) "(:root\n  \"value\"\n  123)\n"
        (create-element `(:root
                           {:attr "attr-val"}
                           "value")) "(:root\n  {:attr \"attr-val\"}\n  \"value\")\n"
        (create-element `(:root
                           ~(array-map :attr "attr-val"
                                       :attr2 1.5)
                           "value")) "(:root\n  {:attr \"attr-val\"\n   :attr2 1.5}\n  \"value\")\n"
        (create-element `(:root
                           (:inner))) "(:root\n  (:inner))\n"
        (create-element `(:root
                           (:inner
                             {:attr "inner"}))) "(:root\n  (:inner\n    {:attr \"inner\"}))\n"
        (create-element `(:root
                           (:inner
                             {:attr "inner"}
                             "inner-val"))) "(:root\n  (:inner\n    {:attr \"inner\"}\n    \"inner-val\"))\n"))

    (testing "Use tabs"
      (are [elem expected]
        (= (replace-newlines expected)
           (write-with-params elem :use-tabs true))
        (create-element `(:root
                           "value")) "(:root\n\t\"value\")\n"
        (create-element `(:root
                           (:inner
                             {:attr "inner"}
                             "inner-val"))) "(:root\n\t(:inner\n\t\t{:attr \"inner\"}\n\t\t\"inner-val\"))\n"))

    (testing "Indent size"
      (are [elem expected]
        (= (replace-newlines expected)
           (write-with-params elem :indent-size 2))
        (create-element `(:root
                           "value")) "(:root\n    \"value\")\n"
        (create-element `(:root
                           (:inner
                             {:attr "inner"}
                             "inner-val"))) "(:root\n    (:inner\n        {:attr \"inner\"}\n        \"inner-val\"))\n"))

    (testing "Indent size, use tabs"
      (are [elem expected]
        (= (replace-newlines expected)
           (write-with-params elem :indent-size 2 :use-tabs true))
        (create-element `(:root
                           "value")) "(:root\n\t\t\"value\")\n"
        (create-element `(:root
                           (:inner
                             {:attr "inner"}
                             "inner-val"))) "(:root\n\t\t(:inner\n\t\t\t\t{:attr \"inner\"}\n\t\t\t\t\"inner-val\"))\n"))

    (testing "Tab size"
      (are [elem expected]
        (= (replace-newlines expected)
           (write-with-params elem :tab-size 0))
        (create-element `(:root
                           "value")) "(:root\n\"value\")\n"
        (create-element `(:root
                           (:inner
                             {:attr "inner"}
                             "inner-val"))) "(:root\n(:inner\n{:attr \"inner\"}\n\"inner-val\"))\n"))
    ))
