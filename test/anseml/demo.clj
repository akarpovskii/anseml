(ns anseml.demo
  (:require [anseml.core :refer :all])
  (:import (java.io BufferedReader StringReader)))

(def example-element
  (anseml.dom.model/create-element
    '(:html
       (:head (:title "Your title here"))
       (:body {:bgcolor "FFFFFF"}
         (:center (:img {:src "brokenImg.png"
                         :align "bottom"}))
         (:hr
           (:a {:href "http://somegreatsite.com"} "Link Name")
           "is a link to another nifty site"
           (:h1 "This is a Header")
           (:h2 "This is a Medium Header")
           "Send me mail at " (:a {:href "mailto:support@yourcompany.com"} "support@yourcompany.com")
           (:p "This is a new paragraph!")
           (:p (:b "This is a new paragraph!"))
           (:br) (:b (:i "This is a new sentence without a paragraph break, in bold italics")))))))

; Write to file
(with-open [w (clojure.java.io/writer  "demo_output.txt")]
  (binding [*out* w]
    (anseml.dom.writer/write example-element)))

; Read file
(let [dom-file (with-open [rdr (clojure.java.io/reader "demo_output.txt")]
                 (anseml.dom.parser/parse rdr))
      sax-file (with-open [rdr (clojure.java.io/reader "demo_output.txt")]
                 (anseml.sax.parser/parse-lazy-seq rdr))]
  (println (if (= dom-file example-element)
             "Files are equal"))
  (println (type sax-file)))

; Transform to HTML DOM
(with-open [w (clojure.java.io/writer  "demo_output_dom.html")]
  (binding [*out* w]
    (anseml.dom.transformation/transform-to-html example-element)))

; Transform to HTML SAX
(with-open [w (clojure.java.io/writer  "demo_output_sax.html")
            rdr (clojure.java.io/reader "demo_output.txt")]
  (binding [*out* w]
    (anseml.sax.transformation/transform-to-html rdr)))

; Fix broken image src attribute
(let [fixed (-> example-element
                (anseml.dom.query/query "//:img")
                (first)
                (anseml.dom.query/update-doc :update-attributes
                                             (fn [attrs]
                                               (assoc attrs :src "https://www.google.ru/images/branding/googlelogo/1x/googlelogo_color_272x92dp.png")))
                (anseml.dom.query/apply-update))]
  (with-open [w (clojure.java.io/writer "demo_output_dom_fixed.html")]
    (binding [*out* w]
      (anseml.dom.transformation/transform-to-html fixed))))


; Schema example
(def schema (anseml.dom.model/create-element
              '(:scm-schema
                 (:scm-attribute {:tag "width"}
                   (:scm-alternative
                     :scm-string
                     :scm-integer))

                 (:scm-attribute {:tag "height"}
                   :scm-integer)

                 (:scm-element {:tag "inner"}
                   :scm-string)

                 (:scm-element {:tag "root"}
                   (:scm-sequence
                     :scm-string
                     :inner)
                   (:scm-attribute
                     :width
                     :height)))))

(println (anseml.dom.schema/validate
           (anseml.dom.schema/build-schema schema)
           (anseml.dom.model/create-element
             '(:root {:width  1
                      :height 2}
                "bla-bla"
                (:inner
                  "bla-bla")))))

(println (anseml.sax.schema/validate
           (anseml.sax.schema/build-schema schema)
           (anseml.sax.parser/parse-lazy-seq
             (BufferedReader.
               (StringReader.
                 "(:root {:width \"1\" :height 2}\n \"bla-bla\"\n (:inner\n \"bla-bla\"))")))))