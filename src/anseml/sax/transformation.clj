(ns anseml.sax.transformation
  (:require [anseml.sax.parser :as sax])
  (:import (java.io BufferedReader)))

(def ^:dynamic
  ^{:doc "Bind to true if you want write to use tabs instead of spaces"}
  *print-indent-use-tabs* false)

(def ^:dynamic
  ^{:doc "The number of spaces to use instead of a tab"}
  *print-tab-size* 2)

(def ^:dynamic
  ^{:doc "The number of tabs to use as an indentation."}
  *print-indent-size* 1)

(defn- indent-char []
  (let [count (* *print-indent-size* (if *print-indent-use-tabs* 1 *print-tab-size*))]
    (apply str (repeat count (if *print-indent-use-tabs* \tab \space)))))

(defn- indent-str [indent]
  (apply str (repeat indent (indent-char))))

(defn- do-transform [rdr]
  {:pre [(instance? BufferedReader rdr)]}
  (let [indent (atom -1)
        indent-tag (fn [] (indent-str @indent))
        indent-body (fn [] (indent-str (inc @indent)))
        close-tag (atom false)]
    (binding [sax/*element-start* (fn [tag attrs]
                                    (swap! indent inc)
                                    (if @close-tag
                                      (println ">"))
                                    (print (str (indent-tag) "<" (name tag)))
                                    (doseq [kv attrs]
                                      (print (str " " (name (first kv)) "="))
                                      (pr (second kv)))
                                    (reset! close-tag true))
              sax/*element-end* (fn [tag]
                                  (if @close-tag
                                    (println "/>")
                                    (println (str (indent-tag) "</" (name tag) ">")))
                                  (swap! indent dec)
                                  (reset! close-tag false))
              sax/*element-primitive* (fn [value]
                                        (if @close-tag
                                          (println ">"))
                                        (println (str (indent-body) value))
                                        (reset! close-tag false))]
      (sax/parse rdr))))

(defn transform-to-html [doc]
  {:pre [(instance? BufferedReader doc)]}
  (do-transform doc))

;(def html-example '(:html
;                     (:head (:title "Your title here"))
;                     (:body {:bgcolor "FFFFFF"}
;                       (:center (:img {:src "clouds.jpg" :align "bottom"}))
;                       (:hr
;                         (:a {:href "http://somegreatsite.com"} "Link Name")
;                         "is a link to another nifty site"
;                         (:h1 "This is a Header")
;                         (:h2 "This is a Medium Header")
;                         "Send me mail at " (:a {:href "mailto:support@yourcompany.com"} "support@yourcompany.com")
;                         (:p "This is a new paragraph!")
;                         (:p (:b "This is a new paragraph!"))
;                         (:br) (:b (:i "This is a new sentence without a paragraph break, in bold italics"))))))
;
;(let [rdr (BufferedReader. (StringReader. (with-out-str (pr html-example))))
;      sax (with-out-str (transform-to-html rdr))]
;  (println sax))
