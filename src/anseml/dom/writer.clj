(ns anseml.dom.writer
  (:require [anseml.dom.model :refer :all]))

(def ^:dynamic
  ^{:doc "Bind to true if you want write to use tabs instead of spaces"}
  *print-indent-use-tabs* false)

(def ^:dynamic
  ^{:doc "The number of spaces to use instead of a tab"}
  *print-tab-size* 2)

(def ^:dynamic
  ^{:doc "The number of tabs to use as an indentation."}
  *print-indent-size* 1)

(defn- print-indented [indent elem]
  (let [indent-char (if *print-indent-use-tabs*
                      \tab
                      (apply str (repeat *print-tab-size* \space)))
        indent-char (apply str (repeat *print-indent-size* indent-char))
        indent-str (apply str (repeat indent indent-char))
        tag (get-tag elem)
        attrs (get-attrs elem)
        value (get-value elem)]
    (print (str "(" tag))
    (if-not (empty? attrs)
      (do (println)
          (print (str indent-str indent-char "{"))
          (loop [kvs (seq attrs)]
            (when kvs
              (pr (ffirst kvs) (second (first kvs))))
            (when (next kvs)
              (println)
              (print (str indent-str indent-char \space))
              (recur (next kvs))))
          (print "}")))
    (if-not (empty? value)
      (do (println)
          (doseq [vals (partition 2 1 nil value)]
            (print (str indent-str indent-char))
            (if (element? (first vals))
              (print-indented (inc indent) (first vals))
              (pr (first vals)))
            (when (next vals)
              (println)))))
    (print ")")))

(defn write
  "Writes the document to the *out*."
  [root]
  {:pre [(element? root)]}
  (print-indented 0 root)
  (println))
