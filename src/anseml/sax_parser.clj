(ns anseml.sax-parser
  (:require [anseml.tokenizer :refer :all])
  (:import (java.io BufferedReader)))

(def ^:dynamic
  ^{:doc "This callback is called when the parser meets an element
   with tag `tag` and attributes `attrs`."}
  *element-start* (fn [tag attrs]))

(def ^:dynamic
  ^{:doc "This callback is called when the parser ends to process
   an element with tag `tag`."}
  *element-end* (fn [tag]))

(def ^:dynamic
  ^{:doc "This callback is called when the parser meets a `value`
   in the body of an element (see `anseml.sax-parser/*element-start*`)."}
  *element-primitive* (fn [value]))

; TODO: proper error messages
; TODO: some validation based on a model
(defn parse [rdr]
  {:pre [(instance? BufferedReader rdr)]}
  (let [tokens (mapcat (comp (partial remove (comp #{:whitespace} first)) tokenize) (line-seq rdr))
        elem-stack (atom '())
        attrs (atom {})
        attr (atom '())
        state (atom ::init)
        unexpected (fn [token] (throw (ex-info "Unexpected token." {:expected nil :got token})))]
    (doseq [token tokens]
      (case (first token)
        :open-paren (case @state
                      ::init (reset! state ::elem-started)
                      ::elem-started (*element-start* (peek @elem-stack) {})
                      (unexpected token))
        :close-paren (case @state
                       ::init (do (*element-end* (peek @elem-stack))
                                  (swap! elem-stack pop))
                       ::elem-started (do (*element-start* (peek @elem-stack) {})
                                          (*element-end* (peek @elem-stack))
                                          (swap! elem-stack pop)
                                          (reset! state ::init))
                       (unexpected token))
        :open-curly (if (not= @state ::elem-started)
                      (unexpected token)
                      (reset! state ::attrs-started))
        :close-curly (if (not= @state ::attrs-started)
                       (unexpected token)
                       (do (reset! state ::init)
                           (*element-start* (peek @elem-stack) @attrs)
                           (reset! attrs {})))
        :keyword (let [kw (keyword (subs (second token) 1))]
                   (case @state
                     ::elem-started (swap! elem-stack conj kw)
                     ::init (do (*element-start* kw {})
                                (*element-end* kw))
                     (if (empty? @attr)
                       (swap! attr conj kw)
                       (unexpected token))))
        (let [primitive (case (first token)
                          :integer (Integer/parseInt (second token))
                          :float (Double/parseDouble (second token))
                          :string (#(subs % 1 (dec (count %))) (second token))
                          nil)]
          (case @state
            ::attrs-started (if (empty? @attr)
                              (unexpected token)
                              (do (swap! attrs assoc (peek @attr) primitive)
                                  (reset! attr '())))
            ::elem-started (do (reset! state ::init)
                               (*element-start* (peek @elem-stack) @attrs)
                               (*element-primitive* primitive))
            ::init (*element-primitive* primitive)
            (unexpected token)))))
    (if-not (empty? @elem-stack)
      (unexpected "End of input"))))

;(println (binding [*element-start* (fn [tag attrs] (println "elem-start" tag attrs))
;                   *element-end* (fn [tag] (println "elem-end" tag))
;                   *element-primitive* (fn [value] (println "elem-value-part" value))]
;           (parse (BufferedReader. (StringReader.
;                                     "(:root\n  (:inner0\n     {:attr1 \"val1\"\n      :attr2 \"val2\"}\n     \"body\")\n  (:inner1\n     {:attr1 \"val1\"\n      :attr2 \"val2\"}\n     \"body\"))")))))

;(with-open [w (clojure.java.io/writer "output.txt")]
;  (.write w "(:root")
;  (doseq [i (range 20000000)]
;    (.write w (str "\n  (:inner" i "\n     {:attr1 \"val1\"\n      :attr2 \"val2\"}\n     \"body\")")))
;  (.write w ")\n"))

;(time
;  (with-open [rdr (clojure.java.io/reader "output.txt")]
;    (sax-parser (line-seq rdr))))
