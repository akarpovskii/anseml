(ns anseml.sax-parser
  (:require [anseml.tokenizer :refer :all])
  (:import (java.io BufferedReader)))

(defn parse-lazy-seq
  ":elem-started, :elem-ended, :elem-primitive"
  [rdr]
  {:pre [(instance? BufferedReader rdr)]}
  (let [tokens (mapcat (comp (partial remove (comp #{:whitespace} first)) tokenize) (line-seq rdr))
        elem-stack (atom '())
        attrs (atom {})
        attr (atom '())
        state (atom :init)
        unexpected (fn [token] (throw (ex-info "Unexpected token." {:expected nil :got token})))]
    (letfn [(iter [[token & tokens]]
              (if token
                (case (first token)
                  :open-paren (case @state
                                :init (do (reset! state :elem-started)
                                          (iter tokens))
                                :elem-started (lazy-seq (cons [:elem-started (peek @elem-stack) {}] (iter tokens)))
                                (unexpected token))
                  :close-paren (case @state
                                 :init (let [top (peek @elem-stack)]
                                         (swap! elem-stack pop)
                                         (lazy-seq (cons [:elem-ended top] (iter tokens))))
                                 :elem-started (let [top (peek @elem-stack)]
                                                 (swap! elem-stack pop)
                                                 (reset! state :init)
                                                 (lazy-seq (cons [:elem-started top {}]
                                                                 (cons [:elem-ended top] (iter tokens)))))
                                 (unexpected token))
                  :open-curly (if (not= @state :elem-started)
                                (unexpected token)
                                (do (reset! state :attrs-started)
                                    (iter tokens)))
                  :close-curly (if (not= @state :attrs-started)
                                 (unexpected token)
                                 (let [at @attrs]
                                   (reset! state :init)
                                   (reset! attrs {})
                                   (lazy-seq (cons [:elem-started (peek @elem-stack) at] (iter tokens)))))
                  :keyword (let [kw (keyword (subs (second token) 1))]
                             (case @state
                               :elem-started (do (swap! elem-stack conj kw)
                                                 (iter tokens))
                               :init (lazy-seq (cons [:elem-started kw {}]
                                                     (cons [:elem-ended kw] (iter tokens))))
                               (if (empty? @attr)
                                 (do (swap! attr conj kw)
                                     (iter tokens))
                                 (unexpected token))))
                  (let [primitive (case (first token)
                                    :integer (Integer/parseInt (second token))
                                    :float (Double/parseDouble (second token))
                                    :string (#(subs % 1 (dec (count %))) (second token))
                                    nil)]
                    (case @state
                      :attrs-started (if (empty? @attr)
                                       (unexpected token)
                                       (do (swap! attrs assoc (peek @attr) primitive)
                                           (reset! attr '())
                                           (iter tokens)))
                      :elem-started (do (reset! state :init)
                                        (lazy-seq (cons [:elem-started (peek @elem-stack) @attrs]
                                                        (cons [:elem-primitive primitive] (iter tokens)))))
                      :init (lazy-seq (cons [:elem-primitive primitive] (iter tokens)))
                      (unexpected token))))
                (if (seq @elem-stack)
                  (unexpected "End of input"))))]
      (iter tokens))))

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
  (doseq [event (parse-lazy-seq rdr)]
    (case (event 0)
      :elem-started (*element-start* (event 1) (event 2))
      :elem-ended (*element-end* (event 1))
      :elem-primitive (*element-primitive* (event 1)))))