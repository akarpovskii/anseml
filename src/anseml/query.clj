(ns anseml.query
  (:require [anseml.model :refer :all]
            [anseml.common :refer :all]
            [clojure.zip :as z]
            [clojure.string :as s]
            [clojure.edn :as edn]))

(declare ^:dynamic *pred-elem*)

(defn- read-preds [string]
  ;(println string)
  (let [pred (edn/read-string string)
        supported-type? (some-fn string? integer? float?    ; supported-primitives
                                 list?                      ; function
                                 keyword?                   ; attribute
                                 (reduce (fn [res op]       ; basic operations
                                           (some-fn res
                                                    #(= op %)))
                                         (constantly false)
                                         ['+ '- '* '/ '= '!= '< '<= '> '>=
                                          'mod 'rem 'and 'or 'not 'nil?]))]
    (letfn [(check-preds [pred]
              (if-not (list? pred)
                (throw (IllegalArgumentException. (str "Expected list, got " string))))
              (doall (map (fn [p]
                            (if (list? p)
                              (check-preds p)
                              (if-not (supported-type? p)
                                (throw (IllegalArgumentException. (str "Unsupported token " p))))))
                          pred)))
            (substitute-keywords [pred]
              (reverse (into '() (map (fn [p]
                                        (cond (list? p) (substitute-keywords p)
                                              (keyword? p) (list p (list get-attrs `*pred-elem*))
                                              :else p))
                                      pred))))]
      (check-preds pred)
      (let [pred (substitute-keywords pred)]
        (fn [elem]
          (binding [*pred-elem* elem]
            (eval pred)))))))

;(println ((read-preds "(= :attr1 \"attr1-val\")")
;          (create-element `(:root
;                             {:attr1 "attr1-val"
;                              :attr2 "attr2-val"}))))

(defn- tokenize-path [path]
  (->> (s/split path #"((?<=//)|(?=//))")
       (mapcat (fn [v]
                 (if (= "//" v)
                   [v]
                   (s/split v #"/"))))
       (remove empty?)
       (map (fn [v]
              (let [create-token (fn [name pred] {::name name, ::pred pred})
                    split (s/split v #"\s+@")
                    name (first split)
                    preds (next split)]
                (cond
                  (empty? preds) (create-token name nil)
                  (= 1 (count preds)) (create-token name (read-preds (first preds)))
                  :else (throw (IllegalArgumentException. (str "Expected 0 or 1 predicate, got " preds)))))))))

;(println (tokenize-path "//:root @(= :attr1 \"val\")"))
;(println ((::pred (first (tokenize-path ":root @(and (= :width 1) (= :tag \"tag2\"))/")))
;          (create-element `(:root
;                             {:width 1
;                              :tag   "tag2"}))))

(defn- iter [path elem]
  (if (z/end? path)
    [elem]
    (let [tag-str (let [tag (get-tag (elem 0))]
                    (case (::dot (meta (elem 0)))
                      ::this "."
                      ::parent ".."
                      (str tag)))
          last-token? (z/end? (z/next path))
          children (fn [node] (let [down (z/down node)
                                    res (take-while
                                          #(and % (% 0))
                                          (iterate z/right down))
                                    res (conj res (z/edit node #(vary-meta % assoc ::dot ::this)))
                                    res (if (z/up node)
                                          (conj res (z/edit (z/up node) #(vary-meta % assoc ::dot ::parent)))
                                          res)]
                                res))
          path-node (::name (path 0))
          path-pred (::pred (path 0))]
      ;(print path-node (get-tag (elem 0)) tag-str)
      ;(println)
      ;(println "--- " (get-tag (elem 0)) tag-str path-node)
      ;(doall (map #(println (get-tag (first %))) (ordinary-children elem)))
      (cond
        (and (= "//" path-node)
             (not= "." tag-str)
             (not= ".." tag-str)) (into [] (distinct (concat (iter (z/next path) elem)
                                                      (mapcat #(iter path %) (children elem)))))
        (and
          (or (and (= "*" path-node)
                   (not= "." tag-str)
                   (not= ".." tag-str))
              ; Enter the node only if it is the beginning of the path
              (and (= "." path-node)
                   (nil? (z/left path))
                   (not= ".." tag-str))
              (= path-node tag-str))
          (try
            (or (nil? path-pred)
                (path-pred (elem 0)))
            (catch Exception _ false))) (if last-token?
                                                       [elem]
                                                       (distinct (mapcat
                                                                   #(iter (z/next path) %)
                                                                   (children elem))))
        :else []))))

(defn query
  "The `elem` must be either an element created with `create-element`,
  or a zipper obtained by a previous call to `query`.
  Returns a zipper focused on the requested node.

  Format:
   `/`            -- searches for element from the root
   `:name`        -- same as `/:name`
   `//`           -- searches for element anywhere in the document starting from the root element
   `.`            -- selects current node
   `..`           -- selects parent node
   `*`            -- corresponds to any element
   `@<predicate>` -- specifies an attribute-level predicate, that may contain following operations:
                     +, -, *, /, =, !=, <, <=, >, >=, mod, rem, and, or, not, nil?"
  [elem path]
  {:pre [(string? path)]}
  (let [path (tokenize-path path)
        elem (if (element? elem)
               (element-zipper elem)
               elem)
        first-node (::name (first path))
        relative? (= (first first-node) \.)
        elem (if relative?
               elem
               (element-zipper (z/root elem)))
        path (if relative? (conj path {::name "." ::pred nil}) path)
        path (z/next (z/seq-zip path))]
    (iter path elem)))

(def root (create-element `(:root
                             {:attr1 "attr1-val"
                              :attr2 "attr2-val"}
                             "value"
                             (:inner1
                               {:tag "tag1"}
                               (:root "inner-root1"))
                             (:inner1
                               {:tag "tag2"
                                :width 1}
                               (:root "inner-root2"))
                             (:inner2)
                             "after-inner")))

;(doall (map println (-> root
;                        (query "./:inner1 @(= :width 1)/")
;                        (->> (map first)))))
;
;(doall (map println (-> root
;                        (query "./:inner1 @(and (= :width 1) (= :tag \"tag2\"))/")
;                        (->> (map first)))))
;
;(println "--------------")
;
;(doall (map println (-> root
;                        element-zipper
;                        z/down
;                        (query "../.")
;                        (->> (map first)))))
;
;(println (-> '(1 2 3)
;             z/seq-zip
;             ))