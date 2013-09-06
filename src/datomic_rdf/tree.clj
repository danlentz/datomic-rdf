(ns datomic-rdf.tree
  (:use [clojure.core])
  (:use [clojure.pprint])
  (:use [datomic-schema.schema :only [defpart defschema fields part]])
  (:use [clojure.repl :only [doc find-doc apropos]])
  (:require [datomic-rdf.model :refer :all])
  (:require [clojure.string :as string])
  (:require [datomic.api :as d])
  (:require [datomic.db  :as db])
  (:require [datomic.common :as common])
  (:require [datomic-schema.schema :as schema])
  (:import (java.net  URI))
  (:import (java.util UUID))
  (:gen-class))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tree-nodes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def <leaf> (or (ffirst (d/q '[:find ?e :in $ :where
                                [?e :node/kind :node.kind/leaf]]
                          (db)))
              (:e (second (:tx-data  @(d/transact (conn)
                                        [{:db/id (d/tempid :db.part/data)
                                           :node/kind :node.kind/leaf}]))))))

(defn leaf? [thing]
  (or (nil? thing) (= thing <leaf>)))

(defmethod find-node :node.kind/leaf [ent]
  nil)

(defn node!
  ([]
    (:e (second (:tx-data @(d/transact (conn)
                             [{:db/id (d/tempid :db.part/data)
                                :node/kind :node.kind/tree}])))))
  ([designator]
    (cond
      (number? designator)  (ffirst (d/q '[:find ?e :in $ ?e :where
                                            [?e :node/kind :node.kind/tree]]
                                      (db) designator))
      (and (coll? designator)
        (= (count designator) 5)) (apply node! designator)
      :else nil))
  ([k v l r w]
    (:e (second (:tx-data @(d/transact (conn)
                             [{:db/id (d/tempid :db.part/data)
                                :node/kind :node.kind/tree
                                :node/k (literal! k)
                                :node/v (literal! v)
                                :node/l (or l <leaf>)
                                :node/r (or r <leaf>)
                                :node/w (or w 1)}]))))))

(defn tree? [ent]
  (= :node.kind/tree (node-kind ent)))

(defn node-k [ent]
  (ffirst (d/q '[:find ?k :in $ ?e :where
                  [?e :node/kind :node.kind/tree]
                  [?e :node/k ?k]]
            (db) ent)))

(defn node-v [ent]
  (ffirst (d/q '[:find ?v :in $ ?e :where
                  [?e :node/kind :node.kind/tree]
                  [?e :node/v ?v]]
            (db) ent)))

(defn node-l [ent]
  (ffirst (d/q '[:find ?l :in $ ?e :where
                  [?e :node/kind :node.kind/tree]
                  [?e :node/l ?l]]
            (db) ent)))

(defn node-r [ent]
  (ffirst (d/q '[:find ?r :in $ ?e :where
                  [?e :node/kind :node.kind/tree]
                  [?e :node/r ?r]]
            (db) ent)))

(defn node-w [ent]
  (ffirst (d/q '[:find ?w :in $ ?e :where
                  [?e :node/kind :node.kind/tree]
                  [?e :node/w ?w]]
            (db) ent)))

(defn k [ent]
  (find-node (node-k ent)))

(defn v [ent]
  (find-node (node-v ent)))

(defn l [ent]
  (when (not= ent <leaf>)
    (find-node (node-l ent))))

(defn r [ent]
  (when (not= ent <leaf>)
    (find-node (node-r ent))))

(defn w [ent]
  (if (= ent <leaf>)
    0
    (node-w ent)))

(defmethod find-node :node.kind/tree [ent]
  (let [n (first (d/q '[:find ?k ?v ?l ?r ?w :in $ ?e :where
                         [?e :node/kind :node.kind/tree]
                         [?e :node/k ?k]
                         [?e :node/v ?v]
                         [?e :node/l ?l]
                         [?e :node/r ?r]
                         [?e :node/w ?w]]
                   (db) ent))]
    (conj (mapv find-node (take 4 n)) (nth n 4))))

(defn node-kv [ent]
  (vec (take 2 (find-node ent))))

(defn node-lr [ent]
  (vec (take 2 (drop 2 (find-node ent)))))

(defn node-kvlr [ent]
  (vec (take 4 (find-node ent))))

(defn node-kvlrw [ent]
  (find-node ent))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Destructuring Macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro kv "destructure tree node: key, value"
  [[[k v] ent] & body]
  `(let [~k (k ~ent)
          ~v (v ~ent)]
     ~@body))

(defmacro lr "destructure tree node: left, right"
  [[[l r] ent] & body]
  `(let [~l (node-l ~ent)
          ~r (node-r ~ent)]
     ~@body))

(defmacro kvlr "destructure tree node: key, value, left, right"
  [[[k v l r] ent] & body]
  `(let [~k (k ~ent)
          ~v (v ~ent)
          ~l (node-l ~ent)
          ~r (node-r ~ent)]
     ~@body))

(defmacro kvlrw "destructure tree node: key, value, left, right, weight"
  [[[k v l r w] ent] & body]
  `(let [~k (k ~ent)
          ~v (v ~ent)
          ~l (node-l ~ent)
          ~r (node-r ~ent)
          ~w (w ~ent)]
     ~@body))

(defn node-call 
  "apply FN to the destructured constituent values of
   NODE. FN is a function taking four parameters: K, V, L, and R,
   where K is the key of NODE, V is the value of NODE, L is the left
   subtree of NODE, and R is the right subtree of NODE."
  [tree fn]
    (apply fn (node-kvlr node)))

(defn left-enum 
  "efficient mechanism to accomplish partial enumeration of
   tree-structure into a vector representation without incurring the
   overhead of operating over the entire tree.  Used internally for
   implementation of higher-level collection api routines"
  ([tree]
    (left-enum tree nil))
  ([tree enum]
    (if (leaf? tree)
      enum
      (kvlr [[k v l r] tree]
        (left-enum l (vector (vector k v) r enum))))))


