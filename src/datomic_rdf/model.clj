(ns datomic-rdf.model
  (:use [clojure.core])
  (:use [clojure.pprint])
  (:use [datomic-schema.schema :only [defpart defschema fields part]])
  (:use [clojure.repl :only [doc find-doc apropos]])
  (:require [clojure.string :as string])
  (:require [datomic.api :as d])
  (:require [datomic.db  :as db])
  (:require [datomic.common :as common])
  (:require [datomic-schema.schema :as schema])
  (:import (java.net  URI))
  (:import (java.util UUID))
  (:gen-class))

;;;
;; URLs
;;
;; (def mem-url "datomic:mem://sandbox")
;; (def db-url  "datomic:free://localhost:4334/sandbox")
;;
;;;
;; Types
;;
;; :keyword :string :boolean :long :bigint :float :double :bigdec
;; :ref :instant :uuid :uri :bytes :enum
;;
;;;
;; Options
;;
;; :unique-value :unique-identity :indexed :many :fulltext :component
;; :nohistory "Some doc string" [:arbitrary "Enum" :values]
;;
;;;

(def db-url "datomic:mem://rdf.model")

(defpart data)

(defschema node (part data)
  (fields
    [kind :enum [:resource :literal :bnode :stmt :graph :tree :leaf :root]]
    [w    :long :one]
    [l    :ref  :one]
    [r    :ref  :one]
    [k    :ref  :one]
    [v    :ref  :one]
    [uuid :uuid :one :indexed :unique-identity]
    ))


(defschema resource (part data)
  (fields
    [uri :uri :one :unique-identity]))

(defschema literal (part data)
  (fields
    [value :string :one :indexed]
    [datatype :ref :one :indexed]
    [language :string :one]))

(defschema bnode (part data)
  (fields
    [name :string :unique-identity]))

(defschema stmt (part data)
  (fields
    [subj :ref :one :indexed]
    [pred :ref :one :indexed]
    [obj  :ref :one :indexed]
    [scope :enum [:global :local]]))

(defschema graph (part data)
  (fields
    [stmts :ref :many]
    [name  :ref :one :unique-identity]))


(defmulti uri type)

(defmethod uri java.lang.String [designator]
  (datomic.io/as-uri designator))

(defmethod uri java.net.URI [designator]
  designator)

(defn conn []
  (d/connect db-url))

(defn db []
  (d/db (conn)))

(defn init-rdf-db []
  (if (d/create-database db-url)
    (do
      @(d/transact (conn) (schema/build-parts d/tempid))
      @(d/transact (conn) (schema/build-schema d/tempid)))))
  
(defn node-kind [ent]
  (when (number? ent)
    (ffirst 
      (d/q '[:find ?ident :in $ ?e :where
              [?e :node/kind ?kind]
              [?kind :db/ident ?ident]]
        (db) ent))))

(defmulti find-node node-kind)

(defmethod find-node :default [ent]
  nil)

(defmulti resource? type)

(defmethod resource? java.lang.String [thing]
  (not
    (empty?
      (d/q '[:find ?e :in $ ?uri :where
              [?e :node/kind     :node.kind/resource]
              [?e :resource/uri  ?uri]]
        (db) (uri thing)))))

(defmethod resource? java.net.URI [thing]
  (not
    (empty?
      (d/q '[:find ?e :in $ ?uri :where
              [?e :node/kind     :node.kind/resource]
              [?e :resource/uri  ?uri]]
        (db) thing))))

(defmethod resource? java.lang.Long [thing]
  (not
    (empty?
      (d/q '[:find ?e :in $ ?e :where
              [?e :node/kind :node.kind/resource]]
        (db) thing))))

(defmethod resource? java.lang.Object [thing]
  false)

(defmulti resource! type)

(defmethod resource! java.lang.String [thing]
  (or
    (ffirst
      (d/q '[:find ?e :in $ ?uri :where
              [?e :node/kind     :node.kind/resource]
              [?e :resource/uri  ?uri]]
        (db) (uri thing)))
     (:e (second (:tx-data @(d/transact (conn)
                              [{:db/id (d/tempid :db.part/data)
                                 :node/kind :node.kind/resource
                                 :resource/uri  (uri thing)}]))))))

(defmethod resource! java.net.URI [thing]
  (or
    (ffirst
      (d/q '[:find ?e :in $ ?uri :where
              [?e :node/kind :node.kind/resource]
              [?e :resource/uri ?uri]]
        (db) thing))
    (:e (second (:tx-data  @(d/transact (conn)
                              [{:db/id (d/tempid :db.part/data)
                                 :node/kind :node.kind/resource
                                 :resource/uri  thing}]))))))

(defmethod resource! java.lang.Long [thing]
  (ffirst
    (d/q '[:find ?e :in $ ?e :where
            [?e :node/kind :node.kind/resource]
            [?e :resource/uri ?uri]]
      (db) thing)))

(defmethod resource! java.lang.Object [thing]
  nil)

(defmethod find-node :node.kind/resource [ent]
  (ffirst
    (d/q '[:find ?uri :in $ ?e :where
            [?e :node/kind :node.kind/resource]
            [?e :resource/uri ?uri]]
      (db) ent)))

(defn- create-initial-resource []
  (resource! "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")) 

(defn bnode!
  ([]
    (let [id (d/tempid :db.part/data)
           node [{:db/id id :node/kind :node.kind/bnode}]]
      (:e (second (:tx-data @(d/transact (conn) node))))))
  ([designator]
    (cond
      (number? designator) (ffirst (d/q '[:find ?e :in $ ?e :where
                                           [?e :node/kind :node.kind/bnode]]
                                     (db) designator))
      (string? designator) (or (ffirst (d/q '[:find ?e :in $ ?name :where
                                               [?e :node/kind :node.kind/bnode]
                                               [?e :bnode/name ?name]]
                                         (db) designator))
                             (let [id (d/tempid :db.part/data)
                                    node [{:db/id id
                                            :node/kind :node.kind/bnode
                                            :bnode/name designator}]]
                               (:e (second (:tx-data @(d/transact (conn) node))))))
      :else nil)))

(defmulti bnode? type)

(defmethod bnode? java.lang.Long [thing]
  (= :node.kind/bnode (node-kind thing)))

(defmethod bnode? java.lang.String [thing]
  (not (empty? (d/q '[:find ?e :in $ ?name :where
                       [?e :node/kind :node.kind/bnode]
                       [?e :bnode/name ?name]]
                 (db) thing))))

(defmethod bnode? java.lang.Object [thing]
  false)

(defmethod find-node :node.kind/bnode [ent]
  (ffirst
    (d/q '[:find ?e :in $ ?e :where
            [?e :node/kind :node.kind/bnode]]
      (db) ent)))

;; TODO: this is not quite working for internationalized :literal/language functionality
;; it is included at present only as a syntactical shortcut for string datatype values
;; e.g., (literal "xyz" "en") instead of (literal "xyz" "http://www.w3.org/2001/XMLSchema#string")
;;
;; TODO: this is pretty hairy... fix when rest of api has been fleshed out and i have a better
;; feeling which conveniences/shorthands are actually useful/necessary

(defn literal!
  ([value-or-ent]
    (cond
      (number? value-or-ent) (ffirst (d/q '[:find ?e :in $ ?e :where
                                             [?e :node/kind :node.kind/literal]]
                                       (db) value-or-ent))
      (string? value-or-ent) (literal! value-or-ent
                               (resource! "http://www.w3.org/2001/XMLSchema#simpleType"))

      (map? value-or-ent)    (literal!
                               (:literal/value value-or-ent)
                               (or
                                 (assert (string? (:literal/value value-or-ent)))
                                 (when-let [dtype (:literal/datatype value-or-ent)]
                                   (find-node (resource! dtype)))
                                 (:literal/language value-or-ent)))
      :else nil))
  ([value datatype]
    (if (nil? datatype)
      (literal! value)
      (if (and (string? datatype) (= (count datatype) 2))      
        (or
          (assert (= datatype "en")) ;; for now to ensure scalar response to string literal queries
          (ffirst (d/q '[:find ?e :in $ ?value ?datatype ?lang :where
                          [?e :node/kind :node.kind/literal]
                          [?e :literal/value ?value]
                          [?e :literal/datatype ?datatype]
                          [?e :literal/language ?lang]]
                    (db) value (resource! "http://www.w3.org/2001/XMLSchema#string") datatype))
          (let [id (d/tempid :db.part/data)
                 node [{:db/id id
                         :node/kind :node.kind/literal
                         :literal/value value
                         :literal/datatype (resource! "http://www.w3.org/2001/XMLSchema#string")
                         :literal/language datatype}]]
            (:e (second (:tx-data @(d/transact (conn) node))))))
        (or
          (assert (string? value))
          (ffirst (d/q '[:find ?e :in $ ?value ?datatype :where
                          [?e :node/kind :node.kind/literal]
                          [?e :literal/value ?value]
                          [?e :literal/datatype ?datatype]]
                    (db) value (resource! datatype)))
          (let [id (d/tempid :db.part/data)
                 node [{:db/id id
                         :node/kind :node.kind/literal
                         :literal/value value
                         :literal/datatype (resource! datatype)}]]
            (:e (second (:tx-data @(d/transact (conn) node))))))))))


(defmulti literal? type)

(defmethod literal? clojure.lang.PersistentArrayMap [thing]
  (string? (:literal/value thing)))

(defmethod literal? java.lang.Long [thing]
  (= :node.kind/literal (node-kind thing)))

(defmethod literal? java.lang.Object [thing]
  false)

(defmethod find-node :node.kind/literal [ent]
  (let [[value datatype]
         (first
         (d/q '[:find ?value ?datatype :in $ ?e :where
                 [?e :node/kind :node.kind/literal]
                 [?e :literal/value ?value]
                 [?e :literal/datatype ?datatype]]
           (db) ent))]
    {:literal/value value
      :literal/datatype (find-node datatype)}))

(defn stmt!
  ([thing]
    (cond
      (number? thing) (ffirst (d/q '[:find ?e :in $ ?e :where
                                      [?e :node/kind :node.kind/stmt]]
                                (db) thing))
      (coll? thing) (stmt! (nth thing 0) (nth thing 1) (nth thing 2))
      :else nil))
  ([sub pred obj]
    (let [stmt-ent (ffirst (d/q '[:find ?e :in $ ?s ?p ?o :where
                                   [?e :stmt/subj ?s]
                                   [?e :stmt/pred ?p]
                                   [?e :stmt/obj  ?o]]
                             (db)
                             (or (resource! sub) (bnode! sub))
                             (resource! pred)
                             (or (resource! obj) (literal! obj) (bnode! obj))))]
      (or
        stmt-ent
        (let [id (d/tempid :db.part/data)
               stmt [{:db/id id
                       :node/kind :node.kind/stmt
                       :stmt/subj (or (resource! sub) (bnode! sub))
                       :stmt/pred (resource! pred)
                       :stmt/obj (or (resource! obj) (literal! obj) (bnode! obj))}]]
          (:e (second (:tx-data @(d/transact (conn) stmt)))))))))

(defn find-stmt [ent]
  (when-let [statement (first (d/q '[:find ?s ?p ?o :in $ ?e :where
                                      [?e :stmt/subj ?s]
                                      [?e :stmt/pred ?p]
                                      [?e :stmt/obj  ?o]]
                                (db) ent))]
    (apply vector (map find-node statement))))
    
(defn stmt? [thing]
  (not (nil? (stmt! thing))))

(defn stmts! [& args]
  (if (and (= 1 (count args)) (coll? (first args)))
    (apply stmts! (first args))
    (mapv stmt! args)))

(defn find-stmts [& args]
  (if (and (= 1 (count args)) (coll? (first args)))
    (apply find-stmts (first args))
    (mapv find-stmt (apply stmts! args))))

(defn stmts? [& args]
  (if (and (= 1 (count args)) (coll? (first args)))
    (apply stmts? (first args))
    (every? stmt? args)))

(defn graph! [designator]
  (let [name-ent (or (resource! designator) (bnode! designator))]
    (assert (number? name-ent))
    (let [graph-ent (ffirst (d/q '[:find ?e :in $ ?name :where
                                    [?e :node/kind :node.kind/graph]
                                    [?e :graph/name ?name]]
                              (db) name-ent))]
      (or
        graph-ent
        (let [id (d/tempid :db.part/data)
               stmt [{:db/id id
                       :node/kind :node.kind/graph
                       :graph/name name-ent}]]
          (:e (second (:tx-data @(d/transact (conn) stmt)))))))))

