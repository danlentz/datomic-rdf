(ns datomic-rdf.model-test
  (:require [clojure.test :refer :all]
            [datomic-rdf.model :refer :all]))

(defn init-db [f]
  (init-rdf-db)
  (f))

(use-fixtures :once init-db)

(deftest resource-tests
  (testing "resource?"
    (is (resource? "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
    (is (not (resource? "xyz")))
    (is (resource? (uri "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")))
    (is (not (resource? (uri "xyz"))))
    (is (not (resource? 5)))
    (is (not (resource? \space))))
  (testing "r"
    (is (number?   (r "http://www.w3.org/1999/02/22-rdf-syntax-ns#subject")))
    (is (resource? (r "http://www.w3.org/1999/02/22-rdf-syntax-ns#subject")))
    (is (=
          :node.kind/resource
          (node-kind (r "http://www.w3.org/1999/02/22-rdf-syntax-ns#subject"))))
    (is (= 
          (r (uri "http://www.w3.org/1999/02/22-rdf-syntax-ns#predicate"))
          (r (r (uri "http://www.w3.org/1999/02/22-rdf-syntax-ns#predicate")))))))

(deftest bnode-tests
  (is (nil? (r (bnode))))
  (is (nil? (bnode \space)))
  (is (number? (bnode)))
  (is (bnode?  (bnode "me")))
  (is (bnode? "me"))
  (is (not (bnode? "you")))
  (is (not (bnode? \space)))
  (is (=
        :node.kind/bnode
        (node-kind (bnode (bnode)))))
  (is (=
        (bnode "xyz")
        (bnode "xyz"))))

(deftest literal-tests
  (testing "literal"
    (is (nil? (literal \space)))
    (is (=
          (literal "three")
          (literal {:literal/value "three"
                     :literal/datatype "http://www.w3.org/2001/XMLSchema#simpleType"})))
    (is (=
          (literal "three" "en")
          (literal {:literal/value "three"
                     :literal/datatype "http://www.w3.org/2001/XMLSchema#string"})))
    (is (=
          (literal "three" "en")
          (literal {:literal/value "three"
                     :literal/language "en"})))
    (is (=
          (literal "three")
          (literal "three" nil)))
    (is (=
          (literal "three")
          (literal "three" "http://www.w3.org/2001/XMLSchema#simpleType")))
    (is (=
          (literal "three" "en")
          (literal "three" "http://www.w3.org/2001/XMLSchema#string")))
    (is (not (=
               (literal "three" "http://www.w3.org/2001/XMLSchema#string")
               (literal "three" "http://www.w3.org/2001/XMLSchema#simpleType"))))
    (is (=
          (literal "three" "http://www.w3.org/2001/XMLSchema#string")
          (literal "three" "http://www.w3.org/2001/XMLSchema#string")))
    (is (=
          :node.kind/literal
          (node-kind (literal "three"))))
    (is (=
          (literal "three")
          (literal (literal "three")))))
  (testing "literal?"
    (is (literal? {:literal/value "three"
                    :literal/datatype "http://www.w3.org/2001/XMLSchema#simpleType"}))
    (is (not (literal? {:literal/datatype "http://www.w3.org/2001/XMLSchema#simpleType"})))
    (is (literal? (literal "three")))
    (is (not (literal? \space)))
    (is (not (literal? (bnode))))
    ))

(deftest node-tests
  (testing "default"
    (is (nil? (find-node 1)))
    (is (nil? (find-node 2))))
  (testing "resource"
    (is (=
          (find-node (r "http://www.w3.org/1999/02/22-rdf-syntax-ns#subject"))
          (uri "http://www.w3.org/1999/02/22-rdf-syntax-ns#subject")))
    (is (=
          (find-node (r "http://www.w3.org/1999/02/22-rdf-syntax-ns#predicate"))
          (uri "http://www.w3.org/1999/02/22-rdf-syntax-ns#predicate")))
    (is (=
          (find-node (r "http://www.w3.org/1999/02/22-rdf-syntax-ns#object"))
          (uri "http://www.w3.org/1999/02/22-rdf-syntax-ns#object"))))
  (testing "bnode"
    (is (number? (find-node (bnode))))
    (is (number? (find-node (bnode "me"))))
    (is (=
          (bnode "me")
          (find-node (bnode "me"))))
    (is (=
          (bnode "me")
          (bnode (find-node (bnode "me"))))))
  (testing "literal"
    (is (map? (find-node (literal "three"))))
    (is (literal? (find-node (literal "three"))))
    (is (=
          (uri "http://www.w3.org/2001/XMLSchema#string")
          (:literal/datatype (find-node (literal "foo" "en")))))
    (let [lit {:literal/value "three"
                :literal/datatype (uri "http://www.w3.org/2001/XMLSchema#simpleType")}]
      (is (= lit (find-node (literal lit))))
      (is (= lit (find-node (literal "three")))))))
      
(deftest stmt-tests
  (is (nil? (stmt 5)))
  (is (number?
        (stmt
          (r "http://example.com/one")
          (r "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
          (r "http://www.w3.org/1999/02/22-rdf-syntax-ns#List"))))
  (is (number?
        (stmt
          (bnode "me")
          (r "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
          (r "http://www.w3.org/1999/02/22-rdf-syntax-ns#List"))))
  (is (number?
        (stmt
          (bnode)
          (r "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
          (r "http://www.w3.org/1999/02/22-rdf-syntax-ns#List"))))
  (is (number?
        (stmt
          (bnode "me")
          (r "http://www.w3.org/1999/02/22-rdf-syntax-ns#value")
          (literal "three"))))
  (is (=
        (stmt
          (bnode "me")
          (r "http://www.w3.org/1999/02/22-rdf-syntax-ns#value")
          (literal "three"))
        (stmt
          (bnode "me")
          (r "http://www.w3.org/1999/02/22-rdf-syntax-ns#value")
          (literal "three"))))
  (is (=
        (stmt
          (r "http://example.com/one")
          (r "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
          (r "http://www.w3.org/1999/02/22-rdf-syntax-ns#List"))
        (stmt
          (uri "http://example.com/one")
          (uri "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
          (uri "http://www.w3.org/1999/02/22-rdf-syntax-ns#List"))))
  (is (=
        (stmt
          (r "http://example.com/one")
          (r "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
          (r "http://www.w3.org/1999/02/22-rdf-syntax-ns#List"))
        (stmt
          (vector
            (uri "http://example.com/one")
            (uri "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
            (uri "http://www.w3.org/1999/02/22-rdf-syntax-ns#List")))))
  (is (=
        (stmt
          "http://example.com/one"
          "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
          "http://www.w3.org/1999/02/22-rdf-syntax-ns#List")
        (stmt
          (uri "http://example.com/one")
          (uri "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
          (uri "http://www.w3.org/1999/02/22-rdf-syntax-ns#List"))))
  (is (=
        (stmt
          (bnode "me")
          (r "http://www.w3.org/1999/02/22-rdf-syntax-ns#value")
          (literal "three"))
        (stmt
          (bnode "me")
          (uri "http://www.w3.org/1999/02/22-rdf-syntax-ns#value")
          (literal "three"))))
  (is (=
        (stmt
          (bnode "x")
          (r "http://www.w3.org/1999/02/22-rdf-syntax-ns#value")
          (literal "three"))
        (stmt
          (stmt
            (bnode "x")
            (r "http://www.w3.org/1999/02/22-rdf-syntax-ns#value")
            (literal "three")))))
  (is (not (=
             (stmt
               (bnode)
               (r "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
               (r "http://www.w3.org/1999/02/22-rdf-syntax-ns#List"))
             (stmt
               (bnode)
               (r "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
               (r "http://www.w3.org/1999/02/22-rdf-syntax-ns#List"))))))

(deftest find-stmt-tests
  (is (=
        (find-stmt
          (stmt
            (uri "http://example.com/one")
            (uri "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
            (uri "http://www.w3.org/1999/02/22-rdf-syntax-ns#List")))
        (vector 
          (uri "http://example.com/one")
          (uri "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
          (uri "http://www.w3.org/1999/02/22-rdf-syntax-ns#List"))))
  (is (=
        (find-stmt
          (stmt
            (bnode "me")
            (r "http://www.w3.org/1999/02/22-rdf-syntax-ns#value")
            (literal "three")))
        [(bnode "me")
          (uri "http://www.w3.org/1999/02/22-rdf-syntax-ns#value")
          {:literal/value "three",
            :literal/datatype (uri "http://www.w3.org/2001/XMLSchema#simpleType")}]))
  (let [st (stmt
             (bnode "me")
             (r "http://www.w3.org/1999/02/22-rdf-syntax-ns#value")
             (literal "three"))]
    (is (=
          st (stmt (find-stmt st))))))

(deftest stmt?-tests
  (is (stmt?  (stmt
                (bnode "x")
                (r "http://www.w3.org/1999/02/22-rdf-syntax-ns#value")
                (literal "three"))))
  (is (stmt?  (vector 
                (uri "http://example.com/one")
                (uri "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
                (uri "http://www.w3.org/1999/02/22-rdf-syntax-ns#List"))))
  (is (stmt?  (find-stmt
                (stmt
                  (bnode "x")
                  (r "http://www.w3.org/1999/02/22-rdf-syntax-ns#value")
                  (literal "three"))))))

(deftest stmts-tests
  (let [slist (list
                (stmt
                  (bnode)
                  (r "http://www.w3.org/1999/02/22-rdf-syntax-ns#value")
                  (literal "one"))
                (stmt
                  (bnode)
                  (r "http://www.w3.org/1999/02/22-rdf-syntax-ns#value")
                  (literal "two"))
                (stmt
                  (bnode)
                  (r "http://www.w3.org/1999/02/22-rdf-syntax-ns#value")
                  (literal "three"))
                (stmt
                  (bnode)
                  (r "http://www.w3.org/1999/02/22-rdf-syntax-ns#value")
                  (literal "four"))
                )]
    (is (every? number? slist))
    (is (= slist (apply stmts slist)))
    (is (= slist (stmts slist)))
    (is (= slist (apply stmts (apply find-stmts slist))))
    (is (= slist (stmts (find-stmts slist))))
    (is (apply stmts? slist))
    (is (stmts? slist))
    (is (not (apply stmts? (cons 1 slist))))
    (is (not (stmts? (cons 1 slist))))))
  



