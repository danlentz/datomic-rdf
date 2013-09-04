(ns datomic-rdf.model-test
  (:require [clojure.test :refer :all]
            [datomic-rdf.model :refer :all]))

(init-rdf-db) 

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


;; (run-tests) => "C-c ,"
