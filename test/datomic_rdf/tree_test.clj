(ns datomic-rdf.model-test
  (:require [clojure.test :refer :all])
  (:require [datomic-rdf.model :refer :all])
  (:require [datomic-rdf.tree  :refer :all])
    )

(defn init-db [f]
  (init-rdf-db)
  (f))

(use-fixtures :once init-db)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tree-nodes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest tree-node-tests
  (testing "<leaf>"
    (is (number? <leaf>))
    (is (leaf? <leaf>))
    (is (leaf? nil)))
  (testing "tree-nodes"
    (is (tree? (node!)))
    (is (tree? (node! (literal! "k") (literal! "v") nil nil nil)))
    (is (not (tree? <leaf>)))
    (is (not (tree? nil)))
    (let [n (node! (literal! "k") (literal! "v") nil nil nil)]
      (is (= n (node! n))))
    (let [n (node!)]
      (is (= n (node! n))))
    (let [n (node! (literal! "k") (literal! "v") nil nil nil)]
      (is (=
            (k n)
            (find-node (node-k n))
            {:literal/value "k",
              :literal/datatype (uri "http://www.w3.org/2001/XMLSchema#simpleType")}))
      (is (=
            (v n)
            (find-node (node-v n))
            {:literal/value "v",
              :literal/datatype (uri "http://www.w3.org/2001/XMLSchema#simpleType")}))
      (is (= (node-l n) <leaf>))
      (is (nil? (l <leaf>)))
      (is (= (node-r n) <leaf>))
      (is (nil? (r <leaf>)))
      (is (= (w n) (node-w n) 1))
      (is (= 0 (w <leaf>)))))
  (testing "round trip"   
    (let [vect [{:literal/value "k",
                  :literal/datatype (uri "http://www.w3.org/2001/XMLSchema#simpleType")}
                 {:literal/value "v",
                   :literal/datatype (uri "http://www.w3.org/2001/XMLSchema#simpleType")}
                 nil nil 1]]
      (is (= vect (find-node (node! vect)))))))

(deftest tree-node-destructuring-tests
  (is (=
        (node-kv    (node! (literal! "k") (literal! "v") nil nil nil))
        (kv [[k v] (node! (literal! "k") (literal! "v") nil nil nil)]
          (vector k v))
        [{:literal/value "k",
           :literal/datatype (uri "http://www.w3.org/2001/XMLSchema#simpleType")}
          {:literal/value "v",
            :literal/datatype (uri "http://www.w3.org/2001/XMLSchema#simpleType")}]))
  (is (=
        (node-lr    (node! (literal! "k") (literal! "v") nil nil nil))
        (lr [[l r] (node! (literal! "k") (literal! "v") nil nil nil)]
          (vector (find-node l) (find-node r)))
        [nil nil]))
  (is (=
        (node-kvlr  (node! (literal! "k") (literal! "v") nil nil nil))
        (kvlr [[k v l r] (node! (literal! "k") (literal! "v") nil nil nil)]
          (vector k v (find-node l) (find-node r)))
        [{:literal/value "k",
           :literal/datatype (uri "http://www.w3.org/2001/XMLSchema#simpleType")}
          {:literal/value "v",
            :literal/datatype (uri "http://www.w3.org/2001/XMLSchema#simpleType")}
          nil nil]))
  (is (=
        (node-kvlrw (node! (literal! "k") (literal! "v") nil nil nil))
        (kvlrw [[k v l r w] (node! (literal! "k") (literal! "v") nil nil nil)]
          (vector k v (find-node l) (find-node r) w))
        [{:literal/value "k",
           :literal/datatype (uri "http://www.w3.org/2001/XMLSchema#simpleType")}
          {:literal/value "v",
            :literal/datatype (uri "http://www.w3.org/2001/XMLSchema#simpleType")}
          nil nil 1])))
  
(deftest left-enum-tests
  (let [ln  (node! (literal! "ln.k") (literal! "ln.v") nil nil nil)
         rn (node! (literal! "rn.k") (literal! "rn.v") nil nil nil)
         n  (node! (literal! "n.k") (literal! "n.v") ln rn 3)]
    (is (=
          (mapv find-node [(literal! "ln.k") (literal! "ln.v")])
          (first (left-enum n))))
    (is (= <leaf> (second (left-enum n))))
    (is (=
          (mapv find-node [(literal! "n.k") (literal! "n.v")])
          (ffirst (drop 2 (left-enum n)))))
    (is (= rn (second (first (drop 2 (left-enum n))))))
    (is (nil? (first (drop 2 (first (drop 2 (left-enum n)))))))))


