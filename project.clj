(defproject datomic-rdf "0.1.0-SNAPSHOT"
  :description "A straightforward RDF graph model for Datomic"
  :url "http://github.com/danlentz/datomic-rdf"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                  [org.clojure/core.logic "0.8.4"]
                  [org.clojure/core.memoize "0.5.6"]
                  [com.datomic/datomic-free  "0.8.4143"]
                  [datomic-schema            "1.0.2"]
                  [tailrecursion/monocopy "1.0.9"]
                  [uk.org.russet/tawny-owl "1.0-SNAPSHOT"]
                  [org.flatland/useful "0.10.3"]
                  [environ "0.4.0"]
                  [org.bovinegenius/exploding-fish "0.3.3"]
                  [dispatch-map "0.2.0"]]
  :plugins [[codox "0.6.4"]]
  :codox {:src-dir-uri "http://github.com/danlentz/datomic-rdf/blob/master"
           :src-linenum-anchor-prefix "L"})

  




 ;; [org.clojars.quoll/turtle "0.1.2"]
 ;;                 [adi "0.1.5"]
 ;;                 [fipp "0.4.0"]
 ;;                 [clj-http "0.7.6"]
 ;;                 [serializable-fn "1.1.3"]
 ;;                  [plaza "0.0.5-SNAPSHOT"]
 ;;                 [org.clojure/data.finger-tree "0.0.1"]
 ;;                 [tentacles "0.2.6"]                  
 ;;                 [org.clojure/tools.logging "0.2.6"]
 ;;                 [org.clojure/data.codec "0.1.0"]
 ;;                 [zololabs/demonic "0.1.0-SNAPSHOT"]
 ;;                  [vijual "0.1.0-SNAPSHOT"]
 ;;                 [net.cgrand/parsley "0.9.2"]
 ;;                  [net.cgrand/regex "1.1.0"]
 ;;                 [fafnir "1.0.2"]
 ;;                 [midje "1.5.1"]
 ;;                                        [serializable-fn "1.1.3"]                 
 ;;                                        [seabass             "2.1.1"]]
 ;;  :plugins [[codox"0.6.4"]]
 ;;  :codox {:src-dir-uri "http://github.com/danlentz/datomic-rdf/blob/master"
 ;;          :src-linenum-anchor-prefix "L"})

