(ns clojure.tools.reader-test
  (:refer-clojure :exclude [read read-string *default-data-reader-fn* *data-readers*])
  (:use [clojure.tools.reader :only [read read-string *default-data-reader-fn* *data-readers*]]
        [clojure.tools.reader.reader-types :only [string-push-back-reader
                                                  indexing-push-back-reader]]
        [clojure.test :only [deftest is are testing]]
        [clojure.tools.reader.impl.utils :exclude [char]])
  (:require [clojure.tools.reader.edn :as tre])
  (:import clojure.lang.BigInt
           (System.IO StringReader)                             ;;; (java.io StringReader BufferedReader)
           clojure.lang.LineNumberingTextReader))               ;;; LineNumberingPushbackReader

(load "common_tests")

(deftest read-keyword
  (is (= :foo-bar (read-string ":foo-bar")))
  (is (= :foo/bar (read-string ":foo/bar")))
  (is (= :user/foo-bar (binding [*ns* (the-ns 'user)]
                         (read-string "::foo-bar"))))
  (is (= :clojure.core/foo-bar
         (do (alias 'core 'clojure.core)
             (read-string "::core/foo-bar"))))
  (is (= :*+!-_? (read-string ":*+!-_?")))
  (is (= :abc:def:ghi (read-string ":abc:def:ghi")))
  (is (= :abc.def/ghi (read-string ":abc.def/ghi")))
  (is (= :abc/def.ghi (read-string ":abc/def.ghi")))
  (is (= :abc:def/ghi:jkl.mno (read-string ":abc:def/ghi:jkl.mno")))
  (is (instance? clojure.lang.Keyword (read-string ":alphabet"))) )

(deftest read-regex
  (is (= (str #"\[\]?(\")\\")
         (str (read-string "#\"\\[\\]?(\\\")\\\\\"")))))

(deftest read-quote
  (is (= ''foo (read-string "'foo"))))

(deftest read-syntax-quote
  (is (= '`user/foo (binding [*ns* (the-ns 'user)]
                      (read-string "`foo"))))
  (is (= () (eval (read-string "`(~@[])"))))
  (is (= '`+ (read-string "`+")))
  (is (= '`foo/bar (read-string "`foo/bar")))
  (is (= '`1 (read-string "`1")))
  (is (= `(1 (~2 ~@'(3))) (eval (read-string "`(1 (~2 ~@'(3)))")))))

(deftest read-deref
  (is (= '@foo (read-string "@foo"))))

(deftest read-var
  (is (= '(var foo) (read-string "#'foo"))))

(deftest read-fn
  (is (= '(fn* [] (foo bar baz)) (read-string "#(foo bar baz)"))))

(deftest read-arg
  (is (= 14 ((eval (read-string "#(apply + % %1 %3 %&)")) 1 2 3 4 5)))
  (is (= 4 ((eval (read-string "#(last %&)")) 1 2 3 4))))

(deftest read-eval
  (is (= 3 (read-string "#=(+ 1 2)"))))

(deftest read-tagged
  ;; (is (= #inst "2010-11-12T13:14:15.666"
  ;;        (read-string "#inst \"2010-11-12T13:14:15.666\"")))
  ;; (is (= #inst "2010-11-12T13:14:15.666"
  ;;        (read-string "#inst\"2010-11-12T13:14:15.666\"")))
  ;; (is (= #uuid "550e8400-e29b-41d4-a716-446655440000"
  ;;        (read-string "#uuid \"550e8400-e29b-41d4-a716-446655440000\"")))
  ;; (is (= #uuid "550e8400-e29b-41d4-a716-446655440000"
  ;;        (read-string "#uuid\"550e8400-e29b-41d4-a716-446655440000\"")))
  (is (= (System.Guid. "550e8400-e29b-41d4-a716-446655440000")                               ;;; java.util.UUID/fromString
         (read-string "#uuid \"550e8400-e29b-41d4-a716-446655440000\"")))
  (is (= (System.Guid. "550e8400-e29b-41d4-a716-446655440000")                               ;;; java.util.UUID/fromString
                  (read-string "#uuid\"550e8400-e29b-41d4-a716-446655440000\"")))
  (when *default-data-reader-fn*
    (let [my-unknown (fn [tag val] {:unknown-tag tag :value val})]
      (is (= {:unknown-tag 'foo :value 'bar}
             (binding [*default-data-reader-fn* my-unknown]
               (read-string "#foo bar")))))))

(defrecord foo [])
(defrecord bar [baz buz])

(deftest read-record
  (is (= (foo.) (read-string "#clojure.tools.reader_test.foo[]")))
  (is (= (foo.) (read-string "#clojure.tools.reader_test.foo []"))) ;; not valid in clojure
  (is (= (foo.) (read-string "#clojure.tools.reader_test.foo{}")))
  (is (= (assoc (foo.) :foo 'bar) (read-string "#clojure.tools.reader_test.foo{:foo bar}")))

  (is (= (map->bar {}) (read-string "#clojure.tools.reader_test.bar{}")))
  (is (= (bar. 1 nil) (read-string "#clojure.tools.reader_test.bar{:baz 1}")))
  (is (= (bar. 1 nil) (read-string "#clojure.tools.reader_test.bar[1 nil]")))
  (is (= (bar. 1 2) (read-string "#clojure.tools.reader_test.bar[1 2]"))))

(deftest read-ctor
  (is (= "CCC" (read-string "#System.String[\\C 3]"))))                                        ;;; java.lang.String  "foo"   \"foo\"  added int arg

(defrecord JSValue [v])

(deftest reader-conditionals
  (let [opts {:read-cond :allow :features #{:cljr}}]
    (are [out s opts] (= out (read-string opts s))
         ;; basic read-cond
         '[foo-form] "[#?(:foo foo-form :bar bar-form)]" {:read-cond :allow :features #{:foo}}
         '[bar-form] "[#?(:foo foo-form :bar bar-form)]" {:read-cond :allow :features #{:bar}}
         '[foo-form] "[#?(:foo foo-form :bar bar-form)]" {:read-cond :allow :features #{:foo :bar}}
         '[] "[#?(:foo foo-form :bar bar-form)]" {:read-cond :allow :features #{:baz}}
         'nil "#?(:default nil)" opts

         ;; environmental features
         "clojure" "#?(:cljr \"clojure\" :cljs \"clojurescript\" :default \"default\")"  opts

         ;; default features
         "default" "#?(:clj \"clr\" :cljs \"cljs\" :default \"default\")" opts                                 ;;; :cljr => :clj -- because we need to trigger default

         ;; splicing
         [] "[#?@(:cljr [])]" opts                                                                             ;;; :clj
         [:a] "[#?@(:cljr [:a])]" opts                                                                         ;;; :clj
         [:a :b] "[#?@(:cljr [:a :b])]" opts                                                                   ;;; :clj
         [:a :b :c] "[#?@(:cljr [:a :b :c])]" opts                                                             ;;; :clj

         ;; nested splicing
         [:a :b :c :d :e] "[#?@(:cljr [:a #?@(:cljr [:b #?@(:cljr [:c]) :d]):e])]" opts                        ;;; :clj
         '(+ 1 (+ 2 3)) "(+ #?@(:cljr [1 (+ #?@(:cljr [2 3]))]))" opts                                         ;;; :clj
         '(+ (+ 2 3) 1) "(+ #?@(:cljr [(+ #?@(:cljr [2 3])) 1]))" opts                                         ;;; :clj
         [:a [:b [:c] :d] :e] "[#?@(:cljr [:a [#?@(:cljr [:b #?@(:cljr [[:c]]) :d])] :e])]" opts               ;;; :clj

         ;; bypass unknown tagged literals
         [1 2 3] "#?(:cljs #js [1 2 3] :cljr [1 2 3])" opts                                                    ;;; :clj
         :clojure "#?(:foo #some.nonexistent.Record {:x 1} :cljr :clojure)" opts)                              ;;; :clj

    (are [re s opts] (is (thrown-with-msg? Exception re (read-string opts s)))                                 ;;; RuntimeException
         #"Features must be keywords" "#?((+ 1 2) :a)" opts
         #"even number of forms" "#?(:cljs :a :cljr)" opts                                                    ;;; :clj
         #"read-cond-splicing must implement" "(#?@(:cljr :a))" opts                                           ;;; :clj
         #"is reserved" "(#?@(:foo :a :else :b))" opts
         #"must be a list" "#?[:foo :a :else :b]" opts
         #"Conditional read not allowed" "#?[:cljr :a :default nil]" {:read-cond :BOGUS}                       ;;; :clj
         #"Conditional read not allowed" "#?[:cljr :a :default nil]" {}))                                      ;;; :clj
  (binding [*data-readers* {'js (fn [v] (JSValue. v) )}]
    (is (= (JSValue. [1 2 3])
           (read-string {:features #{:cljs} :read-cond :allow} "#?(:cljs #js [1 2 3] :foo #foo [1])")))))

(deftest preserve-read-cond
  (is (= 1 (binding [*data-readers* {'foo (constantly 1)}]
             (read-string {:read-cond :preserve} "#foo []"))))

  (let [x (read-string {:read-cond :preserve} "#?(:clj foo :cljs bar)")]
    (is (reader-conditional? x))
    (is (= x (reader-conditional '(:clj foo :cljs bar) false)))
    (is (not (:splicing? x)))
    (is (= :foo (get x :no-such-key :foo)))
    (is (= (:form x) '(:clj foo :cljs bar))))
  (let [x (first (read-string {:read-cond :preserve} "(#?@(:clj [foo]))"))]
    (is (reader-conditional? x))
    (is (= x (reader-conditional '(:clj [foo]) true)))
    (is (:splicing? x))
    (is (= :foo (get x :no-such-key :foo)))
    (is (= (:form x) '(:clj [foo]))))
  (is (thrown-with-msg? Exception #"No reader function for tag"                          ;;; RuntimeException
                        (read-string {:read-cond :preserve} "#js {:x 1 :y 2}" )))
  (let [x (read-string {:read-cond :preserve} "#?(:cljs #js {:x 1 :y 2})")
        [platform tl] (:form x)]
    (is (reader-conditional? x))
    (is (tagged-literal? tl))
    (is (= tl (tagged-literal 'js {:x 1 :y 2})))
    (is (= 'js (:tag tl)))
    (is (= {:x 1 :y 2} (:form tl)))
    (is (= :foo (get tl :no-such-key :foo))))
  (testing "print form roundtrips"
    (doseq [s ["#?(:clj foo :cljs bar)"
               "#?(:cljs #js {:x 1, :y 2})"
               "#?(:clj #clojure.test_clojure.reader.TestRecord [42 85])"]]
      (is (= s (pr-str (read-string {:read-cond :preserve} s)))))))

(alias 'c.c 'clojure.core)

(deftest read-namespaced-map
  (binding [*ns* (the-ns 'clojure.tools.reader-test)]
    (is (= {::foo 1} (read-string "#::{:foo 1}")))
    (is (= {::foo 1 :bar 2} (read-string "#::{:foo 1 :_/bar 2}")))
    (is (= {:a/foo 1 :bar 2} (read-string "#:a{:foo 1 :_/bar 2}")))
    (is (= {:clojure.core/foo 2} (read-string "#::c.c{:foo 2}")))))


(defn multiple-reader-variants-from-string [s filename]
  [(-> (StringReader. s)
       (LineNumberingTextReader.)                        ;;; LineNumberingPushbackReader
       (indexing-push-back-reader 1 filename))
   (-> (StringReader. s)
                                                         ;;; (BufferedReader.) -- no equivalent
       (indexing-push-back-reader 1 filename))])

(defn first-reads-from-multiple-readers [s]
  (for [rdr (multiple-reader-variants-from-string s "file.edn")]
    (tre/read rdr)))

(deftest trdr-54
  (let [read-vals (mapcat first-reads-from-multiple-readers
                          ["[a\rb]" "[a\r b]" "[a \rb]"])]
    (doseq [pairs (partition 2 1 read-vals)]
      (is (= (first pairs) (second pairs))))))
	  
(deftest read-symbol
  (is (= 'foo (read-string "foo")))
  (is (= 'foo/bar (read-string "foo/bar")))
  (is (= '*+!-_? (read-string "*+!-_?")))
  (is (= 'abc:def:ghi (read-string "abc:def:ghi")))
  (is (= 'abc.def/ghi (read-string "abc.def/ghi")))
  (is (= 'abc/def.ghi (read-string "abc/def.ghi")))
  (is (= 'abc:def/ghi:jkl.mno (read-string "abc:def/ghi:jkl.mno")))
  (is (instance? clojure.lang.Symbol (read-string "alphabet")))
  (is (= "foo//" (str (read-string "foo//"))))
  (is (Double/IsNaN ^double (read-string "##NaN")))                  ;;; java.lang.Double/isNaN 
  (is (Double/IsInfinity ^double (read-string "##Inf")))             ;;; java.lang.Double/isInfinite
  (is (Double/IsInfinity ^double (read-string "##-Inf")))            ;;; java.lang.Double/isInfinite
  (testing "Correct array class symbols"
    (doseq [n (range 1 10)
            :let [sym (str "String/" n)
                  qsym (str "System.String/" n)]]                    ;;; "java.lang.String/"
      (let [rsym (read-string sym)
            rqsym (read-string qsym)]
        (is (= ((juxt namespace name) rsym)
               ["String" (str n)]))
        (is (= ((juxt namespace name) rqsym)
               ["System.String" (str n)])))))                        ;;; "java.lang.String"
  (testing "Correct prim array symbols"
    (doseq [prim ["int" "long" "boolean" "byte" "char" "double" "float" "short"]]
      (doseq [n (range 1 10)
              :let [sym (str prim "/" n)]]
        (let [rsym (read-string sym)]
          (is (= ((juxt namespace name) rsym)
                 [prim (str n)]))))))
  (testing "Incorrect Array class symbols"
    (doseq [suffix ["" "0" "11" "1a"]
            :let [sym (str "String/" suffix)]]
      (is (thrown? clojure.lang.ExceptionInfo (read-string sym)) sym))))	  