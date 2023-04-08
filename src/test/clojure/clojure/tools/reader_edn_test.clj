(ns clojure.tools.reader-edn-test
  (:refer-clojure :exclude [read-string])
  (:use [clojure.tools.reader.edn :as edn :only [read-string]]
        [clojure.test :only [deftest is testing]])
  (:import clojure.lang.BigInt))

(load "common_tests")

(deftest read-keyword
  (is (= :foo-bar (read-string ":foo-bar")))
  (is (= :foo/bar (read-string ":foo/bar")))
  (is (= :*+!-_? (read-string ":*+!-_?")))
  (is (= :abc:def:ghi (read-string ":abc:def:ghi")))
  (is (= :abc.def/ghi (read-string ":abc.def/ghi")))
  (is (= :abc/def.ghi (read-string ":abc/def.ghi")))
  (is (= :abc:def/ghi:jkl.mno (read-string ":abc:def/ghi:jkl.mno")))
  (is (instance? clojure.lang.Keyword (read-string ":alphabet"))) )

(deftest read-tagged
  ;; (is (= #inst "2010-11-12T13:14:15.666"
  ;;        (read-string "#inst \"2010-11-12T13:14:15.666\"")))
  ;; (is (= #inst "2010-11-12T13:14:15.666"
  ;;        (read-string "#inst\"2010-11-12T13:14:15.666\"")))
  ;; (is (= #uuid "550e8400-e29b-41d4-a716-446655440000"
  ;;        (read-string "#uuid \"550e8400-e29b-41d4-a716-446655440000\"")))
  ;; (is (= #uuid "550e8400-e29b-41d4-a716-446655440000"
  ;;        (read-string "#uuid\"550e8400-e29b-41d4-a716-446655440000\"")))
  (is (= (System.Guid. "550e8400-e29b-41d4-a716-446655440000")                  ;;; java.util.UUID/fromString
         (read-string "#uuid \"550e8400-e29b-41d4-a716-446655440000\"")))
  (is (= (System.Guid. "550e8400-e29b-41d4-a716-446655440000")                  ;;; java.util.UUID/fromString
         (read-string "#uuid\"550e8400-e29b-41d4-a716-446655440000\"")))
  (let [my-unknown (fn [tag val] {:unknown-tag tag :value val})]
    (is (= {:unknown-tag 'foo :value 'bar}
           (read-string {:default my-unknown} "#foo bar")))))
		 
(deftest pushback-reader-test
  (testing "TRDR-63"
    (is (= '(+) (edn/read (clojure.lang.PushbackTextReader. (System.IO.StringReader. "(+)")))))))		          ;;; java.io.PushbackReader.  java.io.StringReader.
