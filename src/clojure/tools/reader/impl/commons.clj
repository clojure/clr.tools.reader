;;   Copyright (c) Nicola Mometto, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.tools.reader.impl.commons
  (:refer-clojure :exclude [char])
  (:require [clojure.tools.reader.reader-types :refer [peek-char read-char reader-error]]
            [clojure.tools.reader.impl.utils :refer [numeric? newline? char]])
  (:import (clojure.lang BigInt Numbers   JReMatcher)                                 ;;; Added JReMatcher
           (System.Text.RegularExpressions Regex)                                     ;;; (java.util.regex Pattern Matcher)
           ))                                                                         ;;; java.lang.reflect.Constructor

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn number-literal?
  "Checks whether the reader is at the start of a number literal"
  [reader initch]
  (or (numeric? initch)
      (and (or (identical? \+ initch) (identical?  \- initch))
           (numeric? (peek-char reader)))))

(defn read-past
  "Read until first character that doesn't match pred, returning
   char."
  [pred rdr]
  (loop [ch (read-char rdr)]
    (if (pred ch)
      (recur (read-char rdr))
      ch)))

(defn skip-line
  "Advances the reader to the end of a line. Returns the reader"
  [reader]
  (loop []
    (when-not (newline? (read-char reader))
      (recur)))
  reader)

(def ^Regex int-pattern #"([-+]?)(?:(0)|([1-9][0-9]*)|0[xX]([0-9A-Fa-f]+)|0([0-7]+)|([1-9][0-9]?)[rR]([0-9A-Za-z]+)|0[0-9]+)(N)?")   ;;; ^Pattern
(def ^Regex ratio-pattern #"([-+]?[0-9]+)/([0-9]+)")                                                                                 ;;; ^Pattern
(def ^Regex float-pattern #"([-+]?[0-9]+(\.[0-9]*)?([eE][-+]?[0-9]+)?)(M)?")                                                         ;;; ^Pattern

(defn- match-int
  [^JReMatcher m]                                                                    ;;; ^Matcher
  (if (.group m 2)
    (if (.group m 8) 0N 0)
    (let [negate? (= "-" (.group m 1))
          a (cond
             (.group m 3) [(.group m 3) 10]
             (.group m 4) [(.group m 4) 16]
             (.group m 5) [(.group m 5) 8]
             (.group m 7) [(.group m 7) (Int32/Parse (.group m 6))]                  ;;;  Integer/parseInt
             :else        [nil nil])
          ^String n (a 0)]
      (when n
        (let [bn (BigInteger. n (int (a 1)))
              bn (if negate? (.Negate bn) bn)]                                       ;;; .negate
          (if (.group m 8)
            (BigInt/fromBigInteger bn)
            (let [lv 0 ]                                                             ;;; (if (< (.bitLength bn) 64)
               (if (.AsInt64 bn (by-ref ^long lv))  lv                               ;;;    (.longValue bn) )
                 (BigInt/fromBigInteger bn)))))))))

(defn- match-ratio
  [^JReMatcher m]                                                                    ;;; ^Matcher
  (let [^String numerator (.group m 1)
        ^String denominator (.group m 2)
        numerator (if (.StartsWith numerator "+")                                    ;;; .startsWith
                    (subs numerator 1)
                    numerator)]
    (/ (-> numerator   BigInteger. BigInt/fromBigInteger Numbers/ReduceBigInt)        ;;; reduceBigInt
       (-> denominator BigInteger. BigInt/fromBigInteger Numbers/ReduceBigInt))))     ;;; reduceBigInt

(defn- match-float
  [^String s ^JReMatcher m]                                                          ;;; ^Matcher
  (if (.group m 4)
    (BigDecimal/Create ^String (.group m 1))                                         ;;; BigDecimal.
    (Double/Parse s)))                                                               ;;; Double/parseDouble

(defn match-number [^String s]
  (let [int-matcher (JReMatcher. int-pattern s)]                                     ;;; .matcher
    (if (.matches int-matcher)
      (match-int int-matcher)
      (let [float-matcher (JReMatcher. float-pattern s)]                             ;;; .matcher
        (if (.matches float-matcher)
          (match-float s float-matcher)
          (let [ratio-matcher (JReMatcher. ratio-pattern s)]                         ;;; .matcher
            (when (.matches ratio-matcher)
              (match-ratio ratio-matcher))))))))

(defn parse-symbol
  "Parses a string into a vector of the namespace and symbol"
  [^String token]
  (when-not (or (= "" token)
                (.EndsWith token ":")                                                 ;;; .endsWith
                (.StartsWith token "::"))                                             ;;; .startsWith
    (let [ns-idx (.IndexOf token "/")]                                                ;;; .indexOf
      (if-let [^String ns (and (pos? ns-idx)
                               (subs token 0 ns-idx))]
        (let [ns-idx (inc ns-idx)]
          (when-not (== ns-idx (count token))
            (let [sym (subs token ns-idx)]
              (when (and (not (numeric? (nth sym 0)))
                         (not (= "" sym))
                         (not (.EndsWith ns ":"))                                      ;;; .endsWith 
                         (or (= sym "/")
                             (== -1 (.IndexOf sym "/"))))                              ;;; .indexOf
                [ns sym]))))
        (when (or (= token "/")
                  (== -1 (.IndexOf token "/")))                                        ;;; .indexOf
          [nil token])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; readers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn read-comment
  [rdr & _]
  (skip-line rdr))

(defn throwing-reader
  [msg]
  (fn [rdr & _]
    (reader-error rdr msg)))
