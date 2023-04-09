;;   Copyright (c) Nicola Mometto, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.tools.reader.impl.commons
  (:refer-clojure :exclude [char])
  (:require [clojure.tools.reader.reader-types :refer [peek-char read-char]]
            [clojure.tools.reader.impl.errors :refer [reader-error]]
            [clojure.tools.reader.impl.utils :refer [numeric? newline? char normalized-re-group char-value-in-radix]])  ;;; DM: Added char-value-in-radix normalized-re-group
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

(def ^Regex int-pattern #"^([-+]?)(?:(0)|([1-9][0-9]*)|0[xX]([0-9A-Fa-f]+)|0([0-7]+)|([1-9][0-9]?)[rR]([0-9A-Za-z]+)|0[0-9]+)(N)?$")   ;;; ^Pattern  &  add ^ $ around
(def ^Regex ratio-pattern #"^([-+]?[0-9]+)/([0-9]+)$")                                                                                 ;;; ^Pattern  &  add ^ $ around
(def ^Regex float-pattern #"^([-+]?[0-9]+(\.[0-9]*)?([eE][-+]?[0-9]+)?)(M)?$")                                                         ;;; ^Pattern  &  add ^ $ around

(defn- match-int
  [^JReMatcher m]                                                                    ;;; ^Matcher
  (if (normalized-re-group m 2)                                                      ;;; .group
    (if (normalized-re-group m 8) 0N 0)                                              ;;; .group
    (let [negate? (= "-" (normalized-re-group m 1))                                  ;;; .group
          a (cond
             (normalized-re-group m 3) [(normalized-re-group m 3) 10]                                              ;;; .group
             (normalized-re-group m 4) [(normalized-re-group m 4) 16]                                              ;;; .group
             (normalized-re-group m 5) [(normalized-re-group m 5) 8]                                               ;;; .group
             (normalized-re-group m 7) [(normalized-re-group m 7) (Int32/Parse (normalized-re-group m 6))]         ;;;  .group Integer/parseInt
             :else        [nil nil])
          ^String n (a 0)]
      (when n
        (let [bn (BigInteger/Parse n (int (a 1)))                                    ;;; BigInteger.
              bn (if negate? (.Negate bn) bn)]                                       ;;; .negate
          (if (normalized-re-group m 8)                                              ;;; .group
            (BigInt/fromBigInteger bn)
            (let [lv 0 ]                                                             ;;; (if (< (.bitLength bn) 64)
               (if (.AsInt64 bn (by-ref ^long lv))  lv                               ;;;    (.longValue bn) )
                 (BigInt/fromBigInteger bn)))))))))

(defn- match-ratio
  [^JReMatcher m]                                                                    ;;; ^Matcher
  (let [^String numerator (normalized-re-group m 1)                                  ;;; .group
        ^String denominator (normalized-re-group m 2)                                ;;; .group
        numerator (if (.StartsWith numerator "+")                                    ;;; .startsWith
                    (subs numerator 1)
                    numerator)]
    (/ (-> numerator   BigInteger/Parse BigInt/fromBigInteger Numbers/ReduceBigInt)        ;;; BigInteger.  reduceBigInt
       (-> denominator BigInteger/Parse BigInt/fromBigInteger Numbers/ReduceBigInt))))     ;;; BigInteger.  reduceBigInt

(defn- match-float
  [^String s ^JReMatcher m]                                                          ;;; ^Matcher
  (if (normalized-re-group m 4)                                                      ;;; .group
    (BigDecimal/Create ^String (normalized-re-group m 1))                            ;;; BigDecimal.    .group
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
		   
;; Significant hacking on parse-symbol to deal with |-quoting.

;; The original parse-symbol can be used when there are no |'s in the token.

(defn- parse-symbol-unquoted
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

(defn- decode-quoted-token
  "Returns a 'masked' version of the token with all quoted characters replaced by 'a' and the raw version with all the |'s removed (handling || properly)"
  [^String token]
  (let [sbMasked (StringBuilder.) 
        sbRaw (StringBuilder.)]
    (loop [i 0 rawMode false]
	  (cond 
	    (>= i (.Length token))
	    [(str sbMasked) (str sbRaw)]
		rawMode 
		(if (= (.get_Chars token i) \|)
          (if (and (< i (dec (.Length token))) (= (.get_Chars token (inc i)) \|))
		    (do (.Append sbMasked \a)
		        (.Append sbRaw \|)
			    (recur (+ i 2) rawMode))
		    (recur (+ i 1) false))
		  (do (.Append sbMasked \a)
		      (.Append sbRaw (.get_Chars token i))
			  (recur (+ i 1) rawMode)))
	    (= (.get_Chars token i) \|)
		  (recur (+ i 1) true)
		:else
		(do (.Append sbMasked (.get_Chars token i))
		    (.Append sbRaw (.get_Chars token i))
			(recur (+ i 1) rawMode))))))
			
(defn- parse-symbol-quoted
  [^String token]
  (let [[masked raw] (decode-quoted-token token)]
    (let [result (parse-symbol-unquoted masked)]
	  (when result
	     (let [[^String ns sym] result] 
	       (if (nil? ns)
	         [nil raw]
		     [(.Substring ^String raw 0 (.Length ns))
              (.Substring ^String raw (inc (.Length ns)))]))))))
	 
		  
(defn parse-symbol
  "Parses a string into a vector of the namespace and symbol"
  [^String token] 
  (if (.Contains token "|") 
    (parse-symbol-quoted token)
	(parse-symbol-unquoted token)))

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
