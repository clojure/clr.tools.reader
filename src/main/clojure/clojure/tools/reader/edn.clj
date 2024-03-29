;;   Copyright (c) Nicola Mometto, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "An EDN reader in clojure"
      :author "Bronsa"}
  clojure.tools.reader.edn
  (:refer-clojure :exclude [read read-string char default-data-readers])
  (:require [clojure.tools.reader.reader-types :refer
             [read-char unread peek-char indexing-reader?
              get-line-number get-column-number get-file-name string-push-back-reader]]
            [clojure.tools.reader.impl.utils :refer
             [char ex-info? whitespace? numeric? desugar-meta namespace-keys second'  char-value-in-radix ]]   ;;; DM: Added char-value-in-radix 
            [clojure.tools.reader.impl.commons :refer :all]
            [clojure.tools.reader.impl.errors :as err]		
            [clojure.tools.reader :refer [default-data-readers]])
  (:import (clojure.lang PersistentHashSet IMeta RT PersistentVector)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare read macros dispatch-macros)

(defn- macro-terminating? [ch]
  (and (not (identical? \# ch))
       (not (identical? \' ch))
       (not (identical? \: ch))
       (macros ch)))

(defn- not-constituent? [ch]
  (or (identical? \@ ch)
      (identical? \` ch)
      (identical? \~ ch)))

;; Some serious hacking on read-token to make it work with |-quoting on symbols.

;; This was the loop after all the leading-character tests in read-token.
;; Now used for anything other than :symbol
(defn- ^String read-token-not-symbol
  [rdr kind initch validate-leading?]
  (loop [sb (StringBuilder.)
         ch initch]
    (if (or (whitespace? ch)
            (macro-terminating? ch)
            (nil? ch))
	  (do (unread rdr ch)
          (str sb))
      (if (not-constituent? ch)
        (err/throw-bad-char rdr kind ch)
        (recur (doto sb (.Append ch)) (read-char rdr))))))
  
 ;; This version allows for |-quoting. 
  
(defn- read-token-symbol  
  [rdr kind initch validate-leading?]
  (let [rawMode (= initch \|)
        sb (StringBuilder.) 
		startch (if rawMode (read-char rdr) initch)]
    (when rawMode
	    (.Append sb initch))
    (loop [sb sb ch startch rawMode rawMode]
      (when (and rawMode (nil? ch))
	    (err/throw-eof-reading rdr :symbol sb))  
	  (if rawMode
	    (cond 
	      (nil? ch)
		  (err/throw-eof-reading rdr :symbol sb)
	      (and (= ch \|) (= (peek-char rdr) '\|))   ;; || in raw mode, eat both
	      (do (read-char rdr)                       ;; eat the second |
		      (recur (.Append sb "||") (read-char rdr) (boolean true)))
	      :else (recur (.Append sb ch) (read-char rdr) (boolean (not= ch \|))))
	    (if (or (whitespace? ch)
                (macro-terminating? ch)
                (nil? ch))
          (do (when ch
                (unread rdr ch))
              (str sb))
		  (if (not-constituent? ch)
            (err/throw-bad-char rdr kind ch)	  
            (recur (.Append sb ch) (read-char rdr) rawMode)))))))
  
(defn- ^String read-token
  ([rdr kind initch]
     (read-token rdr kind initch true))

  ([rdr kind initch validate-leading?]
     (cond
      (not initch)
      (err/throw-eof-at-start rdr kind)

      (and validate-leading?
           (not-constituent? initch))
      (err/throw-bad-char rdr kind initch)
       
	  (= kind :symbol)
	  (read-token-symbol  rdr kind initch validate-leading?)
	  
      :else
	  (read-token-not-symbol rdr kind initch validate-leading?))))



(declare read-tagged)

(defn- read-dispatch
  [rdr _ opts]
  (if-let [ch (read-char rdr)]
    (if-let [dm (dispatch-macros ch)]
      (dm rdr ch opts)
      (read-tagged (doto rdr (unread ch)) ch opts))
    (err/throw-eof-at-dispatch rdr)))

(defn- read-unmatched-delimiter
  [rdr ch opts]
  (err/throw-unmatch-delimiter rdr ch))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; readers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn- read-unicode-char
  ([^String token ^long offset ^long length ^long base]
   (let [l (+ offset length)]
     (when-not (== (count token) l)
       (err/throw-invalid-unicode-literal nil token))
     (loop [i offset uc 0]
       (if (== i l)
         (char uc)
         (let [d (char-value-in-radix (int (nth token i)) (int base))]                          ;;; Character/digit
           (if (== d -1)
             (err/throw-invalid-unicode-digit-in-token nil (nth token i) token)
             (recur (inc i) (long (+ d (* uc base))))))))))

  ([rdr initch base length exact?]
   (let [length (long length)
         base (long base)]
     (loop [i 1 uc (char-value-in-radix (int initch) (int base))]                               ;;; Character/digit
       (if (== uc -1)
         (err/throw-invalid-unicode-digit rdr initch)
         (if-not (== i length)
           (let [ch (peek-char rdr)]
             (if (or (whitespace? ch)
                     (macros ch)
                     (nil? ch))
               (if exact?
                 (err/throw-invalid-unicode-len rdr i length)
                 (char uc))
               (let [d (char-value-in-radix (int ch) (int base))]                               ;;; Character/digit
                 (read-char rdr)
                 (if (== d -1)
                   (err/throw-invalid-unicode-digit rdr ch)
                   (recur (inc i) (long (+ d (* uc base))))))))
           (char uc)))))))

(def ^:private ^:const upper-limit (int \uD7ff))
(def ^:private ^:const lower-limit (int \uE000))

(defn- read-char*
  [rdr backslash opts]
  (let [ch (read-char rdr)]
    (if-not (nil? ch)
      (let [token (if (or (macro-terminating? ch)
                          (not-constituent? ch)
                          (whitespace? ch))
                    (str ch)
                    (read-token rdr :character ch false))
            token-len (count token)]
        (cond

         (== 1 token-len)  (char (nth token 0))                                                   ;;; Character/valueOf

         (= token "newline") \newline
         (= token "space") \space
         (= token "tab") \tab
         (= token "backspace") \backspace
         (= token "formfeed") \formfeed
         (= token "return") \return

         (.StartsWith token "u")                                                                   ;;; .startsWith
         (let [c (read-unicode-char token 1 4 16)
               ic (int c)]
           (if (and (> ic upper-limit)
                    (< ic lower-limit))
             (err/throw-invalid-character-literal rdr (.ToString ic "x"))                         ;;; (Integer/toString ic 16)
             c))

         (.StartsWith token "o")                                                                  ;;; .startsWith
         (let [len (dec token-len)]
           (if (> len 3)
             (err/throw-invalid-octal-len rdr token)
             (let [uc (read-unicode-char token 1 len 8)]
               (if (> (int uc) 0377)
                 (err/throw-bad-octal-number rdr)
                 uc))))

         :else (err/throw-unsupported-character rdr token)))
      (err/throw-eof-in-character rdr))))

(defn ^:private starting-line-col-info [rdr]
  (when (indexing-reader? rdr)
    [(get-line-number rdr) (int (dec (int (get-column-number rdr))))]))

(defn- ^PersistentVector read-delimited
  [kind delim rdr opts]
  (let [[start-line start-column] (starting-line-col-info rdr)
        delim (char delim)]
    (loop [a (transient [])]
      (let [ch (read-past whitespace? rdr)]
        (when-not ch
          (err/throw-eof-delimited rdr kind start-line start-column (count a)))

        (if (identical? delim (char ch))
          (persistent! a)
          (if-let [macrofn (macros ch)]
            (let [mret (macrofn rdr ch opts)]
              (recur (if-not (identical? mret rdr) (conj! a mret) a)))
            (let [o (read (doto rdr (unread ch)) true nil opts)]
              (recur (if-not (identical? o rdr) (conj! a o) a)))))))))

(defn- read-list
  [rdr _ opts]
  (let [the-list (read-delimited :list \) rdr opts)]
    (if (empty? the-list)
      '()
      (clojure.lang.PersistentList/create the-list))))

(defn- read-vector
  [rdr _ opts]
  (read-delimited :vector \] rdr opts))

(defn- read-map
  [rdr _ opts]
  (let [[start-line start-column] (starting-line-col-info rdr)
        coll (read-delimited :map \} rdr opts)
        l (to-array coll)]
    (when (== 1 (bit-and (alength l) 1))
      (err/throw-odd-map rdr start-line start-column coll))
    (RT/map l)))

(defn- read-number
  [rdr initch opts]
  (loop [sb (doto (StringBuilder.) (.Append initch))                                   ;;; .append
         ch (read-char rdr)]
    (if (or (whitespace? ch) (macros ch) (nil? ch))
      (let [s (str sb)]
        (unread rdr ch)
        (or (match-number s)
            (err/throw-invalid-number rdr s)))
      (recur (doto sb (.Append ch)) (read-char rdr)))))                                ;;; .append


(defn- escape-char [sb rdr]
  (let [ch (read-char rdr)]
    (case ch
      \t "\t"
      \r "\r"
      \n "\n"
      \\ "\\"
      \" "\""
      \b "\b"
      \f "\f"
      \u (let [ch (read-char rdr)]
           (if (== -1 (char-value-in-radix (int ch) 16))                               ;;; Character/digit
            (err/throw-invalid-unicode-escape rdr ch)
             (read-unicode-char rdr ch 16 4 true)))
      (if (numeric? ch)
        (let [ch (read-unicode-char rdr ch 8 3 false)]
          (if (> (int ch) 0377)
            (err/throw-bad-octal-number rdr)
            ch))
        (err/throw-bad-escape-char rdr ch)))))

(defn- read-string*
  [rdr _ opts]
  (loop [sb (StringBuilder.)
         ch (read-char rdr)]
    (case ch
      nil (err/throw-eof-reading rdr :string \" sb)
      \\ (recur (doto sb (.Append (escape-char sb rdr)))                               ;;; .append
                (read-char rdr))
      \" (str sb)
      (recur (doto sb (.Append ch)) (read-char rdr)))))                                ;;; .append

(defn- read-symbol
  [rdr initch]
  (when-let [token (read-token rdr :symbol initch)]
    (case token

      ;; special symbols
      "nil" nil
      "true" true
      "false" false
      "/" '/

      (or (when-let [p (parse-symbol token)]
            (symbol (p 0) (p 1)))
          (err/throw-invalid rdr :symbol token)))))

(defn- read-keyword
  [reader initch opts]
  (let [ch (read-char reader)]
    (if-not (whitespace? ch)
      (let [token (read-token reader :keyword ch)
            s (parse-symbol token)]
        (if (and s (== -1 (.IndexOf token "::")))                                          ;;; .indexOf
          (let [^String ns (s 0)
                ^String name (s 1)]
            (if (identical? \: (nth token 0))
              (err/throw-invalid reader :keyword (str \: token)) ; No ::kw in edn.
              (keyword ns name)))
          (err/throw-invalid reader :keyword (str \: token))))
      (err/throw-single-colon reader))))
	  
(defn- wrapping-reader
  [sym]
  (fn [rdr _ opts]
    (list sym (read rdr true nil opts))))

(defn- read-meta
  [rdr _ opts]
  (let [m (desugar-meta (read rdr true nil opts))]
    (when-not (map? m)
      (err/throw-bad-metadata rdr m))
	  
    (let [o (read rdr true nil opts)]
      (if (instance? IMeta o)
        (with-meta o (merge (meta o) m))
        (err/throw-bad-metadata-target rdr o)))))

(defn- read-set
  [rdr _ opts]
  (PersistentHashSet/createWithCheck (read-delimited :set \} rdr opts)))

(defn- read-discard
  [rdr _ opts]
  (doto rdr
    (read true nil true)))

(defn- read-namespaced-map
  [rdr _ opts]
  (let [token (read-token rdr :namespaced-map (read-char rdr))]
    (if-let [ns (some-> token parse-symbol second)]
      (let [ch (read-past whitespace? rdr)]
        (if (identical? ch \{)
          (let [items (read-delimited  :namespaced-map \} rdr opts)]
            (when (odd? (count items))
              (err/throw-odd-map rdr nil nil items))
            (let [keys (take-nth 2 items)
                  vals (take-nth 2 (rest items))]
              (RT/map (to-array (mapcat list (namespace-keys (str ns) keys) vals)))))
          (err/throw-ns-map-no-map rdr token)))
      (err/throw-bad-ns rdr token))))

(defn- read-symbolic-value
  [rdr _ opts]
  (let [sym (read rdr true nil opts)]
    (case sym
      Inf Double/PositiveInfinity                                ;;; Double/POSITIVE_INFINITY
      -Inf Double/NegativeInfinity                               ;;; Double/NEGATIVE_INFINITY
      NaN Double/NaN
      (err/reader-error rdr (str "Invalid token: ##" sym)))))
	  
(defn- macros [ch]
  (case ch
    \" read-string*
    \: read-keyword
    \; read-comment
    \^ read-meta
    \( read-list
    \) read-unmatched-delimiter
    \[ read-vector
    \] read-unmatched-delimiter
    \{ read-map
    \} read-unmatched-delimiter
    \\ read-char*
    \# read-dispatch
    nil))

(defn- dispatch-macros [ch]
  (case ch
    \^ read-meta                ;deprecated
    \{ read-set
    \< (throwing-reader "Unreadable form")
    \! read-comment
    \_ read-discard
    \: read-namespaced-map
	\# read-symbolic-value
    nil))

(defn- read-tagged [rdr initch opts]
  (let [tag (read rdr true nil opts)
        object (read rdr true nil opts)]
    (if-not (symbol? tag)
      (err/throw-bad-reader-tag rdr "Reader tag must be a symbol"))
    (if-let [f (or (get (:readers opts) tag)
                   (default-data-readers tag))]
      (f object)
      (if-let [d (:default opts)]
        (d tag object)
        (err/throw-unknown-reader-tag rdr tag)))))
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn read
  "Reads the first object from an IPushbackReader or a java.io.PushbackReader.
   Returns the object read. If EOF, throws if eof-error? is true otherwise returns eof.
   If no reader is provided, *in* will be used.

   Reads data in the edn format (subset of Clojure data):
   http://edn-format.org

   clojure.tools.reader.edn/read doesn't depend on dynamic Vars, all configuration
   is done by passing an opt map.

   opts is a map that can include the following keys:
   :eof - value to return on end-of-file. When not supplied, eof throws an exception.
   :readers  - a map of tag symbols to data-reader functions to be considered before default-data-readers.
              When not supplied, only the default-data-readers will be used.
   :default - A function of two args, that will, if present and no reader is found for a tag,
              be called with the tag and the value."
  ([] (read *in*))
  ([reader] (read {} reader))
  ([{:keys [eof] :as opts} reader]
     (let [eof-error? (not (contains? opts :eof))]
       (read reader eof-error? eof opts)))
  ([reader eof-error? eof opts]
     (try
       (loop []
         (let [ch (read-char reader)]
           (cond
            (whitespace? ch) (recur)
            (nil? ch) (if eof-error? (err/throw-eof-error reader nil) eof)
            (number-literal? reader ch) (read-number reader ch opts)
            :else (let [f (macros ch)]
                    (if f
                      (let [res (f reader ch opts)]
                        (if (identical? res reader)
                          (recur)
                          res))
                      (read-symbol reader ch))))))
       (catch Exception e
         (if (ex-info? e)
           (let [d (ex-data e)]
             (if (= :reader-exception (:type d))
               (throw e)
               (throw (ex-info (.Message e)                                           ;;; .getMessage
                               (merge {:type :reader-exception}
                                      d
                                      (if (indexing-reader? reader)
                                        {:line   (get-line-number reader)
                                         :column (get-column-number reader)
                                         :file   (get-file-name reader)}))
                               e))))
           (throw (ex-info (.Message e)                                               ;;; .getMessage
                           (merge {:type :reader-exception}
                                  (if (indexing-reader? reader)
                                    {:line   (get-line-number reader)
                                     :column (get-column-number reader)
                                     :file   (get-file-name reader)}))
                           e)))))))

(defn read-string
  "Reads one object from the string s.
   Returns nil when s is nil or empty.

   Reads data in the edn format (subset of Clojure data):
   http://edn-format.org

   opts is a map as per clojure.tools.reader.edn/read"
  ([s] (read-string {:eof nil} s))
  ([opts s]
     (when (and s (not (identical? s "")))
       (read opts (string-push-back-reader s)))))
