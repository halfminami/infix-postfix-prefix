(ns infix-postfix-prefix.tokenize
  (:require [infix-postfix-prefix.util :as util]
            [clojure.math :as math]))

(defmacro ^:private next-extractor
  "Parser template.
  
  `split` takes `s` and splits it into [fst rst] where `fst` is considered a
  token. If no token, returns logical false.

  `convert` takes `fst` and returns a value for a token. If not a token, returns
  logical false.

  `make` takes a return value from `convert` and always returns a token."
  [split convert make s]
  `(when-let [[fst# rst#] (~split ~s)]
     (when-let [d# (~convert fst#)]
       [(~make d#) rst#])))

(defn- first-rest
  "Returns [fst rst] if `coll` is non-empty. Used as `split` of single letter
  patterns for `make-next-extractor`. Thus `fst` is (index char)."
  [coll]
  (when-first [fst coll] [fst (rest coll)]))

(defn- first-rest-maps
  "Returns a function that takes (index char) and applies char to `map`. Used
  to create `convert` with `first-rest` for `make-next-extractor`."
  [m]
  (fn [[i ch]] (when-let [v (m ch)] [i v])))

;; record may be better?
(defn- base-token
  ([kind name start] (base-token kind name start "NONE"))
  ([kind name start s] {:kind kind, :name name, :start start, :string s}))

;; -----------------------------------------------------------------------------
;; <number>
;; -----------------------------------------------------------------------------

(defn- nonzero-digit? [ch] (<= (int \1) (int ch) (int \9)))

(defn- digit? [ch] (or (nonzero-digit? ch) (= 0 (compare ch \0))))

;; split only
(defn- split-first-neg [coll]
  (when-first [neg coll]
    (if (= (int \~) (int neg)) (split-at 1 coll) (split-at 0 coll))))

;; split and validate
(defn- neg-non-negative [coll]
  (when-let [[neg coll] (split-first-neg coll)]
    (and (every? digit? coll) [neg coll])))

(defn- <non-negative-number>? [coll] {:pre [(every? digit? coll)]}
  (when-first [fst coll]
    (or (nonzero-digit? fst) (empty? (rest coll)))))

(defn- <number>? [coll]
  (when-let [[neg num] (neg-non-negative coll)]
    (<non-negative-number>? num)))

(defn- char->integer [ch] {:pre [(digit? ch)]} (- (int ch) (int \0)))

(defn- <non-negative-number>->int [coll] {:pre [(<non-negative-number>? coll)]}
  (reduce #(+ (* 10 %1) %2) 0 (map char->integer coll)))

;; handles empty coll
(defn- <number>->int [[indices coll]]
  (and (<number>? coll)
       [indices
        (let [[neg coll] (split-first-neg coll)]
          (* (if (seq neg) -1 1)
             (<non-negative-number>->int coll)))]))

(defn- make-number [[indices number]]
  (-> (base-token :number :integer (first indices) (str (when (< number 0) \~) (abs number)))
      (assoc :which number)))

(defn- split-number [s]
  (when-let [fst-ch (some-> (first s) (second) (int))]
    (let [[prep coll] (if (= (int \~) (int fst-ch)) (split-at 1 s) (split-at 0 s))
          [fst rst] (split-with #(digit? (second %1)) coll)
          fst (concat prep fst)]
      [(util/unzip fst) rst])))

(defn next-number
  "Returns [token seq] if the next token in `s` is <number>. Returns logical
  false otherwise."
  [s]
  (next-extractor split-number <number>->int make-number s))

;; -----------------------------------------------------------------------------
;; mapping operators
;; -----------------------------------------------------------------------------

(defn- wrapped-arith [op]
  #(try [:ok (op %1 %2)]
        (catch ArithmeticException e [:error {:reason "I can't do this math", :cause e}])))

(def operators
  "Defined operators. Each of them takes 2 arguments and returns result."
  (-> {\+ [+ :plus "+"], \- [- :minus "-"], \* [* :mult "*"], \/ [math/floor-div :div "/"], \% [rem :rem "%"]}
      (update-vals #(update %1 0 wrapped-arith))))

(defn- make-op [[start [op name s]]]
  (-> (base-token :op name start s) (assoc :which op)))

(defn next-op
  "Returns [token seq] if the next token in `s` is an operator. Returns logical
  false otherwise."
  [s]
  (next-extractor first-rest (first-rest-maps operators) make-op s))

;; -----------------------------------------------------------------------------

(def ^:private parens {\( :open, \) :close})

(defn- make-paren [[start name]] (base-token :paren name start))

(defn next-paren
  "Returns [token seq] if the next token in `s` is either closing or opening
  parenthesis. Returns logical false otherwise."
  [s]
  (next-extractor first-rest (first-rest-maps parens) make-paren s))

(defn drop-blank
  "Drops blank characters."
  [s]
  (drop-while #(#{\space \tab} (second %1)) s))

(defn next-token
  "Returns [token seq] if there is a valid token in the beginning of the `s`.
  Doesn't ignore blank characters. Returns logical false otherwise."
  [s]
  ((some-fn next-number next-op next-paren) s))

(defn- tokenize-error [rst]
  {:reason "I don't know how to parse this"
   :start (first (first rst))})

(defn tokenize
  "Returns [:ok tokens] unless it is invalid. If invalid, returns [:error map]
  containing the position of the invalid character."
  ([string] (tokenize string 0))
  ([string offset]
   (loop [rst (util/with-index offset (seq string)) tokens []]
     (if-let [rst (seq (drop-blank rst))]
       (if-let [[token rst] (next-token rst)]
         (recur rst (conj tokens token))
         [:error (tokenize-error rst)])
      [:ok tokens]))))
