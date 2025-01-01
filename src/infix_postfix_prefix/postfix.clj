(ns infix-postfix-prefix.postfix
  (:require [infix-postfix-prefix.util :as util]
            [clojure.string :as str]))

;; -----------------------------------------------------------------------------
;; {pre,post}fix evaluation
;; -----------------------------------------------------------------------------
;; reusing functions between postfix and prefix

(defn- split-at-strict [n coll]
  (let [[fst rst] (split-at n coll)]
    (when (= n (count fst)) [fst rst])))

(defn- eval-with-stack
  "Wraps stack operations. Each token is tested with `pop?`. If it is logical
  true, the token and `arity` stack items are passed to `combine`. The token is
  the first argument to `combine` and the newest item on the stack is passed as
  the second. Overall `combine` takes 1 + `arity` arguments. `combine` returns
  the next item to push to the stack as [:ok item], or [:error map] to terminate
  further operations returning it. Stack items are converted to push via
  `extract-token`.
  
  Returns [:ok stack] if no error. If the stack doesn't contain enough items
  when `pop?`, returns [:error m] containing the intermediate state of the stack
  and tokens."
  [tokens pop? combine arity extract-token] {:pre [(nat-int? arity)]}
  (letfn [(read-token [stack token]
            (if (pop? token)
              (if-let [[fst rst] (split-at-strict arity stack)]
                (util/map-result [result (apply combine token fst)]
                                 (conj result rst)
                                 (update result 1 #(util/put-new %1 :stack stack :token token)))
                [:error {:reason "used up stack too early", :token token}])
              [:ok (extract-token token) stack]))]
    (loop [tokens tokens stack '()]
      (util/if-first-rest [token tokens tokens]
        (util/map-result-ok [result (read-token stack token)]
                            (let [[_ top stack] result]
                              (recur tokens (conj stack top))))
        [:ok stack]))))

(defn- kind-op? [m] (= :op (:kind m)))

(defmacro ^:private eval-somefix
  ([tokens combine] `(eval-somefix ~tokens ~combine :which))
  ([tokens combine extract-token]
   `(util/map-result [[~'_ data#]
                      (eval-with-stack ~tokens kind-op? ~combine 2 ~extract-token)]
                     (case (count data#)
                       1 [:ok (first data#)]
                       0 [:error {:reason "stack is empty, maybe input is empty"}]
                       [:error {:reason "evaluation finished, but too many numbers", :stack data#}])
                     (let [start#  (get-in data# [:token :start] :unavailable)
                           reason# (get data# :reason)]
                       [:error {:reason (format "evaluation failed at %s (%s)" start# reason#)
                                :start  start#, :cause data#}]))))

;; -----------------------------------------------------------------------------

;; op returns result
(defn- postfix-combine [{:keys [which]} a b] (which b a))

(defn eval-postfix
  "Evaluates postfix notation. Returns result."
  [tokens]
  (eval-somefix tokens postfix-combine))

(defn- prefix-combine [{:keys [which]} a b] (which a b))

(defn eval-prefix
  "Evaluates prefix notation. Uses the same evaluation as postfix notation under
  the hood."
  [tokens]
  (eval-somefix (reverse tokens) prefix-combine))

;; -----------------------------------------------------------------------------
;; stringify
;; -----------------------------------------------------------------------------

(defn- postfix-stringify-combine [{:keys [string]} a b]
  [:ok (str "(" (str/join " " [b string a]) ")")])

(defn postfix-tokens->infix-string
  "Given a sequence of tokens of valid postfix notation, returns a string
  representing its infix notation using a lot of parentheses."
  [tokens]
  (str ((eval-somefix tokens postfix-stringify-combine :string) 1)))

(defn- prefix-stringify-combine [{:keys [string]} a b]
  [:ok (str "(" (str/join " " [a string b]) ")")])

(defn prefix-tokens->infix-string
  "Given a sequence of tokens of valid prefix notation, returns a string
  representing its infix notation using a lot of parentheses."
  [tokens]
  (str ((eval-somefix (reverse tokens) prefix-stringify-combine :string) 1)))
