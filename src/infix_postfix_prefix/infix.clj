(ns infix-postfix-prefix.infix
  (:require [infix-postfix-prefix.util :as util]
            [clojure.string :as str]))

;; -----------------------------------------------------------------------------
;; matching parentheses
;; -----------------------------------------------------------------------------
;; unwrapping parentheses around <expr> and putting it into a :wrapped node
;; to make branching easier (and efficient)

(defn- make-wrapped [coll] {:kind :wrapped, :which coll})

(defn- paren-tree
  ([tokens] (paren-tree tokens []))
  ([tokens tree]
   (util/if-first-rest [token tokens tokens]
     (case (:name token)
       :open (util/map-result [result (paren-tree tokens)]
                              (let [[_ wrappen _ tokens] result]
                                (if (empty? wrappen)
                                  [:error {:reason "empty expression inside parentheses", :token token}]
                                  (recur tokens (conj tree (make-wrapped wrappen)))))
                              (update result 1 #(util/put-new %1 :token token)))
       :close [:ok tree token tokens]
       (recur tokens (conj tree token)))
     [:error {:reason "didn't close"}])))

;; we can simply wrap up the whole formula..
;; but it'd make reporting parenthesis error slightly harder
(defn- tokens->paren-tree [tokens]
  (util/map-result-error
   [result
    (loop [tokens tokens tree []]
      (util/if-first-rest [token tokens tokens]
        (case (:name token)
          :open  (util/map-result [result (paren-tree tokens)]
                                  (let [[_ wrappen _ tokens] result]
                                    (if (empty? wrappen)
                                      [:error {:reason "empty expression inside parentheses", :token token}]
                                      (recur tokens (conj tree (make-wrapped wrappen)))))
                                  (update result 1 #(util/put-new %1 :token token)))
          :close [:error {:reason "extra closing parenthesis", :token token}]
          (recur tokens (conj tree token)))
        [:ok tree]))]
   (let [[_ m]                  result
         {:keys [reason token]} m
         {:keys [start]}        token]
     [:error {:reason (format "invalid parenthesis at %s (%s)" start reason)
              :start  start
              :cause  m}])))

;; -----------------------------------------------------------------------------
;; parsing into tree by BNF
;; -----------------------------------------------------------------------------
;; making formula tree

(declare <add-or-sub>)

(defn- <expr> [ptree]
  (case (count ptree)
    1 (let [token (first ptree)]
        (case (:kind token)
          :wrapped (<add-or-sub> (:which token))
          :number  [:ok (assoc token :left nil :right nil)]
          [:error {:reason "expected a number or expression, got something else", :token token}]))
    0 [:error {:reason "expected a single token, got no token"}]
    [:error {:reason "expected a single token, got too many tokens", :token (first ptree)}]))

(defmacro ^:private bnf-branch
  [split-token? found-left found-right not-found ptree]
  `(util/map-result [[~'_ [left# mid# right#]]
                     (util/split-right-fn ~ptree ~split-token?)]
                    (util/map-result [[~'_ right#] (~found-right right#)]
                                     (util/map-result [[~'_ left#] (~found-left left#)]
                                                      [:ok (assoc mid# :left left# :right right#)]
                                                      [:error (util/put-new left# :token mid#)])
                                     [:error (util/put-new right# :token mid#)])
                    (~not-found right#)))

(defn- <rem> [ptree]
  (bnf-branch #(and (= :op (:kind %1)) (= :rem (:name %1)))
              <rem> <expr> <expr> ptree))

(defn- <mul-or-div> [ptree]
  (bnf-branch #(and (= :op (:kind %1)) (case (:name %1) (:mult :div) true false))
              <mul-or-div> <rem> <rem> ptree))

(defn- <add-or-sub> [ptree]
  (bnf-branch #(and (= :op (:kind %1)) (case (:name %1) (:plus :minus) true false))
              <add-or-sub> <mul-or-div> <mul-or-div> ptree))

(defn- paren-tree->tree [ptree]
  (util/map-result-error [result (<add-or-sub> ptree)]
                         (let [[_ m]                  result
                               {:keys [reason token]} m
                               {:keys [start]}        token]
                           [:error {:reason (format "invalid expression at %s (%s)" start reason)
                                    :start  start
                                    :cause  m}])))

;; -----------------------------------------------------------------------------
;; putting it all together
;; -----------------------------------------------------------------------------

(defn infix-tokens->tree
  "Creates a formula tree from `tokens` if it represents a valid infix
  notation."
  [tokens]
  (if (seq tokens)
    (util/map-result-ok [[_ ptree] (tokens->paren-tree tokens)]
                        (paren-tree->tree ptree))
    [:error {:reason "empty expression"}]))

(defn eval-tree
  "Evaluates a formula tree. Returns result."
  [tree]
  (case (:kind tree)
    :number [:ok (:which tree)]
    :op     (let [{:keys [left right which start]} tree]
              (util/map-result-ok
               [[_ l-m] (eval-tree left)]
               (util/map-result-ok
                [[_ r-m] (eval-tree right)]
                (util/map-result-error
                 [[_ data] (which l-m r-m)]
                 [:error (util/put-new data :start start)]))))))

(defn eval-infix
  "Evaluates infix notation."
  [tokens]
  (util/map-result-ok [[_ tree] (infix-tokens->tree tokens)]
                      (eval-tree tree)))

;; -----------------------------------------------------------------------------
;; stringify
;; -----------------------------------------------------------------------------
;; prefix and postfix notations can be constructed with DFS on formula tree. I
;; am going to convert between them via
;; {pre,post}fix string -> infix string -> formula tree -> {post,pre}fix string

(defn tree->prefix-string
  "Given a valid formula tree, returns a string representing its prefix
  notation."
  [tree]
  (case (:kind tree)
    :number (:string tree)
    :op     (str/join " " [(:string tree) (tree->prefix-string (:left tree)) (tree->prefix-string (:right tree))])))

(defn tree->postfix-string
  "Given a valid formula tree, returns a string representing its postfix
  notation."
  [tree]
  (case (:kind tree)
    :number (:string tree)
    :op     (str/join " " [(tree->postfix-string (:left tree)) (tree->postfix-string (:right tree)) (:string tree)])))

(defn tree->infix-string
  "Given a valid formula tree, returns a string representing its infix
  notation using a lot of parentheses."
  [tree]
  (case (:kind tree)
    :number (:string tree)
    :op     (str "("
                 (str/join " " [(tree->infix-string (:left tree)) (:string tree) (tree->infix-string (:right tree))])
                 ")")))
