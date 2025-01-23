(ns infix-postfix-prefix.core
  (:require [clojure.string :as str]
            [infix-postfix-prefix.infix :as infix]
            [infix-postfix-prefix.postfix :as postfix]
            [infix-postfix-prefix.util :as util]
            [infix-postfix-prefix.tokenize :as tkn]
            [infix-postfix-prefix.prompt :as prm]))

(defn which-notation
  "Tries to detect infix, postfix and prefix notation."
  [tokens]
  (cond
    (= :op (:kind (first tokens))) :prefix
    (= :op (:kind (last tokens)))  :postfix
    :else                          :infix))

(defn- formula-with-notation
  "Given an input string maybe with special prefix, returns [notation start
  string].
  `string` is ready to be parsed with index offset `start`. `notation` is
  `:auto` if isn't specified by user."
  [s]
  (cond
    (str/starts-with? s (prm/special-prefix :infix))
    (let [n (count (prm/special-prefix :infix))]
      [:infix n (subs s n)])
    
    (str/starts-with? s (prm/special-prefix :postfix))
    (let [n (count (prm/special-prefix :postfix))]
      [:postfix n (subs s n)])
    
    (str/starts-with? s (prm/special-prefix :prefix))
    (let [n (count (prm/special-prefix :prefix))]
      [:prefix n (subs s n)])

    :else
    [:auto 0 s]))

(defn- tokenize-detect-notation
  "Given an input string maybe with special prefix, returns [notation string
  tokens]."
  [s]
  (let [[sym offset s] (formula-with-notation s)]
    (util/map-result-ok [[_ tokens] (tkn/tokenize s offset)]
                        [:ok
                         (case sym
                           :infix   [:infix s tokens]
                           :postfix [:postfix s tokens]
                           :prefix  [:prefix s tokens]
                           :auto    [(which-notation tokens) s tokens])])))

(defn- call-corresponding-eval [notation tokens]
  (case notation
    :infix   (infix/eval-infix tokens)
    :postfix (postfix/eval-postfix tokens)
    :prefix  (postfix/eval-prefix tokens)))

(defn eval-repl
  "Evaluates string `s` and makes some data for REPL. When `:ok` it contains
  some sequence that can be applied to `print-success`."
  [s]
  (util/map-result-ok
   [[_ data] (tokenize-detect-notation s)]
   (let [[notation s tokens] data]
     (util/map-result
      [[_ data] (call-corresponding-eval notation tokens)]
      ;; evaluation ok
      (let [infix-str   (case notation
                          :infix   s
                          :postfix (postfix/postfix-tokens->infix-string tokens)
                          :prefix  (postfix/prefix-tokens->infix-string tokens))
            tree        (-> (tkn/tokenize infix-str) (get 1)
                            (infix/infix-tokens->tree) (get 1))
            infix-str   (infix/tree->infix-string tree)
            postfix-str (infix/tree->postfix-string tree)
            prefix-str  (infix/tree->prefix-string tree)]
        [:ok [data notation infix-str prefix-str postfix-str]])
      [:error (assoc data :notation notation)]))))

(defn -main [& args]
  ;; https://stackoverflow.com/a/11709814
  (.addShutdownHook (Runtime/getRuntime) (Thread. (fn [] (newline) (println "bye!"))))
  
  (prm/print-banner)
  
  (loop []
    (when-let [s (prm/read-prompt)]
      (util/map-result [[_ data] (eval-repl s)]
                       (apply prm/print-success data)
                       (prm/print-error data))
      (recur))))
