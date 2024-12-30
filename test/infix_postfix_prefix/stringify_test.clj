(ns infix-postfix-prefix.stringify-test
  (:require [clojure.test :refer :all]
            [infix-postfix-prefix.util :as util]
            [infix-postfix-prefix.tokenize :refer [tokenize]]
            [infix-postfix-prefix.infix :as infix]
            [infix-postfix-prefix.postfix :as postfix]))

(defn infix->prefix [s] (-> (tokenize s) (get 1)
                            (infix/infix-tokens->tree) (get 1)
                            (infix/tree->prefix-string)))

(defn infix->postfix [s] (-> (tokenize s) (get 1)
                             (infix/infix-tokens->tree) (get 1)
                             (infix/tree->postfix-string)))

(defn infix->infix [s] (-> (tokenize s) (get 1)
                           (infix/infix-tokens->tree) (get 1)
                           (infix/tree->infix-string)))

(defn prefix->infix [s] (-> (tokenize s) (get 1)
                            (postfix/prefix-tokens->infix-string)))

(defn postfix->infix [s] (-> (tokenize s) (get 1)
                             (postfix/postfix-tokens->infix-string)))

(deftest infix->prefix-test
  (is (= "1" (infix->prefix "1")))
  (is (= "+ + 27 5 3" (infix->prefix "27+5+3")))
  (is (= "+ 27 + 5 3" (infix->prefix "27+(5+3)")))
  (is (= "- ~27 - ~5 ~3" (infix->prefix "~27-(~5-~3)"))))

(deftest infix->postfix-test
  (is (= "1" (infix->postfix "1")))
  (is (= "27 5 + 3 +" (infix->postfix "27+5+3")))
  (is (= "27 5 3 + +" (infix->postfix "27+(5+3)")))
  (is (= "~27 ~5 ~3 - -" (infix->postfix "~27-(~5-~3)"))))

(deftest infix->infix-test
  (is (= "1" (infix->infix "1")))
  (is (= "((27 + 5) + 3)" (infix->infix "27+5+3")))
  (is (= "(27 + (5 + 3))" (infix->infix "27+(5+3)")))
  (is (= "(~27 - (~5 - ~3))" (infix->infix "~27-(~5-~3)"))))

(deftest prefix->infix-test
  (is (= "1" (prefix->infix "1")))
  (is (= "((27 + 5) + 3)" (prefix->infix "++ 27 5 3")))
  (is (= "(27 + (5 + 3))" (prefix->infix  "+27+5 3")))
  (is (= "(~27 - (~5 - ~3))" (prefix->infix "-~27-~5~3"))))

(deftest postfix->infix-test
  (is (= "1" (postfix->infix "1")))
  (is (= "((27 + 5) + 3)" (postfix->infix "27 5+3+")))
  (is (= "(27 + (5 + 3))" (postfix->infix "27 5 3++")))
  (is (= "(~27 - (~5 - ~3))" (postfix->infix "~27~5~3--"))))
