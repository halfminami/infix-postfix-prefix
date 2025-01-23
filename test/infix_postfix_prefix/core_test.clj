(ns infix-postfix-prefix.core-test
  (:require [clojure.test :refer :all]
            [infix-postfix-prefix.core :refer :all]
            [infix-postfix-prefix.util :as util]))

(deftest eval-repl-test
  (testing "heuristics"
    (is (= [:ok [3 :infix "(1 + 2)" "+ 1 2" "1 2 +"]]
           (eval-repl "1 + 2")))
    (is (= [:ok [3 :prefix "(1 + 2)" "+ 1 2" "1 2 +"]]
           (eval-repl "+ 1 2")))
    (is (= [:ok [3 :postfix "(1 + 2)" "+ 1 2" "1 2 +"]]
           (eval-repl "1 2 +")))
    (is (= [:ok [42 :infix "42" "42" "42"]]
           (eval-repl "42"))))
  (testing "special prefix"
    (is (= [:ok [3 :infix "(1 + 2)" "+ 1 2" "1 2 +"]]
           (eval-repl "#infix 1 + 2")))
    (is (= [:ok [3 :postfix "(1 + 2)" "+ 1 2" "1 2 +"]]
           (eval-repl "#postfix 1 2 +")))
    (is (= [:ok [3 :prefix "(1 + 2)" "+ 1 2" "1 2 +"]]
           (eval-repl "#prefix + 1 2")))
    (is (= [:ok [42 :prefix "42" "42" "42"]]
           (eval-repl "#prefix 42")))))

(defn error= [want-map want-regex [got-sym got-map]]
  (and (= :error got-sym)
       (string? (re-find want-regex (:reason got-map)))
       (util/map-partial= want-map got-map)))

(deftest eval-repl-invalid-test
  (testing "heuristics"
    (is (error= {} #"empty"
                (eval-repl "")))
    (is (error= {:start 0} #"parse"
                (eval-repl "#")))
    (is (error= {:start 1} #"early"
                (eval-repl "1+")))
    (is (error= {:start 2} #"no token"
                (eval-repl "1+*2")))
    (is (error= {:start 1} #"too many"
                (eval-repl "1~2+3"))))
  (testing "special prefix"
    (is (error= {} #"empty"
                (eval-repl "#prefix ")))
    (is (error= {:start 0} #"parse"
                (eval-repl "#prefix")))
    (is (error= {:start 9} #"early"
                (eval-repl "#prefix 1+")))
    (is (error= {:start 8} #"no token"
                (eval-repl "#infix 1+")))
    (is (error= {} #"many"
                (eval-repl "#postfix 1~2")))))
