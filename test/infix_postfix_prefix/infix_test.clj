(ns infix-postfix-prefix.infix-test
  (:require [clojure.test :refer :all]
            [infix-postfix-prefix.infix :refer :all]
            [infix-postfix-prefix.tokenize :refer [tokenize]]
            [infix-postfix-prefix.util :as util]))

(defn tokenize-unwrap [s] ((tokenize s) 1))

(deftest infix-valid-test
  (testing "simple"
    (is (= [:ok 11]
           (eval-infix (tokenize-unwrap "11"))))
    (is (= [:ok 3]
           (eval-infix (tokenize-unwrap "1+2"))))
    (is (= [:ok 24]
           (eval-infix (tokenize-unwrap "3%2*4+2*10")))))
  (testing "parenthesis"
    (is (= [:ok 24]
           (eval-infix (tokenize-unwrap "(3%2*4+2*10)"))))
    (is (= [:ok 23]
           (eval-infix (tokenize-unwrap "3%(2*4)+2*10"))))
    (is (= [:ok 24]
           (eval-infix (tokenize-unwrap "3%2*(4+2*10)"))))
    (is (= [:ok 30]
           (eval-infix (tokenize-unwrap "3%(((2*4))+2)*(10)"))))
    (is (= [:ok 5]
           (eval-infix (tokenize-unwrap "100%3*(5+42)%(3+3)"))))
    (is (= [:ok 0]
           (eval-infix (tokenize-unwrap "5/2/3"))))
    (is (= [:ok -3]
           (eval-infix (tokenize-unwrap "5/~2"))))))

(defn eval-error= [want-m reason-re [sym m]]
  (and (= :error sym)
       (util/map-partial= want-m m)
       (string? (re-find reason-re (:reason m)))))

(deftest infix-invalid-test
  (testing "parenthesis"
    (is (eval-error= {} #"empty"
                     (eval-infix (tokenize-unwrap ""))))
    (is (eval-error= {:start 0} #"empty"
                     (eval-infix (tokenize-unwrap "()"))))
    (is (eval-error= {:start 4} #"empty"
                     (eval-infix (tokenize-unwrap "1+2*()"))))
    (is (eval-error= {:start 0} #"didn't close"
                     (eval-infix (tokenize-unwrap "(11"))))
    (is (eval-error= {:start 2} #"extra"
                     (eval-infix (tokenize-unwrap "11)")))))
  (testing "syntax"                     ; may be subject to change..
    (is (eval-error= {:start 0} #"too many"
                     (eval-infix (tokenize-unwrap "1 2+3"))))
    (is (eval-error= {:start 2} #"too many"
                     (eval-infix (tokenize-unwrap "2+3~1"))))
    (is (eval-error= {:start 0} #"no token"
                     (eval-infix (tokenize-unwrap "+"))))
    (is (eval-error= {:start 1} #"no token"
                     (eval-infix (tokenize-unwrap "1+"))))
    (is (eval-error= {:start 2} #"no token"
                     (eval-infix (tokenize-unwrap "1+*3"))))
    (is (eval-error= {:start 3} #"no token"
                     (eval-infix (tokenize-unwrap "1+3*"))))
    (is (eval-error= {:start 0} #"no token"
                     (eval-infix (tokenize-unwrap "+1*3")))))
  (testing "math"
    (is (eval-error= {:start 1} #"math"
                     (eval-infix (tokenize-unwrap "1/0"))))
    (is (eval-error= {:start 1} #"math"
                     (eval-infix (tokenize-unwrap "1/3%1"))))
    (is (eval-error= {:start 1} #"math"
                     (eval-infix (tokenize-unwrap "5/(2/3)"))))))
