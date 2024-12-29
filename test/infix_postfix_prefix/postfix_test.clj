(ns infix-postfix-prefix.postfix-test
  (:require [clojure.test :refer :all]
            [infix-postfix-prefix.postfix :refer :all]
            [infix-postfix-prefix.tokenize :refer [tokenize]]
            [infix-postfix-prefix.util :as util]))

;; s always should be valid
(defn tokenize-unwrap [s] ((tokenize s) 1))

(deftest postfix-valid-test
  (is (= [:ok 8]
         (eval-postfix (tokenize-unwrap "3 5 +"))))
  (is (= [:ok 1]
         (eval-postfix (tokenize-unwrap "1"))))
  (is (= [:ok 3]
         (eval-postfix (tokenize-unwrap "5 2 -"))))
  (is (= [:ok 3]
         (eval-postfix (tokenize-unwrap "3 4 2 1 * 8 - 9 + % *")))))

(defn eval-error= [want-m reason-re [sym m]]
  (and (= :error sym)
       (util/map-partial= want-m m)
       (string? (re-find reason-re (:reason m)))))

(deftest postfix-invalid-test
  (is (eval-error= {} #"empty"
                   (eval-postfix (tokenize-unwrap ""))))
  (is (eval-error= {} #"many"
                   (eval-postfix (tokenize-unwrap "1 2"))))
  (is (eval-error= {:start 2} #"early"
                   (eval-postfix (tokenize-unwrap "1 *"))))
  (is (eval-error= {:start 4} #"math"
                   (eval-postfix (tokenize-unwrap "1 0 /")))))

(deftest prefix-valid-test
  (is (= [:ok 8]
         (eval-prefix (tokenize-unwrap "+ 3 5"))))
  (is (= [:ok 1]
         (eval-prefix (tokenize-unwrap "1"))))
  (is (= [:ok 3]
         (eval-prefix (tokenize-unwrap "- 5 2"))))
  (is (= [:ok 3]
         (eval-prefix (tokenize-unwrap "* 3 % 4 + - * 1 2 8 9")))))
