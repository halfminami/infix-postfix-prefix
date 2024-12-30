(ns infix-postfix-prefix.tokenize-test
  (:require [clojure.test :refer :all]
            [infix-postfix-prefix.util :as util]
            [infix-postfix-prefix.tokenize :refer :all]))

;; Compares tokens partially.
(defn token-result= [[want-token want-seq] [got-token got-seq]]
  (and (util/map-partial= want-token got-token) (= want-seq got-seq)))

(deftest number-test
  (is (token-result= [{:kind :number, :which 38} '((2 \x))]
                     (next-number (util/with-index (seq "38x")))))
  (is (token-result= [{:kind :number, :which 0} '((1 \space))]
                     (next-number (util/with-index (seq "0 ")))))
  (is (token-result= [{:kind :number, :which 5040} '()]
                     (next-number (util/with-index (seq "5040")))))
  (is (token-result= [{:kind :number, :which 1} '((1 \o) (2 \8))]
                     (next-number (util/with-index (seq "1o8")))))
  (is (token-result= [{:kind :number, :which -10} '((3 \~))]
                     (next-number (util/with-index (seq "~10~")))))
  (is (token-result= [{:kind :number, :which 0} '()]
                     (next-number (util/with-index (seq "~0"))))))

(deftest non-number-test
  (is (not (next-number (util/with-index (seq "02")))))
  (is (not (next-number (util/with-index (seq "x")))))
  (is (not (next-number '())))
  (is (not (next-number (util/with-index (seq "~~1")))))
  (is (not (next-number (util/with-index (seq "~00")))))
  (is (not (next-number (util/with-index (seq "~"))))))

(deftest operator-test
  (is (token-result= [{:kind :op, :name :plus} '((1 \1) (2 \8))]
                     (next-op (util/with-index (seq "+18")))))
  (is (token-result= [{:kind :op, :name :minus} '()]
                     (next-op (util/with-index (seq "-")))))
  (is (token-result= [{:kind :op, :name :mult} '()]
                     (next-op (util/with-index (seq "*")))))
  (is (token-result= [{:kind :op, :name :div} '()]
                     (next-op (util/with-index (seq "/")))))
  (is (token-result= [{:kind :op, :name :rem} '()]
                     (next-op (util/with-index (seq "%")))))
  (is (not (next-op (util/with-index (seq "2"))))))

(deftest paren-test
  (is (token-result= [{:kind :paren, :name :open} '((1 \1))]
                     (next-paren (util/with-index (seq "(1")))))
  (is (token-result= [{:kind :paren, :name :close} '((1 \*))]
                     (next-paren (util/with-index (seq ")*")))))
  (is (not (next-paren (util/with-index (seq "*"))))))

(deftest blank-test
  (is (= (map list (range 5 11) "0000  ")
         (drop-blank (util/with-index (seq "     0000  ")))))
  (is (= (util/with-index (seq "0000"))
         (drop-blank (util/with-index (seq "0000"))))))

;; Tokens are compared partially
(defn tokenize-result= [[want-sym want-seq] [got-sym got-seq]]
  (cond
    (not (= want-sym got-sym))
    false
    (= :ok want-sym)
    (util/every?* util/map-partial= want-seq got-seq)
    (= :error want-sym)
    (util/map-partial= want-seq got-seq)
    :else
    false))

;; each token is checked in previous tests
(deftest tokenize-test
  (is (tokenize-result= [:ok []] (tokenize "")))
  (is (tokenize-result= [:ok []] (tokenize "       ")))
  (is (tokenize-result= [:ok [{:kind :number, :start 0}
                              {:kind :number, :start 5}
                              {:kind :op, :start 7}]]
                        (tokenize "3333 0 +")))
  (is (tokenize-result= [:ok [{:kind :number, :start 5}
                              {:kind :number, :start 10}
                              {:kind :op, :start 11}]]
                        (tokenize "     3333 0+ ")))
  (is (tokenize-result= [:ok [{:kind :op} {:kind :op} {:kind :op} {:kind :op}]]
                        (tokenize "****")))
  (is (tokenize-result= [:ok [{:kind :paren, :start 0}
                              {:kind :number, :start 1}
                              {:kind :op, :start 2}
                              {:kind :number, :start 3}
                              {:kind :paren, :start 4}]]
                        (tokenize "(3/2)")))
  (is (tokenize-result= [:ok [{:kind :op, :start 0, :name :minus}
                              {:kind :number, :start 1, :which 10}
                              {:kind :op, :start 3, :name :minus}
                              {:kind :number, :start 4, :which 0}
                              {:kind :op, :start 5, :name :minus}]]
                        (tokenize "-10-0-")))
  (is (tokenize-result= [:ok [{:kind :number, :start 0, :which -10}
                              {:kind :number, :start 3, :which 0}
                              {:kind :op, :start 5, :name :minus}]]
                        (tokenize "~10~0-")))
  (is (tokenize-result= [:ok [{:kind :op, :start 0, :name :minus}
                              {:kind :number, :start 1, :which 1}
                              {:kind :op, :start 2, :name :minus}
                              {:kind :number, :start 3, :which -10}]]
                        (tokenize "-1-~10"))))

(deftest tokenize-invalid-test
  (is (tokenize-result= [:error {:start 0}] (tokenize "00")))
  (is (tokenize-result= [:error {:start 0}] (tokenize "a")))
  (is (tokenize-result= [:error {:start 4}] (tokenize " */%00")))
  (is (tokenize-result= [:error {:start 1}] (tokenize "0~+2"))))
