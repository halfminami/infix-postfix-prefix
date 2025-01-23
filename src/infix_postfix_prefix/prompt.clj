(ns infix-postfix-prefix.prompt
  (:require [clojure.string :as str]))

;; quick simple repl

(defmacro ^:private println-all [coll]
  `(do ~@(map (fn [s-or-coll]
                (if (string? s-or-coll)
                  `(println ~s-or-coll)
                  `(apply println ~s-or-coll)))
              coll)
       (flush)))

(def prompt-string "> ")

(def special-prefix
  "Formula string starting with one of these strings would be treated as the
  corresponding notation."
  {:infix "#infix ", :prefix "#prefix ", :postfix "#postfix ", :help "#h"})

(defn print-help []
  (println-all [["; In this prompt, you can evaluate and convert between infix, prefix and postfix"]
                ["; notations. Commands:"]
                [";  " (special-prefix :help) "=> show this help"]
                [";   <formula> => evaluate a formula by guessing its notation"]
                [";  " (str/trimr (special-prefix :infix)) "<formula> => evaluate a formula as an infix notation"]
                [";  " (str/trimr (special-prefix :prefix)) "<formula> => evaluate a formula as a prefix notation"]
                [";  " (str/trimr (special-prefix :postfix)) "<formula> => evaluate a formula as a postfix notation"]
                ["; CTRL-C to exit."]]))

(defn read-prompt
  "Next prompt. Filters out help. If `nil` probably CTRL-D."
  []
  (when-let [s (do (print prompt-string) (flush) (read-line))]
    (if (str/starts-with? s (special-prefix :help))
      (do (print-help) (recur))
      s)))

(defn print-banner []
  (println-all [["This is interactive formula parser."]
                ["Type" (special-prefix :help) "to show help."]
                []]))

(defn- str-n-times [n s]
  (apply str (take n (cycle s))))

;; in my environment a bug of old rlwrap has erased the prompt string
(defn print-error [{:keys [start reason notation]}]
  (when start
    (print (str-n-times (+ (count prompt-string) start) " "))
    (println "^ here"))
  (println-all [["Error:" reason]])
  (when notation (println-all [["Read as" notation]]))
  (println-all [[]]))

(defn print-success [e from infix prefix postfix]
  (println-all [["input:  " from "=>" e]
                []
                ["infix:  " infix]
                ["prefix: " prefix]
                ["postfix:" postfix]
                []]))
