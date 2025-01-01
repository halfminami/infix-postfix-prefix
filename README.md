# infix-postfix-prefix

Evaluation and conversion between infix, postfix and prefix notations.

It has a simple REPL to evaluate arithmetic expressions besides eval functions:
```
lein run
```
- detect/specify which notation input is
- evaluate input of whichever notation or report error
- print the input in three notations

I recommend you use `rlwrap` for better editing!
```
$ rlwrap lein run
This is interactive formula parser.
Type #h to show help.

> (7+3)/0*2+20*2
       ^ here
Error: evaluation failed at 5 (I can't do this math)
Read as :infix

> (7+3)/8*2+20*2
input:   :infix => 42

infix:   ((((7 + 3) / 8) * 2) + (20 * 2))
prefix:  + * / + 7 3 8 2 * 20 2
postfix: 7 3 + 8 / 2 * 20 2 * +

> 7 3 + 8 / 2 * 20 2 * +
input:   :postfix => 42

infix:   ((((7 + 3) / 8) * 2) + (20 * 2))
prefix:  + * / + 7 3 8 2 * 20 2
postfix: 7 3 + 8 / 2 * 20 2 * +

> 
```

## Uses

This is a recreational Clojure project with leiningen. There is a documentation
for the syntax of the arithmetic expression in `doc/bnf.md`.

## Test

Contains tests for functions and the REPL:
```
lein test
```
