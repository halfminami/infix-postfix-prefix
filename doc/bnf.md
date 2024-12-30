# Definitions for formulas

This file contains some kind of Backus-Naur form to describe the formula
syntax. This hopefully helps with the parser implementation.

## Common Definitions

Considering whole numbers only:
```bnf
// exact, decimal.
<number> ::= "~"? <non-negative-number>
<non-negative-number> ::= <non-zero-digits> <digits>+ | <digits>
<non-zero-digits> ::= "1" | "2" | ... | "9"
<digits> ::= "0" | <non-zero-digits>
```

Defined operators are `"+", "-", "/", "*", "%"`, standing for addition,
subtraction, division (floored), multiplication, remainder respectively, and
are binary operators.

Spaces separate tokens and are ignored. Add them between consecutive numbers or
elsewhere to improve readability.

## The notations

Postfix notation:
```bnf
<formula> ::= <expr>
<expr> ::= <expr> <expr> <op> | <number>

<op> ::= "+" | "-" | "/" | "*" | "%"

// example: 1 2 3 % - 4 + => (1 - (2 % 3)) + 4
```
will be parsed with a famous stack operation.

Prefix notation:
```bnf
<formula> ::= <expr>
<expr> ::=  <op> <expr> <expr> | <number>

<op> ::= "+" | "-" | "/" | "*" | "%"

// example: + - 1 % 2 3 4 => (1 - (2 % 3)) + 4
```
will be parsed with a famous stack operation reversing the input string.

Infix notation:
```bnf
<formula> ::= <add-or-sub>

<add-or-sub> ::= <add-or-sub> <add-or-sub-op> <mul-or-div> | <mul-or-div>
<mul-or-div> ::= <mul-or-div> <mul-or-div-op> <rem> | <rem>
<rem> ::= <rem> <rem-op> <expr> | <expr>

<expr> ::= "(" <add-or-sub> ")" | <number>

<add-or-sub-op> ::= "+" | "-"
<mul-or-div-op> ::= "*" | "/"
<rem-op> ::= "%"

// example: 1 * 2 % 3 / 4 + 5 * 6 => ((1 * (2 % 3)) / 4) + (5 * 6)
// example: 1 * 2 % (3 / 4 + 5) * 6 => (1 * (2 % ((3 / 4) + 5))) * 6
```

According to the spec, operator precedence is `% > {*, /} > {+, -}`, with `%`
being the highest and `+, -` being the lowest.
