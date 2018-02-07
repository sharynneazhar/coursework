# Project 0 - A First Interpreter

The objective of this mini-project is to develop your first rudimentary interpreter. You will extend with the AE language presented in class to get up-to-speed with Haskell and write three different interpreters using three different styles. For this project, AE will be extended to include an `if0` construct in addition to constructs discussed in class:

## Running the Test

Load the file:
```
ghci p0.hs
```

Run the QuickCheck test (e.g. 10 different test runs)
```
testEval 10
```

[*results.txt*](https://github.com/sharynneazhar/coursework/blob/master/eecs662/project0/results.txt) shows a sample verbose output when `testEval 10` is run.


## Project Requirements

```haskell
AE ::= natural
       AE + AE |
       AE - AE |
       AE * AE |
       AE / AE |
       if0 AE then AE else AE
```

The new construct, `if0`, is an expression that evaluates its first argument and if it is 0 evaluates its second. If not, it evaluates its third.

Assume that **natural** represents the natural numbers as we have been discussing in class, so you must add error handling for negative numbers.

To aid in your quest, the file `p0.hs` implements the Haskell AE data type and a parser from strings to the data type. At the bottom of the file you will find signatures for the four functions required for the four exercises below.

### Exercise 1
Write a function, `evalAE::AE -> Int`, that takes a `AE` data structure, evaluates it, and returns an `Int`. If your interpreter fails, it should throw an error using the error function discussed in class. Remember that **number** is a natural number and cannot be negative.

### Exercise 2
Write a function, `evalAEMaybe::AE -> Maybe Int`, that takes the same `AE` data structure as Exercise 1, evaluates it, and returns a value. Use `Just` to return a number and `Nothing` to indicate an error. Do not use `Maybe` as a monad in this interpreter. Use `if` and/or `case` to process errors.

### Exercise 3
Write a function, `evalM::AE -> Maybe Int`, that takes the same `AE` data structure as Exercises 1 & 2, evaluates it, and returns a number. This time, use the `do` notation to treat `Maybe` as a monad.

### Exercise 4
Write a function, `interpAE` that combines the `AE` parser and `evalM` into a single operation that parses and then evaluates an `AE` expression. You will be provided a parser and must integrate it with your interpreter.
