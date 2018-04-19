# Project 3 - Functions and Elaboration
The objective of this project is to add dynamically scoped and statically scoped strict functions to BAE and introduce elaboration to the interpretation process. You will first define an interpreter that adds functions and a conditional construct to BAE while removing the `bind` expression. You will then define an interpreter that uses elaboration to define interpretation of `bind` in terms of function application.


## Running the Program

Load the file:
```
ghci p3.hs
```

There are three `interp` methods available to easily run the different `eval`s:

```
interpDyn :: String -> (Maybe CFAE)
interpDyn = (evalDynCFAE []) . parseCFAE
```
```
interpStat :: String -> (Maybe CFAEValue)
interpStat = (evalStatCFAE []) . parseCFAE
```
```
interpCFBAE :: String -> (Maybe CFAEValue)
interpCFBAE = (evalCFBAE []) . parseCFBAE
```



## Project Requirements

To aid in your quest, the file `p2.hs` implements Haskell data types and function signatures needed for this project. I am not providing a parser for this project because parsers are evil.

### Exercise 1
In this exercise you will write an interpreter for a modified FBAE language presented in our text that does not include the `bind` construct, but does include first-class functions and an `if0-then-else` construct. Following is the grammar for this language that we will call **CFAE**:

```
CFAE ::= number |
         id |
         CFAE * CFAE  |
         CFAE - CFAE |
         lambda id in CFAE |
         app CFAE CFAE |
         if0 CFAE CFAE CFAE
```

**CFAE** adds dynamically scoped, first-class functions with strict evaluation semantics and removes `bind`. Your interpreter will use deferred substitution for efficiency but will not require closures as it is dynamically scoped. Perform the following:

1. Write a function, `evalDynCFAE :: Env -> CFAE -> (Maybe CFAE) `that evaluates its second argument using the environment provided in its first and returns a **CFAE** AST structure.

### Exercise 2
In this exercise you will write an interpreter for a modified **CFAE** language from the previous exercise that is statically rather than dynamically scoped. You will need to add closures and values to the interpreter to accomplish this goal.

1. Write a function, `evalStatCFAE :: Env -> CFAE -> (Maybe CFAEValue)` that interprets its second value using the environment provided in its first. This evaluator needs to return a value rather than a **CFAE** expression to implement static scoping using closures.

### Exercise 3
In this exercise you will write a pair of interpreters for a an extension of the **CFAE** language that includes the `bind` construct. This new language will be called **CFAE**. The trick is that for this exercise you will not write another interpreter at all. Instead you will write an elaborator that will translate **CFAE** language constructs into **CFAE** constructs, then call the **CFAE** interpreter:

```
CFBAE ::= number |
          id |
          CFBAE * CFBAE  |
          CFBAE - CFBAE |
          bind id = CFBAE in CFBAE |
          lambda id in CFBAE |
          app CFBAE CFBAE |
          if0 CFBAE CFBAE CFBAE
```

1. Write a function, `elabCFBAE :: CFBAE -> CFAE`, that takes a **CFBAE** data structure and returns a semantically equivalent **CFAE** structure. Specifically, you must translate the bind construct from **CFBAE** into constructs from **CFAE**.

2. Write a function, `evalCFBAE :: Env -> CFBAE -> (Maybe CBAEValue)`, that combines your elaborator and statically scoped **CFAE** interpreter into a single operation that elaborates and interprets a **CFBAE** expression.

The **CFBAE** interpreter introduces elaboration to the **CFAE** interpreter by using a function that transforms **CFBAE** abstract syntax into **CFAE** syntax before evaluation. Most of this translation is routine - there are shared constructs in the two languages. For bind we have to do a bit more work. Thankfully, not too much more.

As discussed in class, the `bind` construct can be elaborated to an application of a function. Specifically:

```
bind id = t1 in t2 == app (lambda id t1) t0
```

Thus, to evaluate a bind expression in **CFBAE**, one need simply translate it into a function application in **CFAE** and execute the result.