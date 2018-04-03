# Midterm Review

## Interpreters

### Concrete and Abstract Syntax
* **Concrete syntax** is the syntax you write as a programmer as input to compilers and interpreters
* **Abstract syntax** is the data structure

#### Example
Abstract Syntax using a Haskell `data` type:

```hs
data AE where
  Num:: int -> AE
  Plus :: AE -> AE -> AE
  Minus :: AE -> AE -> AE
  deriving (Show, Eq)
```

### Parsing and Interpretation
* Parsing *should not* be on the test

### Values and terms
* Difference between a value and a term
  * A **value** cannot be evaluated further
  * A **term** is a valid statement/expression in a language.
  * e.g. `(Num 3)` versus `(Plus (Num 3) (Num 4))`
* Lambda is **definitely** a value


## Identifiers and Substitution

### Binding identifiers to values

A `bind` is like a `let` statement. It defines a value and *binds* it to an identifier that may then be used in a subsequent expression.

* **Binding Instance** - the variable name used
* **Bound Instance** - when you use a variable in scope
* **Free Instance** - a variable not defined in scope, i.e. no `let` found

#### Example 1
```hs
bind i = a in i + j
```

Defining each part of the statement above:
- **i** is the *binding instance*. It is the identifier/variable name used.
- **i = a** is the *binding*. It is the declaration of the identifier equaling some expression (by that I mean, a could equal (5*5+5).)
- **i + j** is the *scope* of the binding. It is where the declared binding applies to. Within this statement, we see that **i** is a *bound instance* and **j** is a free instance. What does this mean? It means within this scope, i has a defined value, giving by the binding. However j *is not defined currently within this scope*.


### Substitution and Interpretation

### Environments and Deferring Substitution

### Identifier Scoping
* The scope is the body

```hs
bind x = 3 in
  (bind x = 4 in
    x) + x
```

### Static and Dynamic Scoping
* Won't be asked to define but will be asked to evaluate a piece of code


## Functions

### Interpreting functions
* Given a function, show what's going on

### Lambda and App
* Understand that lambda is a `bind` broken in half
* Understand `formal` parameters
* Undaerstand how `app` works
* Won't be asked anything about `closures`

#### Example
* **Formal Parameter:** `(lambda x in x + x)`
* **Actual Parameter:** `(3 + 5 - 7)`

*This is a **strict** evaluation.*
```hs
(app (lambda x in x + x) (3 + 5 - 7))

evalM env (app f a) = do v <- evalM env a;            // evaluate the value
                         lambda i b <- evalM env f;   // evaluate the lambda
                         eval (i, v) : env b           // adds a new binding to the environment

```

*This is a **lazy** evaluation.*

```hs
(app (lambda x in x + x) (3 + 5 - 7))

evalM env (app f a) = do lambda i b <- evalM env f;   // evaluate the lambda
                         eval (i, v) : env b           // adds a new binding to the environment

```


## Misc Notes
