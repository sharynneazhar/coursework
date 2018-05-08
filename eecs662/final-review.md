# Final Review

## Interpreters

### Concrete and Abstract Syntax
**Concrete Syntax** is the syntax you write as a programmer as input to compilers and interpreters. For example, the block structure of C and the functional syntax of Haskell.

E.g. concrete syntax for *terms*
```hs
t ::= NUM | t + t | t - t
```

**Abstract Syntax** is the data structure representing parsed terms. It is far more useful than concrete syntax when writing interpreters and compilers.

An abstract syntax for `AE` written using a Haskell data type is:

```hs
data AE where
  Num:: int -> AE
  Plus :: AE -> AE Â-> AE
  Minus :: AE -> AE -> AE
  deriving (Show, Eq)
```

where `Num`, `Plus`, and `Minus` are *constructors* of the data type `AE` that correspond with numbers, sums, and differences in the `AE` concrete syntax.

For example `(Num 1)` is the abstract syntax for **1**. `(Plus (Num 1) (Num 3))` is the abstract syntax for **1+3**. `(Minus (Plus (Num 3 (Num 5)) (Num 1))` is the abstract syntax for **3+5-1**.

Every term in the concrete syntax must have an associated term in the abstract syntax.

### Parsing and Interpretation

A **parser** is a program that translates concrete syntax into an AST. It does the following:
1. Checks the syntax of its inputs,
2. Generates error messages if the syntax is bad, and
3. Generates an AST if the syntax is good

E.g.
```hs
parseAE :: String -> AE
```

A **pretty printer** is the inverse of a parser. It translates an `AE` data structure into a string representing the concrete syntax.

E.g.
```hs
pprintAE :: AE -> String
```

An **evaluator** converts and abstract syntax term into a value.

An **interpreter** is any program that translates syntax from one form into another.

### Values and terms

A **value** is a piece of data that cannot be interpreted further. It is a good result from evaluation.

E.g.
```hs
Num 3
```

A **term** is a valid statement or expression in a language that will be evaluated.

E.g.
```hs
Plus (Num 3) (Num 4)
```

### Miscellaneous Terms

**Deterministic** - Every invocation with the same inputs generates the same outputs

**Normalization** - Every invocation halts

**Induction** - Proof principle commonly used for countably infinite structures

**Extensionality** - Proof principle used to show two structures are equal.


## Identifiers and Substitution

### Binding identifiers to values

A **bind** expression is like a *let*. It defines a value and *binds* it to an identifier that may then be used in a subsequent expression.

E.g.
```hs
bind x = 5 + 2 in
  x + x - 4
```

This expression does the following:
1. Creates the identifier, `x`
2. Assigns it the value 5 + 2 using the concrete syntax `x = 5 + 2`
3. Defines a region where `x` can be used using the `in` keyword, `x + x - 4`.

A **binding** is simply an identifier/value pair.

### Substitution and Interpretation

The `subst` function handles substituting values for identifier or variable names.

The notation `[i->v]s` says that `i` is replaced by `v` in `s`.

E.g.
```hs
eval bind x = 5 in x + 7
== eval [x->5]x + 7
== eval 5 + 7
== 12
```

In our `BAE` data type for binding:

```hs
subst i v (Bind i' v' b') = if (i == i')
                            then (Bind i' (subst i v v') b')
                            else (Bind i' (subst i v v') (subst i v b'))
```

If the identifier being replaced is the same as the bound instance, then no substitution is performed in the `bind` body. Otherwise, substitution is performed.

### Environments and Deferring Substitution

An **environment** is a data structure containing *bindings* generated from binding instances. Environments are altered only by executing `bind`.

It is a *list of bindings*, or a list of identifier/value pairs.

```hs
type Env = [(String,BAE)]
```

A **context** is a list containing bindings of identifiers to *types* defining identifiers currently in scope.

### Identifier Scoping

An **identifier instance** is any time that identifier is used.

E.g. Here there are two instances of `x` and one instance of `y`
```hs
bind x = 5 + 2 in
  x + y - 4
```

A **binding instance** is the instance where an identifier is declared and given a value.

An **identifier scope** is the program region where an identifier can be used. We say the scope is where the identifier is **bound**.

A **bound instance** of an identifier is the use of the identifier in the scope of its binding.

A **free instance** is the use of an identifier outside the scope of its binding.


## Functions

### Interpreting functions

**First-order functions** are functions that have a special representation that is not accessible to the programmer. They cannot take other functions as arguments or return functions.

**Higher-order functions** are functions that take other functions as arguments and may return functions as values.

**First-class functions** are values treated like any other value in a language.

### Lazy and strict interpretation

**Strict** evaluation is when parameters of a function are evaluated before the function is called. This is also called *call-by-value*.

**Lazy** evaluation is when parameters are evaluated only when their values are needed. This is also called *call-by-name* or *call-by-need*.

### Static and dynamic scoping

### Closures as function values


## Type Inference

### Numbers and booleans

### Function types

### Typing expressions

### Typing lambda and app

```hs
data TFBAE where
  TNum :: TFBAE
  (:->:) :: TFBAE -> TFBAE -> TFBAE
  deriving (Show, Eq)
```

```hs
typeofM cont (Lambda x t b) = do { tyB <- typeofM ((x,t):cont) b;
                                   return (t :->: tyB) }
```

```hs
typeofM cont (App x y) = do { tyXd :->: tyXr <- typeofM cont x;
                              tyY <- typeofM cont y;
                              if (tyXd == tyY)
                              then return tyXr
                              else Nothing }
```


## Recursion

### The fix construct

### Implementing recursion


## Language Extension

### Adding concrete and abstract syntax

### Adding new values

### Extending eval and typeof

### Using elaboration

**Elaboration** is the process of transforming one expression into another that transforms one data structure into another. In other words, *translating one abstract syntax into another*.

`bind` can be rewritten in terms of `lambda`:

```hs
bind x = 5 in x + 1
```

is the same as:

```hs
app (lambda x in x + 1) 5
```



