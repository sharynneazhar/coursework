# Quiz 1 Review

## Intro to Algorithms

An **algorithm** is a finite, definite, effective procedure with some input and some output.

In terms of analysis of algorithms, the two properties of an algorithm we should analyze are:
1. Correctness
2. Efficiency

A **statement** is a sentence which is either true or false.

A **predicate** is a sentence that contains a finite number of variables.

E.g. "He is a student of KU" where "He" is a statement variable.

A predicate can be transformed into a statement in two ways:
1. By specifying values for the predicate variables
2. By applying a quantifier

A **quantifier** has two types:
1. &forall; - Universal ("for all")
2. &exist; - Existential ("there exists a")

There are four ways to construct a **compound statement**
1. Negation
2. Conjunction
3. Dysjunction
4. Conditional

Two statements are **logically equivalent** if and only if they have identifcal truth values for each possible substitution of statements for their statement variables.

Logical equivalency can be proved using a **truth table**.

An **argument** is a sequence of statements.

A **premise**  is all statements in an argument except for the last one.

A **conclusion** is the last statement in an argument.

A **valid argument** is when no matter what values are substituted for the statement variables in the premise, when all the resulting premises are true, the conclusion is also true.

An argument can be valid even if not all the premises hold:

E.g. Proof by counterexample that not every rockstar has red hair.
> If John Lennon was a rockstar, then John Lennon had red hair. John Lennon was a rockstar so John had red hair.

All premises can be true even if the argument is invalid:

> If New York is a big city, then New York has tall buildings. New York has tall buildings, New York is a big city.

A **sound argument** is when all of its premises are true and the argument form is valid.

## Proofs

### Proof by Contradiction

> If a directed graph with a finite number of vertices has no cycle, there exists at least one vertex without an incoming edge.

Suppose that all vertices in the graph has at least one incoming edge.

We can traverse the graph in the following way. Begin at an arbitrary vertex and traverse to any arbitrary ancestor vertex by tracing back the incoming edge. Note that because we assume that all vertices have at least one incoming edge, the traversal will never halt.

Since the graph only has a finite number of vertices, say n vertices, after n + 1 traces, at least one vertex will be visited twice. By definition, the traversal contains a cycle. We assumed that the graph does not contain any cycle, so this is a contradiction.

Therefore, our assumption that all vertices in teh graph has at least one incoming edge is wrong. This means that there must exist at least one vertex without an incoming edge.

### Proof by Contraposition

> For all integers, if n<sup>2</sup> is even, then n is even.

Suppose that for all integers, if n is odd, then n<sup>2</sup> is odd.

By definition of odd integers, since n is odd, then n = 2x + 1 for some integer x. We can substitute this into n<sup>2</sup>. This gives us:

n<sup>2</sup> = (2x + 1)<sup>2</sup>
= 4x<sup>2</sup> + 4x + 1
= 2(2x<sup>2</sup> + 2x) + 1

Let k = 2x<sup>2</sup> + 2x. We get n<sup>2</sup> = 2k + 1 which by definition is also an odd integer.

Hence, we can prove that for all integers, if n<sup>2</sup> is even, then n is even.

### Proof by Induction

> For all integers k &ge; 8, k cents can be obtained by using only 3-cent and 5-cent coins.

Base Step: It is true that for k = 8, we can use one 3-cent and one 5-cent coins.

Induction Step: Assume that k cents can be obtained using 3-cent and 5-cent coins, then k + 1 cents can also be obtained with 3-cent and 5-cent coins. There are two cases:

1. If at least one 5-cent coin is used in obtaining k cents, we can substitute the 5-cent coin with two 3-cent coins to obtain k + 1 cents, where k + 1 cents can also be obtained using only 3-cent and 5-cent coins.

2. If no 5-cent coin is used, then at least three 3-cent coins were used to obtain k cents. This is because k &ge; 8 and 8 cents cannot be obtained using only two 3-cent coins. In this case, we can substitute the three 3-cent coins with two 5-cent coins and obtain k + 1 cents. Therefore, we prove that k + 1 cents can also be obtained using only 3-cent and 5-cent coins.

We conclude that for all integers k &ge; 8, k cents can be obtained using 3-cents and 5 -ent coins.

For all integers, if n<sup>2</sup> is even, then n is even.

