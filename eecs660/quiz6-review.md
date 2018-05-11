# Quiz 6 Review

## Shortest Path Problem

> Given a weighted directed graph G = (V, E), and the cost/length for each edge (denote the length of edge e as l<sub>e</sub> and l<sub>e</sub> &geq; 0 for all e &in; E), find the shortest path d(v) from vertex u to v whose total length is minimized among all paths from u to v.

### Djikstra's Algorithm

```
Initialize S = {u} and d(s) = 0
while S does not equal v
  select a node v not in S with at least one edge from S for which d(v) = m(d(u) + l);
  add v to S, record d(v)
endwhile
return d(v)
```

### Proofs

> For any given set S that is generated at any step, for each v &in; S, d(v) &leq; d(P'<sub>v</sub>). d(P'<sub>v</sub>) is an arbitrary path that goes from s to v.

We will prove this by induction.

Starting with S = {s}, the statement is clearly correct since d(s) = 0 and l<sub>e</sub> &geq; 0 for all e &in; E. Suppose the statement holds when |S| = k for some value k &geq; 1, we want to show that the statement is also true when we add a new vertex into S and make |S| = k + 1. Now, assume that there exists another path P'<sub>v</sub> that can reach v.

Observe that v &notin; S, so there exists an edge with one end in S and another end in V-S. Let (x, y) be the first of these edges in P'<sub>v</sub> and we have x &in; S and y &in; V-S. Therefore, the total length of P'<sub>v</sub> is thus d(P'<sub>v</sub>) = d(P'<sub>x</sub>) + l<sub>(x,y)</sub> + d(y,...,v) &geq; d(x) + l<sub>(x,y)</sub> + d(y,...,v).

Also, according to the algorithm, v is selected because d(v) = min(d(u) + l<sub>e</sub>). It follows that d(x) + l<sub>(x,y)</sub> &geq; d(u) + l<sub>(u,v)</sub>. Plugging it into the equation, we get d(P'<sub>v</sub>) &geq; d(x) + l<sub>(x,y)</sub> + d(y,...,v) &geq; d(u) + l<sub>(u,v)</sub> + d(y,...,v) &geq; d(u) + l<sub>(u,v)</sub> = d(u).

## Divide and Conquer

### Merge Sort

