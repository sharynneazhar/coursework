# Quiz 2 Review

## Stable Matching Problem

Assume that we have m men and n women. Each man gives his preference on the women and each women gives her preference on the man. No tie is allowed in their rankings.

The **stable matching problem** seeks to find a set of one-to-one pairs between the men and women such that no instability is present.

**Instability** means that there exist two pairs where both the man in one pair and the woman in another prefer each other over their respective partners (and therefore have a strong motivation to break with their partners to form a new pair).

More formally:
> There exists pairs (m, w) and (m', w') where m and m' are two individual men and w and w' are two individual women. If m prefers w' over w, and w' prefers m over m', then m and w' would break with their current partners and pair up.

There are three properties of stable matching:
1. Termination
2. Perfect Matching
3. Stable (or no instability)

### Gale & Shapley Algorithm

```
Initialize each person to be free
while (some man is free and hasn't proposed to every woman) {
  choose such a man m
  w = first woman on m's list to whom m has not yet proposed
  if (w is free)
    assign m and w to be engaged
  else if (w prefers m to her fiance m')
    assign m and w to be engaged
    free m'
  else
    w rejects m
}
```

#### PseudoCode

**Input**: Two 2D nxn array, `mR` for men's rankings and `wR` for women's rankings where `mR[i][j]` is the ID of the woman who man i ranks on the j<sup>th</sup> palce. Similarly, we define `wR`.

```
Initialize 1D array next_ID;
for i in 1 to n:
  next_ID[i] = 0;
endfor

Initialize a 1D array w_engaged;
for i in 1 to n:
  w_engaged[i] = -1;
endfor

Initialize a 2D array for w_pref_rank;
for i in 1 to n:
  for j in 1 to n:
    w_pref_rank[i][wR[i][j]] = j;
  endfor
endfor

Initialize queue as an empty queue;
for i in 1 to n:
  queue.enqueue(i);
endfor

while (!queue.empty()) {
  m = queue.dequeue();
  w = mR[m][next_ID[m]];
  ++next_ID[m];

  if w_engaged[w] < 0:
    w_engaged[w] = m;
  else if w_pref_rank[w][m] < w_pref_rank[w][w_engaged[w]]:
    queue.enqueue(w_engaged[w]);
    w_engaged[w] = m;
  else
    queue.enqueue(m);
}
```

**Output**: `w_engaged` as the stable matching set where men `w_engaged[i]` will be matched with woman i.

### Proofs

> The GS algorithm terminates.

Note that each man will not propose to the same woman more than once. There exist only a limited number of man and woman. So, the algorithm will always terminate.

> There exists no single man nor woman who is not paired.

Note that once a woman is proposed to she will become engaged and never become free again. A woman will also only "trade up" according to the algorithm.

Let an arbitrary man, m, be unmatched. Then, there exists another woman, w, being unmatched because each man can pair with only one woman.

Since w is unmatched, according to our algorithm, w is never proposed to because for any woman, if she gets engaged, she will never be free again.

However, the algorithm terminates (a given premise) which means that all men have proposed to all women. This implies that m has proposed to all women.

This is a contradiction to the fact that w is never proposed to.

Hence, we can conclude that there exists no man nor woman being unmatched upon the termination of the algorithm.

> There exists no instability upon termination of the algorithm.

Suppose there exists an unstable pair (m, w'). Upon termination of the algorithm, m is paired with w and m' is paired with w'. In other words, we have (m, w) pair and (m', w') pair in the current matching.

There are two cases:

1. m has not proposed to w'.  Since men propose in decreasing order, m must prefer w over w'. In this case, the pair (m, w') is not an unstable pair which contradicts with our assumption that (m, w') is an unstable pair.

2. M has proposed to w'. In this case, no matter whether m is rejected immediately or later, w' prefers m' over m because women only "trade up". So, this also contradicts with our assumption that (m, w') is an unstable pair.

In either case, (m, w') is stable therefore we can conclude that there exists no unstable pair upon the termination of the algorithm.

> Every execution of the GS algorithm result in the same matching set.

Proving the results of different executions is the same as proving that for every execution of the GS algorithm, all men get his best valid partner.

**Proposition**: For every execution of the GS algorithm, all men get their best valid partner.

In this case, we define a **best valid partner** as the partner highest in preference.

Let us suppose, by way of contradiction, that some execution &epsilon; of the G-S algorithm results in a matching S in which some man is paired with a woman who is not his best valid partner. Since men propose in decreasing order of preference, this means that some man is rejected by a valid partner during the execution &epsilon; of the algorithm. So consider the first moment during the execution &epsilon; in which some man, say m, is rejected by a valid partner w. Again, since men propose in decreasing order of preference, and since this is the first time such a rejection has occurred, it must be that w is m’s best valid partner best(m).

The rejection of m by w may have happened either because m proposed and was turned down in favor of w’s existing engagement, or because w broke her engagement to m in favor of a better proposal. But either way, at this moment w forms or continues an engagement with a manm whom she prefers to m.

Since w is a valid partner of m, there exists a stable matching S containing the pair (m, w). Now we ask: Who is m paired with in this matching? Suppose it is a woman w = w.

Since the rejection of m by w was the first rejection of a man by a valid partner in the execution &epsilon;, it must be thatm had not been rejected by any valid partner at the point in &epsilon; when he became engaged to w. Since he proposed in decreasing order of preference, and since w is clearly a valid partner of m, it must be that m prefers w to w. But we have already seen that w prefers m to m, for in execution &epsilon; she rejected m in favor of m. Since (m, w) ∈ S, it follows that (m, w) is an instability in S.

This contradicts our claim that S is stable and hence contradicts our initial assumption.

### Time Complexity

The algorithm has a **O(n<sup>2</sup>)** complexity.

#### Analysis

* Identifying if a free man exists:
  * We can maintain a linked list to record all free men. We can insert/delete an element from the linked list in a constant number number of operations. There is no order specified in the algorithm, so we can use a FIFO structure for the proposals. Also, if an engaged man becomes free, we can add him to the end of the linked list.

* Identifying the highest-ranked woman that he has not proposed to:
  * We can assign IDs to men and women in some order. We can maintain an array of size n and let `next_ID[i]` be the highest-ranked woman who man i has not proposed to. We increase `next_ID[i]` by one for each execution of the while loop.

* Identifying whether a woman is currently engaged and, if yes, her current partner:
  * We can maintained the information in an array with size n, similar to above. We update `w_engaged[i]` if woman i accepts the proposal.

* Determining, for a given women, whether she prefers the current proposing man over her current partner:
  * We can maintain the information in an nxn array where `w_pref_rank[i][j]` stores the ranking of man j in woman i's list. We can compare the rankings of the two men by looking up the table in constant time. Also, note that the table can be pre-constructed in n<sup>2</sup> time, and therefore it is with the same order of magnitude as the number of the while loop.

