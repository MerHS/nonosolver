# nonosolver-haskell

Nonogram solver

## Input

```
Height Width
[Row Hints]^h
[Column Hints]^w
```

e.g.

```
3 3
1 1
2
1 1
3
1
1 1
```

## output

```
■□■
■■□
■□■
```

## Tactics

Apply eachtTactics Row-by-Row and Column-by-Column until the board is fixed

* Tactic 1. Segmentize & Solve Left-most Hints
  1. Check pivoted hint is solved / discriminate contradiction
  2. Split over solved / pivoted hint
    2.1. Turn off remainders with pivoted on-segment 
  3. Dispose each hints to the left-most position
    3.1. Record Min, Max, Set of hints up to present.
    3.2. If some On-cell overlaps with current hint, discriminate it by that record.
    3.3. Fix pivot if present
  4. Flip every state & Dispose by right-most position
  5. Flip
  6. Calculate expected left to right range of each segment
  7. if some on-segment is touched with only one hint, solve or pivot it.
  8. Repeat 1-7 until the whole the row is fixed

* Tactic 2. Mergeable On-segment
  1. Discriminate each on-segment is mergeable with neightbor on-segment
    1.1 Use expected left-right range in tactic 1.
  2. if every on-segment is unmergeable & number of unresolved hint is matched with the number of unmergeable, set pivots or resolve it.
    2.1. Turn off redundancies by pivots.
