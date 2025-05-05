# Kripke-style untyped NbE in racket
[![status-badge](https://woodpecker.electriclam.com/api/badges/4/status.svg)](https://woodpecker.electriclam.com/repos/4)

An implementation of normalization by evaluation loosely based on [A
Denotational Account of Untyped Normalization by
Evaluation](https://www.brics.dk/RS/03/40/BRICS-RS-03-40.pdf) and
[Abel's thesis on NbE](https://www.cse.chalmers.se/~abela/habil.pdf).

Since the implementation is untyped, the `normalize` function only
gives you β-normal forms. However, you can get a little bit of
βη-equivalence by invoking Coquand's algorithm (`η-eq?`) on
β-normal forms.

A more efficient version of NbE based on de Bruijn levels (inverted de
bruijn indices) can be found in the `debruijn-levels` branch.
