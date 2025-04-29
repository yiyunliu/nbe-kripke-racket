# Kripke-style untyped NbE in racket
[![status-badge](https://woodpecker.electriclam.com/api/badges/4/status.svg)](https://woodpecker.electriclam.com/repos/4)
An implementation of normalization by evaluation loosely based on [A
Denotational Account of Untyped Normalization by
Evaluation](https://www.brics.dk/RS/03/40/BRICS-RS-03-40.pdf) and
[Abel's thesis on NbE](https://www.cse.chalmers.se/~abela/habil.pdf).

Since the implementation is untyped, the `normalize` function only
gives you $\beta$-normal forms. However, you can get a little bit of
$\beta\eta$-equivalence by invoking Coquand's algorithm (`η-eq?`) on
$\beta$-normal forms.

I'm hoping to expand the repository into a benchmark suite where I can
visualize the performance trade-off of type-directed NbE compared to
untyped NbE composed with Coquand's algorithm.
