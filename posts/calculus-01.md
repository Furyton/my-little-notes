---
title: calculus review 01
author: Furyton
date: 2022-03-28
tags: caluclus, review
---

## basic

linear transformations

measure of matrices: $\left \rVert A\right \lVert_F^2$

triagle inq in matrices:  $\rVert AB \lVert\le \rVert A\lVert\rVert B\lVert$


neighborhood of $x$, exist an open ball init

closure  $\bar{A}$, smallest close set that contains A

interior  $\mathring{A}$, largest open set that is contained in A

boundary of subset, $\partial A$

---

convergence of sequence, in terms of coordinates

limits of multivariable functions: continuity is preserved under dot product operation

continuity: the preimage of a neighborhood of $f(x)$ is also a neighborhood of x 

uniform continuity: linear transformations are uniform continuity

convergence of the sum of series (vectors): absolute(*norm* in vector cases) convergence implies convergence

complext exponentials

- complex exponential series converges: $e^z=1+z+\frac{z^2}{2!}+\dots=\sum_{k=0}^\infty \frac{z^k}{k!}$, since the absolute series converges

euler formular:  $e^{it}=cost+isint$

geometric series of matrices:

- $S=I+A+A^2+\dots$ converges to $(1-A)^{-1}$ if $\lVert A \rVert \lt 1$ 

- the set of invertable n by n matrices is open

bounded: subset $X\subset \R^n$ is **bounded** if it is contained in some ball centered at the origin

compact: nonempty subset  $C\subset \R^n$ is compact if it is **closed** and **bounded**

## important theorem

- Bolzano-Weierstrass theorem: a compact set C contains a seq, then that seq has a convergent sub seq whose limit is in C

  - 
