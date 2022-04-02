---
title: Understanding Machine Learning 06
author: furyton
date: 2022-03-30
tags: UML
---

we will summarize different kinds of defs of learnability first

## learnability

### PAC learnability

H is PAC learnable if the realizability assumption holds and

$\forall \epsilon,\delta\gt 0$, there exists a learner A and $m_H(\epsilon,\delta)
$ s.t. for any sample sequence S with size $m\ge m_H$

with probability greater than $1-\delta$

$$
L_D(A(S))\le \epsilon
$$

### agnostic PAC learnability

H is agnostic PAC learnable if

$\forall \epsilon,\delta\gt 0$, there exists a learner A and $m_H(\epsilon,\delta)$ s.t. for any sample sequence S with size $m\ge m_H$

with probability greater than $1-\delta$

$$
L_D(A(S))\le \min_{h\in H}\{L_D(h)\}+\epsilon
$$

### uniform convergence property

H enjoys the uniform convergence property if

$\forall \epsilon,\delta \gt 0$, there exists $m_H^{UC}(\epsilon, \delta)$ s.t. for any sample sequence S with size $m\ge m_H^{UC}$

with probability greater than $1-\delta$, $\forall h\in H$

$$
|L_S(h) - L_D(h)|\le\epsilon
$$

these defination of learnibility are equal according to the fundamental theory

---

here we have another def which has different power with the above

## nonuniform learnability

we allow $m_H$ to be non-uniform over h

H is nonuniform learnable if

$\forall \epsilon,\delta\gt 0$ there exists a learner A and $m_H^{NUC}(\epsilon,\delta,h)$, s.t. for all $h\in H$ and any sample with size $m\ge m_H^{NUC}(\epsilon,\delta,h)$

with probability greater than $1-\delta$

$$
L_D(A(S))\le L_D(h)+\epsilon
$$

note when m is decided, and it is that makes non-uniform learnability weaker than PAC
