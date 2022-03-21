---
title: Understanding Machine Learning 05
author: Shiguang Wu
date: 2022-03-21
---

## VCdim

here we go to one of the famous theorys, VC dimension

before go into a little deeper, we will have a look at the motivation

### motivation

first, it's clear that finity isn't a sufficient and nessesary condition of PAC learnability. [exercise 3.3](./understanding-machine-learning-02.html) is a simple example. and in the [last post](./understanding-machine-learning-04.html) (No-Free-Lunch theorem), we have seen H that contains all possible functions isn't PAC learnable. when we rethink about the proof, we may notice that the construction of set $C$ is the key point, and 'cause we are considering **all** possible functions, the error of different f's can be cancel somehow causing the large error.

borrow the idea, if we can find a subset $C$ of domain $\mathcal{X}$, and if H contains all the functions when taking $C$ as the domain, then it will cause large true risk using the same proof

further, if such kind of $C$ is infinitely large, then $H$ is not learnable

the thoughts above give us the basic idea of how VC dimesion comes

### VC dimension

---

Def. restriction of H to C

here, $\mathcal{C}=\{c_1,c_2,\dots,c_m\}\subseteq\mathcal{X}$

and $\mathcal{H}_C=\{h(c_1),\dots,h(c_m)\}$

---

Def. Shattering

H shatters on C $\iff$ $|H_C|=2^{|C|}$

---

to be cont...
