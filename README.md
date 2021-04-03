# SuSLiX (Synthesis with Separation Logic and eXamples)

[![Build Status](https://travis-ci.org/TyGuS/suslik.svg?branch=master)](https://travis-ci.org/TyGuS/suslik)
[![License](https://img.shields.io/badge/License-BSD%202--Clause-orange.svg)](https://raw.githubusercontent.com/TyGuS/suslik/master/LICENSE)
[![DOI](https://zenodo.org/badge/101061595.svg)](https://zenodo.org/badge/latestdoi/101061595)

An Extension of SuSLik (Synthesis of Heap-Manipulating Programs from Separation Logic
Specifications) with Examples.

<p align="center">
  <a href = "http://comcom.csail.mit.edu/comcom/#SuSLik"><img src="https://github.com/TyGuS/suslik/blob/master/misc/suslik-logo.png" width="150" height="150"></a>
  </p>



## Theory Behind the Tool

The details of Synthetic Separation Logic can be found in the
[accompanying draft paper](https://arxiv.org/pdf/1807.07022.pdf).

The Github repository with SuSLiK-related instructions can be found at (https://github.com/TyGuS/suslik).

The details of SuSLiX can be found in the Capstone report.

## Setup and Build

### Requirements 

* [Java SE Development Kit 8](http://www.oracle.com/technetwork/java/javase/downloads/jdk8-downloads-2133151.html)
* [Scala Build Tool](https://www.scala-sbt.org/), `sbt` (version >=1.1.6)
* [Z3 SMT solver](https://github.com/Z3Prover/z3)
* [CVC4 SMT solver](https://cvc4.github.io/), version 1.7
* [Cyclist Theorem Prover](http://www.cyclist-prover.org/installation)
* [Scala](https://www.scala-lang.org/download/) (version >= 2.12.6) - to run the standalone artifact

## Running the Synthesizer on Case Studies in the Capstone Report

To compile the executables, run the following in your command line:

```
sbt assembly
```

to produce an executable `JAR`-file that you can run as explained below.

The following case studies that were explored in the Capstone report can be found in the folder

`$PROJECT_ROOT/src/test/resources/synthesis/exampledriven`

The definitions of inductive predicates are given in the single `.def`-file, in thie case called `mydefs.def`, whose contents are as follows:

```
predicate lseg(loc x, loc y, set s) {
|  x == y        => { s =i {} ; emp }
|  not (x == y)  => { s =i {v} ++ s1 ; [x, 2] ** x :-> v ** (x + 1) :-> nxt ** lseg(nxt, y, s1) }
}


predicate treeS(loc x, set s) {
|  x == 0        => {s =i {}; emp}
|  not (x == 0)  => {s =i {v} ++ s1 ++ s2 ; [x, 3] ** x :-> v ** (x + 1) :-> l ** (x + 2) :-> r ** treeS(l, s1) ** treeS(r, s2)}
}


```

The remaining files (`*.syn`) are the actual case studies, which will be explained in their corresponding sections below. They are structured in the following format:


```
<A textual comment about what capability of the synthesizer is being assessed.>
#####
<Hoare-style specification of the synthesized procedure in SL>
#####
<Input-output examples specified in the style explained in the Capstone report>
#####
<Optional expected result>
```

To run the synthesizer on the case study, execute the following script:

```
suslik fileName [options]
```

where the necessary arguments and options are

```
  fileName                 a synthesis file name (case studies found in $PROJECT_ROOT/src/test/resources/synthesis/exampledriven)
  -r, --trace <value>      print the entire derivation trace; default: true
  -t, --timeout <value>    timeout for the derivation; default (in milliseconds): 300000 (5 min)
  -d, --depth <value>      derivation depth; default: 100
  -a, --assert <value>     check that the synthesized result against the expected one; default: true
  -c, --maxCloseDepth <value>
                           maximum unfolding depth in the post-condition; default: 1
  -o, --maxOpenDepth <value>
                           maximum unfolding depth in the pre-condition; default: 1
  -b, --branchAbduction <value>
                           abduce conditional branches; default: false
  --commute <value>        only try commutative rule applications in one order; default: true
  --phased <value>         split rules into unfolding and flat phases; default: true
  --fail <value>           enable early failure rules; default: true
  --invert <value>         enable invertible rules; default: true
  -s, --printStats <value> print synthesis stats; default: true
  -e, --printEnv <value>   print synthesis context; default: false
  -f, --printFail <value>  print failed rule applications; default: false
  -g, --tags <value>       print predicate application tags in derivations; default: false
  -l, --log <value>        log results to a csv file; default: true
  --help                   prints this usage text

```

### `pick`
`$PROJECT_ROOT/src/test/resources/synthesis/exampledriven/pick.syn` is defined as follows:

```
should be able to synthesize a two-pointer arbitrary constant-assigning procedure with hints to guide the choice of constant
using incomplete SL specifications, with the help of examples.
###
{true; x :-> a ** y :-> b} void bar(loc x, loc y) {true ; y :-> z ** x :-> z}
###
[x :-> a ** y :-> b] [x :-> a ** x :-> a]
###
Output program:

{x :-> a ** y :-> b}
{x :-> z ** y :-> z}
void bar (loc x, loc y) {
  let b = *y;
  *x = b;
}

or

{x :-> a ** y :-> b}
{x :-> z ** y :-> z}
void bar (loc x, loc y) {
  let a = *x;
  *y = a;
}
```
### `fstElement`
`$PROJECT_ROOT/src/test/resources/synthesis/exampledriven/fstElement.syn` is defined as follows:

```
should be able to synthesize a program that gets the first element of a linked list with incomplete SL specifications,
with the help of examples.
#######

{true ; ret :-> x ** lseg(x, 0, S1)}
void fstElement(loc ret)
{true ;  ret :-> v ** lseg(x, 0, S)}

######

[ret :-> x ** x :-> v] [ret :-> v  ]

#########
output program:

{ret :-> x ** lseg(x, 0, S1)<_alpha_525>}
{ret :-> v ** lseg(x, 0, S)<_alpha_526>}
void fstElement (loc ret) {
  let x = *ret;
  let v = *x;
  *ret = v;
}
```

### `treeRightChild`

`$PROJECT_ROOT/src/test/resources/synthesis/exampledriven/treeRightChild.syn` is defined as follows:

```
should be able to synthesize a program that gets the right child of a tree using incomplete specifications, with the help of examples
#####

{true ; ret :-> x ** treeS(x, S1)}
void treeRightChild(loc ret)
{true ; ret :-> r ** treeS(x,S)}

#####
[ret :-> x ** (x+2) :-> r] [ret :-> r ]
#####
Output program:

void treeRightChild (loc ret) {
  let x = *ret;
  let r = *x+2;
  *ret = r;
}
```