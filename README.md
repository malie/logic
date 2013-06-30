
## what is this?

The function cnf-to-ddnnf can convert a formula encoded in CNF into d-DNNF.

d-DNNF is an encoding of logic formulas conforming to certain rules:
1. all negations are at the leaves.
2. in all OR nodes, exactly one sub-clause is true, called 'deterministic'.
3. the set of variables appearing in a sub-clause of an AND node does not
   overlap with those of a different sub-clause of the same AND node,
   called decomposable.

## why?

d-DNNF's are a relatively compact representation of all solutions of a CNF,
at least more compact than listing all solutions.


## how is this implemented?

It uses very similar techniques DPLL solvers use:

* identify unit clauses
* split by assuming values for variables according to some simple heuristics
* separately solve independent partitions of a large cnf


## example

one-of-three written in CNF as

    (a or b or c)
      and (not a or not b)
      and (not a or not c)
      and (not b or not c)

is encoded as

    ((1 2 3) (-1 -2) (-1 -3) (-2 -3))

and converted to d-DNNF by

    CL-USER> (cnf-to-ddnnf '((1 2 3) (-1 -2) (-1 -3) (-2 -3)))

    (OR (AND 2 -1 -3) (AND -2 (OR (AND 1 -3) (AND -1 3))))

## another example

    CL-USER> (cnf-to-ddnnf '((1 2 3) (-3 4 5)))

    (OR (AND 3 (ND-OR 4 5) (INDEPENDENT 1 2))
        (AND -3 (ND-OR 1 2) (INDEPENDENT 4 5)))

## output format

* OR nodes are deterministic, i.e. by construction only one of the OR arguments is true
* AND nodes are decomposable
* ND-OR nodes are non-deterministic OR nodes. A shortcut for trivially expanded OR.
  Only used at the leaves for a syntactically shorter result.
* INDEPENDENT nodes are nodes allowing any truth value for the given variables.
  Another shortcut that can be trivially expanded.
  Only used at the leaves for a syntactically shorter result.

## loading

Code should load in any Common Lisp environment, use

    (load "dpll.lisp")
    (load "ddnnf.lisp")


## todos

Are the produced d-DNNF's really smooth?

