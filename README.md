

# Refset - Subsets with Reference Semantics for R

[![Build Status](https://travis-ci.org/hughjonesd/refset.png?branch=master)](https://travis-ci.org/hughjonesd/refset)

`refset` provides subsets with reference semantics, i.e. subsets
which automatically reflect changes in the original object, and which
optionally update the original object when they are changed. These are called
reference subsets, or refsets for short.

Traditionally, R does pass-by-value: when you do `a <- b`, `a` becomes a copy of
`b` and the two objects are completely separate. This makes sense since R is
mostly used at the command line. Values pass through a series of channels
and come out to the user at the other end, usually within a few moments.

However, it can be useful to have objects which are more closely
bound together. For example, 

