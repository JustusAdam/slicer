# Slicer

A simple program slicer for a procedural language (but without support for
procedures).

Implementation closely follows the
[paper](https://ieeexplore.ieee.org/abstract/document/5010248/) from Mark Weiser.

There isn't terribly much you can do with this yet, but you can take a look at
`app/Main.hs`. Running the app will print out a sliced version of `testprog`
using the slicing criterion supplied in `main`, consisting of a line number and
a set of variables.
