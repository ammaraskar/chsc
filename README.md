The Cambridge Haskell Supercompiler (CHSC)
===

Overview
---

The Cambridge Haskell Supercompiler (CHSC) is a preprocessor which supercompiles programs written in a subset of Haskell. The output is
source code suitable for presentation to the Glasgow Haskell Compiler (GHC) for compilation as standard Haskell.

The hope is that the process of supercompiling the input makes it more efficient and hence CHSC can be used as a program optimisation
tool. Because of the focus on program optimisation the design of CHSC is somewhat different from that of supercompilers used for
theorem proving:

  * We are careful to never duplicate work that is shared in the input program
  * We compile a language which models full Haskell: in particular, we can supercompile strict primops and recursive let bindings
  * We care a lot about code duplication and compile time, even if we don't have any good answers yet

The intention is to eventually integrate the supercompiler into GHC to supercompile the Core language. This will let us truly supercompile
any Haskell program, not just those written in the subset that CHSC knows how to desugar.

Bugs
---

At the time of writing, there are no known bugs in the `master` branch of the supercompiler. However, supercompiling certain programs
(even some of the examples I have provided) has never been observed to terminate due to the supercompilation code explosion problem.

Using
---

The project can be built using stack by using:

    $ stack build

Once built, the supercompiler can be executed like so

    $ stack exec supercompile examples/toys/MapMapFusion.core

This will print output and the end similar to the following:

    examples/toys/MapMapFusion.core
    mapmapfusion & 0.0s & 0.50 & 0.82 & 1.02 \\

The fields on the second row are, from left to right:

  1. The name of the benchmark
  2. The time taken to supercompile (in seconds)
  3. Time taken by GHC to compile to the supercompiled program, as a fraction of the time taken for the input program
  4. Runtime of the supercompiled program, as a fraction of that of the input program
  5. Total heap allocation by the supercompiled program as a fraction of that of the input

In addition to supercompiling and running the benchmarks, the test script outputs the supercompiled forms of the programs:

  * The input program (as a valid Haskell program) is placed in `input/examples/toys/MapMapFusion.hs`
  * The output program (again, as valid Haskell) is placed in `output/primops/gen/examples/toys/MapMapFusion.hs`

Naturally, the suffix of this file name varies with the file name that `test` was invoked with.

You can use `test` with more than one filename if you would like.

Paper
---

A description of the design of CHSC was accepted to the Haskell Symposium 2010, and is available online:

  * [Supercompilation by Evaluation](http://www.cl.cam.ac.uk/~mb566/papers/sbe-hs10.pdf)
