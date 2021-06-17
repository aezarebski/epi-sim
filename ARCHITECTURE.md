# ARCHITECTURE

This document describes the high-level structure of the `epi-sim` package to
make it easier to navigate the source code.

## Modules

The `Epidemic` and `Epidemic.Utilty` modules provide basic functions for
simulating epidemics and working with the resulting data sets, eg simulate a
birth-death process filter out the observed cases and write them to CSV. The
submodules under `Epidemic.Types` provide combinators and functionality for
working these these data and writing your own simulations. There are some basic
simulation models already provided in the `Epidemic.Model` submodules. For the
most part, writing a new epidemic model revolves around definining the
`randomEvent` function.

### Time

For absolute times there is the `AbsoluteTime` type and for differences between
times there is the `TimeDelta` type. For quantities that vary across time the
`Timed` type is a way to represent piecewise constant functions and there are
several helper functions to query these objects.

There is the 'TimeStamp' type class for things that have an absolute time
associated with them.
