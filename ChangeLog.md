# Changelog for epi-sim

## 0.2.0.0

- Predicates such as `isSampling` and `isOccurrence` have been replaced with
  clearer named alternatives: `isNonReconTreeObservation` and `isReconTreeLeaf`
  to avoid uncertainty about whether scheduled events are included.
- Upgrade to `epi-types-0.2.1.0` which adds substantial amounts of Newick
  functionality meaning this can be removed.

## 0.1.9.0

- Upgrade to `epi-types-0.2.0.0` which makes a lot of functions redundant so
  they have been removed.

## 0.1.8.8

- Upgrade to `epi-types-0.1.1.1` and remove `joinTimed` since it is no longer
  needed.

## 0.1.8.7

- Upgrade to `epi-types-0.1.1.0` and start using the `Timed *` class.

## 0.1.8.6

- Use `-j2` to slightly improve compilation time.
- Include a `observedEvents` function which was missing from the
  `InhomogeneousBDS` module.

## 0.1.8.5

- Start using cabal directly without hpack.
- Move time dependent parameters into a `epi-sim` package so they are easier to
  share, and import them here.

## 0.1.8.4

- Improve documentation of construction of `inhomBDSRates` and include a test to
  check this conforms to expectations.
- Implement a birth-death-sampling model with time-dependent birth rates and
  include an example of what this returns in the README.

## 0.1.8.3

- Improve error handling in `birth-death-lines.R`.
- Refactor some of the plotting code in `birth-death-lines.R` to make it easier
  to maintain.
- Use the `future` and `furrr` packages in R to generate the visualisation of a
  simulation in parallel since the serial version is frustratingly slow. Both
  packages are on CRAN so it does not seem unreasonable.

## 0.1.8.2

- Include an additional parameter in the `simulation` and
  `simulationWithSystemRandom` to determine whether the simulation should be
  conditioned on having at least two sampled leaves in the reconstructed tree.
  It does this by repeatedly simulating until such a tree is acheved.

## 0.1.8.1

- Implement an example of the inhomogeneous birth-death process and include it
  in the `README`.
- Fix broken example using the BDSCOD model and organise examples in `examples/`
  rather than just the single one in `app/`.

## 0.1.8.0

- Define an inhomogeneous birth-death process in `InhomogeneousBD`.
- Define an inhomogeneous exponential random sampler using `Timed Rate`s.
- Extend the `ModelParameters` class to have a `birthProb` function.
- Include a `Timed` type for time varying values.
- Include a test that the final size of a birth-death simulation is
  approximately correct.
- Format the changelog to make it easier to read outside of a browser.
- Fix naming convention and exports in the `BirthDeath`, `BirthDeathSampling`
  and `BirthDeathSamplingOccurrence`modules to conform to the pattern used in
  the `BDSCOD` module and move `simulation` into the `Utility` module since it
  is common between all models
- Adjust the type of the `ModelParameters` interface to include potential time
  dependence.

## 0.1.7.2

- Make `Event` an instance of `FromRecord` to match with `ToRecord`.

## 0.1.7.1

- Provide `vis/birth-death-lines.R` which visualises a simulation.

## 0.1.7.0

- Provide `vis/ott.R` to visualise observations through time and make
  `vis/ltt.R` ignore disasters.
- Replace some Catastrophe specific functions with equivalents for handling
  scheduled events more generally
  + `noCatastrophe` becomes `noScheduledEvent`
  + `firstCatastrophe` becomes `firstScheduled`
- Change the naming of the BDSCO model to be consistent with the new BDSCOD
  model.
- Implement a birth-death-sampling-catastrophe-occurrence-disaster model in
  `Epidemic.BDSCOD`.

## 0.1.6.2

- Update the visualisation code in `vis/ltt.R` such that it parses the
  catastrophe events to draw the LTT.
- BUG FIX: In the observed events catastrophes are replicated when they should
  not be.

## 0.1.6.1

- Include a visualisation script for the LTT of reconstructed trees: `vis/ltt.R`
  which is described in the README.

## 0.1.6.0

- Move the `ToField` declarations into the `Epidemic` module to avoid orphaned
  instance and select a representation for multiple people in the CSV output.
- Implement a birth-death-sampling-catastrophe-occurrence model in
  `Epidemic.BirthDeathSamplingCatastropheOccurrence`; it's a shame about the
  length of the name.
- Include a contructor for catastrophe events in which multiple people can be
  sampled simultaneously.

## 0.1.5.1

- Include a `.gitignore`.
- Improved documentation with `haddock`.

## 0.1.5.0

- Include some testing with `hspec`.
- The `Epidemic.BirthDeathSamplingOccurrence` module now has a function to
  extract the observable events.
- Change the simulation functions to return events in the order they occurred.
- Change the configuration functions to take tuples of model parameters.

## 0.1.4.0

- Make `Event` an instance of the `Ord` typeclass.
- Implement types and conversion functions for a `TransmissionTree` and a
  `SampleTree`.
- Implement a parser and printer for Newick in `Epidemic.Utility`.

## 0.1.3.0

- Implement a birth-death-sampling-occurrence model in
  `Epidemic.BirthDeathSamplingOccurrence`.
- Implement a visualisation of the birth-death-sampling LTT in
  `vis/birthDeathSamplingVisualisation.R`.
- Implement a birth-death-sampling model in `Epidemic.BirthDeathSampling`.

## 0.1.2.0

- Implement a visualisation of the infection network in
  `vis/infection-tree.vg.json`.
- Implement `Epidemic.Utility.eventsAsJsonTree`.
- Move the `Epidemic.Simulation` module to `Epidemic.Utility`.

## 0.1.1.0

- Write the output to CSV using `cassava`

## 0.1.0.0

- Basic prototype to simulate a constant rate birth-death process.
