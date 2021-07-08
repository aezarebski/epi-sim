# Changelog for epi-sim

## 0.7.0

- Simulation functions (eg `simulationWithGenIO`) return either the simulated
  events of the summary produced by the termination handler if the simulation
  terminated early.
- The `InhomBDSCODPop` now records the number of individuals that have been
  removed and there are getter functions exported to access this information.
- Add `TerminationHandler` to provide better control over early termination of
  simulations. For example, this makes it possible to terminate the simulation
  early if certain stopping conditions are met and to call a function that
  summarises why the simulation is being terminated. This will break old code in
  that the `configuration` functions provided by the example models all now have
  an additional parameter of type `Maybe TerminationHandler`.

## 0.6.0

- Improve documentation.
- Allow a flexible start time of the simulation so it does not assume a start
  time of zero.
- Rename a bunch of the simulation functions so their use case is clearer.

## 0.5.2

- Include helper functions in `Epidemic.Types.Simulation` to make it easier to
  create PRNG with or without a fixed seed.

## 0.5.1

- Bug fix and tidy up some code.

## 0.5.0

- Add absolute times to the extinction and stopping time events to provide a
  consistent interface.
- Add the `aggregated` function to help aggregated individual level samples into
  population level samples. This is tested in `aggregationTests`.
- Add `TimeStamp` type class to abstract working with types that have an
  absolute time associated with them.
- Add `TimeInterval` type for working with intervals of time, there are also
  some helper functions to make it easier to work with intervals.
- Add the `maybeNextTimed` helper function and clean up some code in the `Time`
  module.
- Extend `ModelParameters` class to have an `eventWeights` to provide a vector
  of event weights for computing which event actually occurred.

## 0.4.2

- Include `simulationWithGenIO` and add `scRequireCherry` to the
  `SimulationConfiguration` record type to make it easier to control how
  simulations are conditioned upon particular observations.
- Documentation.
- Bug fix in edge case of no sequenced samples.

## 0.4.1

- Update the `simulationWithSystemRandom` function so that this works again.

## 0.4.0.0

- The following changes to the `EpidemicEvent` type will be the real sticking
  point in moving from `0.3.0.0` to `0.4.0.0`:
  
```
-- | Events that can occur in an epidemic with their absolute time.
data EpidemicEvent
  = Infection AbsoluteTime Person Person -- ^ infection time, infector, infectee
  | Removal AbsoluteTime Person -- ^ removal without observation
  | Sampling AbsoluteTime Person -- ^ removal and inclusion in phylogeny
  | Catastrophe AbsoluteTime People -- ^ scheduled sampling of lineages
  | Occurrence AbsoluteTime Person -- ^ removal and observed by not in phylogeny
  | Disaster AbsoluteTime People -- ^ scheduled occurrence of lineages
  | Extinction -- ^ epidemic went extinct time time can be recovered from the preceeding removal
  | StoppingTime -- ^ the simulation reached the stopping time
  deriving (Show, Generic, Eq)
```

becomes

```
-- | Events that can occur in an epidemic with their absolute time.
data EpidemicEvent
  = Infection AbsoluteTime Infector Infectee
  | Removal AbsoluteTime Person
  | IndividualSample
      { indSampTime :: AbsoluteTime
      , indSampPerson :: Person
      , indSampSeq :: Bool
      }
  | PopulationSample
      { popSampTime :: AbsoluteTime
      , popSampPeople :: People
      , popSampSeq :: Bool
      }
  | Extinction -- ^ epidemic went extinct time time can be recovered from the preceeding removal
  | StoppingTime -- ^ the simulation reached the stopping time
  deriving (Show, Generic, Eq)
```
 
- Remove CSV export, now there is only JSON export. If you want to include an
  orphan instance for working with `cassava` the following might be useful

```
instance Csv.ToRecord EpidemicEvent where
  toRecord e =
    case e of
      (Infection time person1 person2) ->
        Csv.record
          [ "infection"
          , Csv.toField time
          , Csv.toField person1
          , Csv.toField person2
          ]
      (Removal time person) ->
        Csv.record ["removal", Csv.toField time, Csv.toField person, "NA"]
      (Sampling time person) ->
        Csv.record ["sampling", Csv.toField time, Csv.toField person, "NA"]
      (Catastrophe time people) ->
        Csv.record ["catastrophe", Csv.toField time, Csv.toField people, "NA"]
      (Occurrence time person) ->
        Csv.record ["occurrence", Csv.toField time, Csv.toField person, "NA"]
      (Disaster time people) ->
        Csv.record ["disaster", Csv.toField time, Csv.toField people, "NA"]
      Extinction -> Csv.record ["extinction", "NA", "NA", "NA"]
      StoppingTime -> Csv.record ["stop", "NA", "NA", "NA"]
```

- Move the Newick material into `Epidemic.Types.Newick`.
- Remvoe the `TransmissionTree` and `SampleTree` data types because there is
  already the `EpidemicTree` and `ReconstructedTree` in the
  `Epidemic.Types.Events` which should be used preferentially.
- Remove dependency on `trifecta` since this functionality is not necessary.

## 0.3.0.0

- Add an `ARCHITECTURE.md` file to outline the structure of this package.
- Move all the temporal types into their own submodule `Epidemic.Types.Time` so
  they are easier to isolate.
- Provide a `Epidemic.Types.Observations` module to provide the functionality
  surrounding extracting the observed events from a simulation.
- Add `Extinction` and `StoppingTime` constructors for the `EpidemicEvent` type
  so that we can encode why the simulation finished in the events. As a result
  of this change, every simulation that was not terminated early should end with
  an `Extinction` event or a `StoppingTime` event.
- Update the resolver to 17.2
- Extend the `ModelParameters` class to include a `Population` parameter type
  since this is needed to compute event rates in for the logistic model.
- Create `Epidemic.Type.Simulation` module for types relating to running generic
  simulations from the models to avoid confusion as to where these are defined.
- Move class definitions into corresponding `Epidemic.Type.X` modules.
- Add a `Epidemic.Model.LogisticBDSD` module implementing a logistic birth-death
  process with unscheduled sampling and scheduled unsequenced sampling.
- Move the models into a new `Epidemic.Model` module so that it is clearer that
  these are really just examples of putting together functionality provided by
  the rest of the library.
- Use a new type `Identifier` to represent identities of people rather than a
  raw integer this way it is clearer what it really is.
- Replace `Time` with `AbsoluteTime` and `TimeDelta` types to make it explicit
  what is being represented.
- Reduce the number of models that are included to the more interesting subset.

## 0.2.2.0

- Clean up for a release candidate.

## 0.2.1.0

- Update the stack resolver to `lts-16.17` and add bounds on the package
  versions to use.
- Remove dependency upon `epi-types` by moving its modules into this package and
  include the tests from that package.
- Remove unused `Setup.hs` file.

## 0.2.0.1

- Improve documentation in `Epidemic.BDSCOD`.
- Upgrade to `epi-types-0.2.1.2` for the bug fix.

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
