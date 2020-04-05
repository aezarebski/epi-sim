# Changelog for epi-sim

## 0.1.7.0

- Provide `vis/ott.R` to visualise observations through time and make `vis/ltt.R` ignore disasters.
- Replace some Catastrophe specific functions with equivalents for handling scheduled events more generally
  + `noCatastrophe` becomes `noScheduledEvent`
  + `firstCatastrophe` becoes `firstScheduled`
- Change the naming of the BDSCO model to be consistent with the new BDSCOD model.
- Implement a birth-death-sampling-catastrophe-occurrence-disaster model in `Epidemic.BDSCOD`.

## 0.1.6.2

- Update the visualisation code in `vis/ltt.R` such that it parses the catastrophe events to draw the LTT.
- BUG FIX: In the observed events catastrophes are replicated when they should not be.

## 0.1.6.1

- Include a visualisation script for the LTT of reconstructed trees: `vis/ltt.R` which is described in the README.

## 0.1.6.0

- Move the `ToField` declarations into the `Epidemic` module to avoid orphaned instance and select a representation for multiple people in the CSV output.
- Implement a birth-death-sampling-catastrophe-occurrence model in `Epidemic.BirthDeathSamplingCatastropheOccurrence`; it's a shame about the length of the name.
- Include a contructor for catastrophe events in which multiple people can be sampled simultaneously.

## 0.1.5.1

- Include a `.gitignore`.
- Improved documentation with `haddock`.

## 0.1.5.0

- Include some testing with `hspec`.
- The `Epidemic.BirthDeathSamplingOccurrence` module now has a function to extract the observable events.
- Change the simulation functions to return events in the order they occurred.
- Change the configuration functions to take tuples of model parameters.

## 0.1.4.0

- Make `Event` an instance of the `Ord` typeclass.
- Implement types and conversion functions for a `TransmissionTree` and a `SampleTree`.
- Implement a parser and printer for Newick in `Epidemic.Utility`.

## 0.1.3.0

- Implement a birth-death-sampling-occurrence model in `Epidemic.BirthDeathSamplingOccurrence`.
- Implement a visualisation of the birth-death-sampling LTT in `vis/birthDeathSamplingVisualisation.R`.
- Implement a birth-death-sampling model in `Epidemic.BirthDeathSampling`.

## 0.1.2.0

- Implement a visualisation of the infection network in `vis/infection-tree.vg.json`.
- Implement `Epidemic.Utility.eventsAsJsonTree`.
- Move the `Epidemic.Simulation` module to `Epidemic.Utility`.

## 0.1.1.0

- Write the output to CSV using `cassava`

## 0.1.0.0

- Basic prototype to simulate a constant rate birth-death process.
