# Changelog for epi-sim

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
