# epi-sim
A tool for simulating epidemics, with a focus on phylodynamics and observation models.

```
stack clean
stack build
stack run
stack test
stack haddock --open --no-haddock-deps
```

## Models

1. Birth-Death (see `Epidemic.BirthDeath`)
2. Birth-Death-Sampling (see `Epidemic.BirthDeathSampling`)
3. Birth-Death-Sampling-Occurrence (see `Epidemic.BirthDeathSamplingOccurrence`)

## Output

The output is a CSV with a header encoding which events occurred when and to whom: `event,time,primaryPerson,secondaryPerson`. The *primary person* is either the infecting person or the person who has been removed in some manner, the *secondary person* is the person who was infected, or this is a missing value. There are functions to assist in extracting observations from a full simulation: `birthDeathSamplingOccurrenceObservedEvents`.
