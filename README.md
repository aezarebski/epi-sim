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
4. Birth-Death-Sampling-Catastrophe-Occurrence (see `Epidemic.BirthDeathSamplingCatastropheOccurrence`)

## Output

The output is a CSV with a header encoding which events occurred when and to whom: `event,time,primaryPerson,secondaryPerson`. The *primary person* is either the infecting person or the person who has been removed in some manner, the *secondary person* is the person who was infected, or this is a missing value. There are functions to assist in extracting observations from a full simulation: `birthDeathSamplingOccurrenceObservedEvents`. In the case of a catastrophe event where multiple individuals may be removed, they are represented as a colon separated list of identifiers in the `primaryPerson` field.
