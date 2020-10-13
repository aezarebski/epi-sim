# epi-sim
A tool for simulating epidemics, with a focus on phylodynamics and observation
models.

## Available models

1. Birth-Death (see `Epidemic.BirthDeath`)
2. Birth-Death-Sampling (see `Epidemic.BirthDeathSampling`)
3. Birth-Death-Sampling-Occurrence (see `Epidemic.BirthDeathSamplingOccurrence`)
4. Birth-Death-Sampling-Catastrophe-Occurrence (see `Epidemic.BirthDeathSamplingCatastropheOccurrence`)
5. Birth-Death-Sampling-Catastrophe-Occurrence-Disaster (see `Epidemic.BDSCOD`)
6. Inhomogeneous Birth-Death (see `Epidemic.InhomogeneousBD`)
7. Inhomogeneous Birth-Death-Sampling (see `Epidemic.InhomogeneousBDS`)

## Output

The output is a CSV with a header encoding which events occurred when and to
whom: `event,time,primaryPerson,secondaryPerson`. The *primary person* is either
the infecting person or the person who has been removed in some manner, the
*secondary person* is the person who was infected, or this is a missing value.
There are functions to assist in extracting observations from a full simulation:
`birthDeathSamplingOccurrenceObservedEvents`. In the case of a catastrophe event
where multiple individuals may be removed, they are represented as a colon
separated list of identifiers in the `primaryPerson` field.

## Development

The desired workflow is to keep a `dev` branch for development with `master`
reserved for code that could be used by others. At the time of writing this is
not the case but will hopefully be so soon. Prior to moving material from `dev`
into `master` we will try to use a `release` branch.
