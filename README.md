# epi-sim

A library for simulating stochastic epidemic models, with a focus on
phylodynamics and observation models.

## Epidemic events

A realisation of the model is represented by a list of `EpidemicEvent`s which
describe the events that occurred.

## Available models

Although this package supports the definition of new models there are some that
are implemented already in the `Epidemic.Model` module. Implemented models
include:

1. Birth-Death-Sampling-Catastrophe-Occurrence-Disaster (see `BDSCOD.hs`)
2. Inhomogeneous Birth-Death-Sampling (see `InhomogeneousBDS.hs`)
3. Inhomogeneous Birth-Death-Sampling-Catastrophe-Occurrence-Disaster (see
   `InhomogeneousBDSCOD.hs`)
4. Logistic Birth-Death-Sampling-Disaster (see `LogisticBDSD.hs`)
