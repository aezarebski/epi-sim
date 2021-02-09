# epi-sim
A library for simulating epidemics, with a focus on phylodynamics and
observation models.

## Available models

Although this package supports the definition of new models there are some that
are implemented already in the `Epidemic.Model` module. Implemented models
include:

1. Birth-Death-Sampling (see `Epidemic.Model.BirthDeathSampling`)
2. Birth-Death-Sampling-Catastrophe-Occurrence-Disaster (see `Epidemic.Model.BDSCOD`)
3. Inhomogeneous Birth-Death-Sampling (see `Epidemic.Model.InhomogeneousBDS`)
4. Logistic Birth-Death-Sampling-Disaster (see `Epidemic.Model.LogisticBDSD`)
