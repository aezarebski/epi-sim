# epi-sim

A library for simulating epidemics, with a focus on phylodynamics and
observation models.

Although this package supports the definition of new models there are some that
are implemented already in the `Epidemic.Model` module. Implemented models
include:

1. Birth-Death-Sampling-Catastrophe-Occurrence-Disaster (see `Epidemic.Model.BDSCOD`)
2. Inhomogeneous BDSCOD (see `Epidemic.Model.InhomogeneousBDSCOD`)
3. Logistic Birth-Death-Sampling-Disaster (see `Epidemic.Model.LogisticBDSD`)

There are more details in the documentation of the "Epidemic" module.
