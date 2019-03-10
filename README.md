# Psychometric Post-Data Analysis for Jamovi

This module is a tool for performing Psychometric Post-Data Analysis.

The current tools available are:

- *Binary Differential Item Functioning* (D.DIF): 

  Assessing DIF on dichotomously scored items using a generalized linear model framework (GLIM). By incorporating the GLIM family of methods for detecting DIF in a user-friendly interface, the technical barrier to assessing DIF has been greatly reduced. The development of techniques for assessing of Type-M error rates (Gelman & Carlin, 2014) on DIF-classification thresholds represents a revolution in the interpretability of DIF-flagging, particularly in the context of low-powered research settings. The goal developing this module is to encourage the use of the the GLIM framework in the broader psychosocial measure validation praxis.

- *T-Test for Mean differences*:

Implements Type-S, Type-M error rate calculations, and sensitivity analysis for T-Tests of mean differences.

- *T-Test for Correlations*:

Implements Type-S, Type-M error rate calculations, and sensitivity analysis for T-Tests of Pearson correlations.

To be added:

- Chi^2 Test Type-S/Type-M
- Everything else

<img src="docs/i1.png" class="img-responsive" alt="">


# Installation

Currently only available from source.

## From source

You will first need to download [jamovi](https://www.jamovi.org/download.html). 

Next either:

Download the 'DIF.jmo' file and use the sideload function in jamovi, 

or

You can clone this repository and compile the module within R with 

```
library(jmvtools)

jmvtools::install()

```

# Troubleshooting

## While using the module

TBD


## Installation

TBD
