# Psychometrics & Post-Data Analysis for Jamovi

This module is a tool for performing various Psychometric & Post-Data Analyses.

The current tools available are:

- *BinaryLogR* (Binary Logistic Regression for Differential Item Functioning): 

   Assessing DIF on dichotomously scored items using a generalized linear model framework (GLIM). By incorporating the GLIM family of methods for detecting DIF in a user-friendly interface, the technical barrier to assessing DIF has been greatly reduced. The development of techniques for assessing of Type-M error rates (Gelman & Carlin, 2014) on DIF-classification thresholds represents a revolution in the interpretability of DIF-flagging, particularly in the context of low-powered research settings. The goal developing this module is to encourage the use of the the GLIM framework in the broader psychosocial measure validation praxis.

- *Test ROC* (Measure Diagnostics)
    The TestROC module provides an interface to the cutpointR R-package. This module enables researchers in psychosocial validation work to assess the optimal cutpoints of measures. _Additional info to come_

In testing:
- Ordinal logistic regression for DIF

- Design analysis for:
	- T-Test for Mean differences
	- T-Test for Correlations

- Reliability analysis for ordinal measures

<img src="docs/i1.png" class="img-responsive" alt="">


# Installation

Currently only available from source.

## From source

You will first need to download [jamovi](https://www.jamovi.org/download.html). 

Next, clone this repository and compile the module within R with `jmvtools`.

```
install.packages('jmvtools', repos=c('https://repo.jamovi.org', 'https://cran.r-project.org'))

setwd(directory/of/cloned/repo)

jmvtools::install()

```

# Troubleshooting

## While using the module

See https://lucasjfriesen.github.io/jamoviPsychoPDA_docs/


## Installation

N/A yet
