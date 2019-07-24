# Psychometrics & Post-Data Analysis for Jamovi

This module is a tool for performing various Psychometric & Post-Data Analyses.

The current tools available are:

- *binaryDIF* (Differential Item Functioning): 

   Assessing DIF on dichotomously scored items using a generalized linear model framework (GLIM). By incorporating the GLIM family of methods for detecting DIF in a user-friendly interface, the technical barrier to assessing DIF has been greatly reduced. The development of techniques for assessing of Type-M error rates (Gelman & Carlin, 2014) on DIF-classification thresholds represents a revolution in the interpretability of DIF-flagging, particularly in the context of low-powered research settings. The goal developing this module is to encourage the use of the the GLIM framework in the broader psychosocial measure validation praxis.

- *TestROC* (Measure Diagnostics)
    The TestROC module provides an interface to the cutpointR R-package. This module enables researchers in psychosocial validation work to assess the optimal cutpoints of measures. _Additional info to come_

To be added:
- Design Analysis for:
	- T-Test for Mean differences
	- T-Test for Correlations
	- Chi^2 Test Type-S/Type-M

- Everything else

<img src="docs/i1.png" class="img-responsive" alt="">


# Installation

Currently only available from source.

## From source

You will first need to download [jamovi](https://www.jamovi.org/download.html). 

Next either:

For the current stable version download the correct file from your operating system in the 'versions' folder.

or

For the most up-to-date version download the 'psychoPDA.jmo' file.

Once the file is downloaded, use jamovi's sideload function to install the module.


Alternatively, You can clone this repository and compile the module within R with `jmvtools`.

```
install.packages('jmvtools', repos=c('https://repo.jamovi.org', 'https://cran.r-project.org'))

setwd(directory/of/cloned/repo)

jmvtools::install()

```

# Troubleshooting

## While using the module

See https://lucasjfriesen.github.io/jamoviPsychoPDA_docs/


## Installation

If the jamovi crashes shortly after or intermittently after installation, you may have downloaded the wrong OS version. Ensure you downloaded the correct file from the 'versions' folder.
