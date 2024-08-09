
## Introduction

This repository hosts the code underlying the research “Evaluating the
Impact of Misspecified Spatial Neighboring Structures in Bayesian CAR
Models.”, an article by Ernest Somua-Wiafe, Richard Minkah, Kwabena
Doku-Amponsah, Louis Asiedu, Edward Acheampong, and Samuel Iddi.

Below is a description of the various R scripts for execution and other
information which serves as a guide for users. All codes were executed
using R version 4.2.3.

The following packages may be installed for smooth program execution.

# Install required packages

``` r
# This chunk is used for package installation and loading
packages <- c("sf", "sp", "spdep", "INLA", "patchwork", "ggplot2", "reshape2", "dplyr", "cowplot")

install_if_missing <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package, repos = 'https://cloud.r-project.org', dependencies = TRUE)
  }
}

update_if_necessary <- function(package, min_version = NULL) {
  installed_version <- tryCatch(
    packageVersion(package),
    error = function(e) NULL
  )
  if (is.null(installed_version) || (!is.null(min_version) && installed_version < min_version)) {
    install.packages(package, repos = 'https://cloud.r-project.org', dependencies = TRUE)
  }
}

update_if_necessary("rlang", "1.1.0")
invisible(lapply(packages, install_if_missing))
lapply(packages, library, character.only = TRUE)
```

# Script Name: \[Pre-simulation Codes.R\]

## Description

This script generates the spatial neighborhood graphs used for the
simulation exercise and sets the parameter values while generating the
independent variables
![X_1 \sim \mathcal{N}(1,10)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;X_1%20%5Csim%20%5Cmathcal%7BN%7D%281%2C10%29 "X_1 \sim \mathcal{N}(1,10)")
and
![X_2 \sim \log(\mathcal{U}\_{\[1,3\]})](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;X_2%20%5Csim%20%5Clog%28%5Cmathcal%7BU%7D_%7B%5B1%2C3%5D%7D%29 "X_2 \sim \log(\mathcal{U}_{[1,3]})").
The purpose is to set apart a `Param.RData` file that stores the
different parameter values and data used during the simulation exercise.

## Usage

To run the R script, the shapefile of Ghana’s 261 districts is required.
From this shapefile, three neighborhood structures are formulated for
the study:

    W1: The first order Queen's contiguity.
    W2: Neighbors falling within the interval (0,70 km].
    W3: Neighbors falling within the interval (0,140 km].

# Script Name: \[BYM2 simulation.R\]

## Description

This script also utilizes the data stored in `Param.RData` for the
simulations. It contains step-by-step comprehensive code that loads and
systematically selects the correct parameter combination and
neighborhood structure for the simulation exercise. Once the spatial
data is sampled from the BYM2 model, we ensure it passes the Moran’s I
test while controlling for extreme observations before proceeding to run
the BYM2 model for each of the neighborhood structures.

## Usage

implementation was achieved through the `R-INLA` package and the
`inla.scale.model()` function was used to scale the structured
component. To help control the total run time, we divided the code to
handle the various combinations separately. `j`: (0 \< integer \< 13)
specifies the particular combination to consider, while `n.sim`:
(integer \> 1) denotes the number of simulations and sets the number of
times the process is repeated. For each dataset generated under the 12
different parameter value combinations, the data is stored in an
`.RData` file. Model

# Script Name: \[Post-simulation.R\]

## Description

This script further extracts the simulated data stored in the `.RData`
files and runs the codes for analysis.

## Usage

The plots were primaritly generated using the
![\textbf{ggplot2}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Ctextbf%7Bggplot2%7D "\textbf{ggplot2}")
package.
