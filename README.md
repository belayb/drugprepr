# R package drugprepr

Prepare prescription data (such as from the [Clinical Practice Research Datalink](https://www.cprd.com/)) into an analysis-ready format, with start and stop dates for each patient's prescriptions. Based on [Pye et al (2018)](https://doi.org/10.1002/pds.4440).

The package **drugprepr** is designed to help improve transparency and efficiency in pharmaco-epidemiology studies that depend on electronic prescribing records.
By pre-processing data in a reproducible way, it is easier to evaluate the sensitivity of analyses to different decisions, such as how to deal with overlapping prescriptions or missing data.

Much of the package depends on a workhorse function, `impute()`, which replaces elements that match some predicate (indicating implausiblity or missingness) with a summary of a grouping level (for instance, the mean of values from the same GP practice).
You can use this function in your own analyses.

The algorithm of [Pye et al (2018)](https://doi.org/10.1002/pds.4440) is implemented in the function `drug_prep()`, which performs the following steps.

1. Impute implausible prescription quantities
2. Impute missing prescription quantities
3. Impute implausible daily doses
4. Impute missing daily doses
5. Estimate prescription durations
6. Remove or truncate implausibly-long prescription durations
7. Impute missing prescription durations
8. Disambiguate prescriptions with the same start date
9. Handle overlapping prescription periods
10. Close small gaps between successive prescriptions

The user is welcome to define their own algorithm with different steps or a different ordering; see the source code on GitHub to see how.

Output is a data frame containing prescription start and stop dates for each patient, practice and drug.
This may be fed into a statistical model to estimate the effect of drug exposure on events, for example exposure to opioids and incidence of fractures.

**drugprepr** can also be used in combination with the package [**doseminer**](https://github.com/Selbosh/doseminer) to extract structured information from freetext prescription instructions.

## Installation

You can install **drugprepr** from CRAN:

```r
install.packages('drugprepr')
```

Or get the latest development version from GitHub:

```r
remotes::install_github('belayb/drugprepr')
```

## Usage

The main function to use is `drug_prep()`.
See the package vignettes and examples in the documentation for more details.

## Contributors

Maintained by Belay Birlie Yimer and David Selby of the [Centre for Epidemiology Versus Arthritis](https://www.cfe.manchester.ac.uk/), University of Manchester, UK.
Other co-authors: Meghna Jani, Goran Nenadic, Mark Lunt, William G. Dixon.

Pull requests and GitHub issues are welcome.
