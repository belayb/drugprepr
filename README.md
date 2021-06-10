# An R package drugprepCPRD
Belay B. Yimer, David Selby, Meghna Jani, Goran Nenadic, Mark Lunt, William G. Dixon

An algorithm for the transparent and efficient preparation of CPRD drug data into information on individuals’ drug use over time. 
The goal of `drugprepCPRD` package is to allow users to create multiverse analyses in a concise and easily interpretable manner. The `drugprepCPRD` package allows reserchers to specify sets of defensible data processing options at each decison node (e.g., different ways of imputing missing quantity and ndd, 
different ways of handling multiple prescriptions), implement them all, and then report the outcomes of all analyses resulting from all possible choice combinations. 
The package depends on the R-package `doseminer` for extracting drug dosage information from CPRD prescription data.

# Installation
You can install the latest development version from `GitHub` with these `R` commands:

```
install.packages("devtools")
devtools::install_github("belayb/drugprepCPRD")
```

# Contributors
Maintained by Belay Birlie Yimer (belaybirlie.yimer@manchester.ac.uk), David Selby (david.selby@manchester.ac.uk), ...
