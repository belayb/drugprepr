# An R package CPRDDrugPrep
Belay Birlie Yimer, David Selby, ...

An algorithm for the transparent and efficient preparation of CPRD drug data into information on individuals’ drug use over time. 
The goal of `CPRDDrugPrep` package is to allow users to create multiverse analyses in a concise and easily interpretable manner. The `CPRDDrugPrep` package allows reserchers to specify sets of defensible data processing options at each decison node (e.g., different ways of imputing missing quantity and ndd, 
different ways of handling multiple prescriptions), implement them all, and then report the outcomes of all analyses resulting from all possible choice combinations. 
The package depends on the R-package `doseminer` for extracting drug dosage information from CPRD prescription data.

# Installation
You can install the latest development version from `GitHub` with these `R` commands:

```
install.packages("devtools")
devtools::install_github("belayb/CPRDDrugPrep")
```
# Examples
## Data 
DrugPrep has been developed to process prescriptions data from the Clinical Practice Research Datalink (CPRD).  You will need a dataset containing the following variables for the drug types (prodcodes) you are interested in:  

Variable description	| Name in script | Name on CPRD |  Where located in CPRD
-----|-----------------|-----------|--------------
Patient identifier | patid | patid | Therapy file
Product identifier | prodcode | prodcode | Therapy file
Start date of prescription	| start | start |Therapy file
Quantity 	| qty | qty | Therapy file
Numeric daily dose | ndd | ndd |	Therapy file or from result of doseminer call
Number of days of treatment prescribed	| numdays | numdays | Therapy file
Dose duration	| dose_duration | dose_duration | common_dosages file
Maximum and minimum length of prescriptions	| | NA | Not in CPRD: self-defined
Maximum and minimum numeric daily dose	|  max_ndd, min_ndd | NA | Not in CPRD: self-defined
Maximum and minimum quantity	|  max_qty, min_qty | NA | Not in CPRD: self-defined

## Call-to-doseminer and ndd commputation 

## Immplusible values 

## Running universe data creation 

## Running universe data analysis 


## Running multiverse data creation 

## Running multiverse analysis 


# Contributors
Maintained by Belay Birlie Yimer (belaybirlie.yimer@manchester.ac.uk), David Selby (david.selby@manchester.ac.uk), ...
