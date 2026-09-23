MEQ 2026
This is a public repository for the 2026 manuscript that is an extension and validation of the Math Experience Questionnaire (MEQ), first introduced by O'Leary, Hallett, &amp; Fitzpatrick (2017).

## Organization

This repository contains the anonymized data from both studies of the manuscript, as well as R scripts that describe the analyses that we done in the paper.  The R scripts are designed to run on these anonymized datasets.

## Files

There are a group of files for Study 1 and a group of files for Study 2.  They are as follows:

|                                  |         |
|:---------------------------------|--------:|
| Study 1 Data Analyses.R        | The R script that contains the analyses describes in Study 1.  It imports the data from Study 1 Data.rds|
| Study 1 Data Coding.R          | The R script that imports the raw data (Study 1 Raw Data.csv), defines subscales, and removes participants for failed any of the attention checks. |
| Study 1 Data.rds               | This is the cleaned data created by the Study 1 Data Coding.R script.  It is in the rds format to preserve factoring. |
| Study 1 Factors.xlsx           | This is an Excel spreadsheet that lists the loadings and uniquenesses from the 6 factor EFA solution in Study 1.  It is produced by Study 1 Data Analyses.R. |
| Study 1 MEQ-R Item Names.csv   | This lists the wording of the items of the MEQ.  It is used by Study 1 Data Analyses.R to create the Study 1 Factors.xlsx file. |
| Study 1 Raw Data.csv           | This is the anonymized raw data that includes all participants except 14 who chose not to have their data included in the study (labelled in this file as Research Observation).|


