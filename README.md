# MEQ 2026
This is a public repository for the 2026 manuscript that is an extension and validation of the Math Experience Questionnaire (MEQ), first introduced by O'Leary, Hallett, &amp; Fitzpatrick (2017).

## Organization

This repository contains the anonymized data from both studies of the manuscript, as well as R scripts that reproduce the analyses that were done in the paper.  The R scripts are designed to run on these anonymized datasets.

## Files

There are a group of files for Study 1 and a group of files for Study 2.  The Study 1 Files are as follows:

### Study 1

|                                  |         |
|:---------------------------------|:--------|
| Study 1 Data Analyses.R        | The R script that contains the analyses describes in Study 1.  It imports the data from Study 1 Data.rds|
| Study 1 Data Coding.R          | The R script that imports the raw data (Study 1 Raw Data.csv), defines subscales, and removes participants for failed any of the attention checks. |
| Study 1 Data.rds               | This is the cleaned data created by the Study 1 Data Coding.R script.  It is in the rds format to preserve factoring. |
| Study 1 Factors.xlsx           | This is an Excel spreadsheet that lists the loadings and uniquenesses from the 6-factor EFA solution in Study 1.  It is produced by Study 1 Data Analyses.R. |
| Study 1 MEQ-R Item Names.csv   | This lists the wording of the items of the MEQ.  It is used by Study 1 Data Analyses.R to create the Study 1 Factors.xlsx file. |
| Study 1 Raw Data.csv           | This is the anonymized raw data that includes all participants except 14 who chose not to have their data included in the study (labelled in this file as Research Observation).|

The Study 2 Files are as follows:

### Study 2

|                                  |         |
|:---------------------------------|:--------|
| Algebra Key.csv           | This is an CSV file that describes the correct answers for the Algebra questions. The variable CodeAnswer gives the answer in the latex code of the correct response while the variable Answer is the mulitple choice option that was correct (with possible values being between 1 and 6).  It is used by Study 2 Data Coding.R to calculate the total correct for the Algebra questions. |
| Fluency Key.csv           | This is an CSV file that describes the correct answers for the Fluency questions. The variable Answer gives the answer in to the simple calcuation while the variable QuestionText describes the question asked.  It is used by Study 2 Data Coding.R to calculate the total correct for the Fluency questions. |
| Fractions Key.csv           | This is an CSV file that describes the correct answers for the Fractions questions. The variable CodeAnswer gives the answer in the latex code of the correct response while the variable Answer is the mulitple choice option that was correct (with possible values being between 1 and 8).  It is used by Study 2 Data Coding.R to calculate the total correct for the Fraction questions. |
| Study 2 4-Factor EFA.xlsx           | This is an Excel spreadsheet that lists the loadings and uniquenesses from the 4-factor EFA solution in Study 2.  It is produced by Study 2 Data Analyses.R and represents the final solution used for the paper. |
| Study 2 5-Factor EFA.xlsx           | This is an Excel spreadsheet that lists the loadings and uniquenesses from the 5-factor EFA solution in Study 2.  It is produced by Study 2 Data Analyses.R and did not present a solution that improved upon the understanding provided in the 4-factor solution, so it was discarded in favour of the 4-factor solution. |
| Study 2 Data Analyses.R        | The R script that contains the analyses describes in Study 1.  It imports the data from Study 1 Data.rds|
| Study 2 Data Coding.R          | The R script that imports the raw data (Study 1 Raw Data.csv), defines subscales, and removes participants for failed any of the attention checks. |
| Study 2 Data.rds               | This is the cleaned data created by the Study 1 Data Coding.R script.  It is in the rds format to preserve factoring. |
| Study 1 Factors.xlsx           | This is an Excel spreadsheet that lists the loadings and uniquenesses from the 6 factor EFA solution in Study 1.  It is produced by Study 1 Data Analyses.R. |
| Study 2 MEQ-R Item Names.csv   | This lists the wording of the items of the MEQ.  It is used by Study 2 Data Analyses.R to create the two excel files for the 4- and 5- factor solutions (Study 2 4-Factor EFA.xlsx and Study 2 5-Factor EFA.xlsx) |
| Study 2 Raw Data.csv           | This is the anonymized raw data that includes all participants except 14 who chose not to have their data included in the study (labelled in this file as Research Observation).|

