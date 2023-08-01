# Speir Research Metrics Repo

This repository contains several scripts and datasets used for analyzing student performance data in our ongoing research project.

## Primary Files

- **Main.R**: This script conducts the analysis for Spring 23 data.
- **Main_surveys.R**: This script conducts the survey analysis
- **Main_Fall_22.R**: This script conducts the analysis for Fall 2022 data.
- **Main_Big_Table.R**: This script performs the analysis to create the big tables (big student table and big summary table).


## File and Directory Overview

1. **Useful Directories**:
   - `csv-files`: Contains original imported CSV files of our datasets.
   - `plots`: Contains various generated plots.
   - `survey-plots/s23`: Contains plots for the Spring 2023 survey.
   - `stats`: Contains summary statistics csv files used to compare vectors

2. **Analysis Function Scripts**:
   - `averages.R`: Made to compute various statistics for a dataframe
   - `boxplots-tiers.R`
   - `boxplots.R`
   - `separate-j-points.R`: Made to separate mcq and jsutification points for a given partial
    - J-gains for S23 are computedin here (not for the big student table though)
    - For big student table, j-gains are computed directly inside the big student table script
   - `f22-separate-j-points.R`
   - `f22_surveys.R`
   - `f22_tier_scores.R`
   - `new-gains.R`: Made to compute bonus gains
    - Only used in Spring 23 (in Big tables the function is directly in the main script)
   - `summary-tables.R`
   - `survey_data_cleaning.R`
   - `tier_scores.R`

3. **Miscellaneous**:
   - `README.md` (this document)

Feel free to message me with any questions.. my notifications are on now 😅
