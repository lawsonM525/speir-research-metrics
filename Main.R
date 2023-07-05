# LOADING FUNCTION FILES
source('partial-distributions.R')
source('separate-j-points.R')
source('triple-distributions.R')
source('new-gains.R')
source('boxplots.R')
source('boxplots-tiers.R')
source('averages.R')

# LOADING DATA

## Overall Grade files
s1_grades <- read.csv("csv-files/S23_Section_1_Grades.csv")
s2_grades <- read.csv("csv-files/S23_Section_2_Grades.csv")
## Detailed Partial Test Files
s1p1 <-  read.csv("csv-files/CSC110-S23-Sec01-partial1-Anon.csv_clean.csv")
s1p2 <-  read.csv("csv-files/CSC110-S23-Sec01-partial2-Anon.csv_clean.csv")
s1p3 <-  read.csv("csv-files/CSC110-S23-Sec01-partial3-Anon.csv_clean.csv")
s2p1 <-  read.csv("csv-files/CSC110-S23-Sec02-partial1-Anon.csv_clean.csv")
s2p2 <-  read.csv("csv-files/CSC110-S23-Sec02-partial2-Anon.csv_clean.csv")
s2p3 <-  read.csv("csv-files/CSC110-S23-Sec02-partial3-Anon.csv_clean.csv")


# BASIC DENSITY PLOTS
# distributions(s1_grades, "Section_1_JMCQ", s1_grades$P1.After.Comments, 
#               s1_grades$P2.After.Comments, s1_grades$P3.After.Comments, 
#               s1_grades$Number.Grade)
# distributions(s2_grades, "Section_2_MCQ", s2_grades$P1.After.Comments, 
#               s2_grades$P2.After.Comments, s2_grades$P3.After.Comments, 
#               s2_grades$Number.Grade)

# Separating mcq points from justification points
s1p1 <- separate(s1p1,'P1')
write.csv(s1p1, file='csv-files/S1P1_Detailed.csv')
s1p2 <- separate(s1p2, 'P2')
write.csv(s1p2, file='csv-files/S1P2_Detailed.csv')
s1p3 <- separate(s1p3, 'P3')
write.csv(s1p3, file='csv-files/S1P3_Detailed.csv')
s2p1 <- separate(s2p1, 'P1')
write.csv(s2p1, file='csv-files/S2P1_Detailed.csv')
s2p2 <- separate(s2p2, 'P2')
write.csv(s2p2, file='csv-files/S2P2_Detailed.csv')
s2p3 <- separate(s2p3, 'P3')
write.csv(s2p3, file='csv-files/S2P3_Detailed.csv')

##Plotting combined plots for MCQ only and justification only points
# triple_plot(s1p1, s1p2, s1p3, 'Section 1')
# triple_plot(s2p1, s2p2, s2p3, 'Section 2')

## Calculating bonuses
s1_gains <- calc_gains(s1_grades,'jmcq')
s2_gains <- calc_gains(s2_grades, 'mcq')

boxplot_df(s1_gains, "s1_gains")
boxplot_df(s2_gains, "s2_gains")

compute_stats(s1_gains, c('p1.gain','p2.gain'), 's1_mcq_gains')
compute_stats(s2_gains, c('p1.gain','p2.gain'), 's2_jmcq_gains')


# Plotting boxplots for partials and mcq points and justification points only
boxplot_partials(s1_grades, s2_grades, "S1-JMCQ", "S2-MCQ")
boxplot_partials_tiers(s1_grades, s2_grades, "S1-JMCQ", "S2-MCQ")
boxplot_partials_tiers_0_100(s1_grades, s2_grades, "S1-JMCQ", "S2-MCQ")

compute_stats(s1_grades, 
              c('P1.After.Comments','P2.After.Comments','P3.After.Comments',
                'Number.Grade'),
              's1_jmcq')
compute_stats(s2_grades, 
              c('P1.After.Comments','P2.After.Comments','P3.After.Comments',
                'Number.Grade'),
              's2_mcq')
