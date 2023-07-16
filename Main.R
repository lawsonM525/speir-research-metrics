# LOADING FUNCTION FILES
source('partial-distributions.R')
source('separate-j-points.R')
source('triple-distributions.R')
source('new-gains.R')
source('boxplots.R')
source('boxplots-tiers.R')
source('averages.R')
library(RColorBrewer)
library(dplyr)
library(tidyverse)

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


results <- separate_partials(s1p3, s2p3)

# Access the dataframes
s1p3a <- results$s1p3a
s1p3b <- results$s1p3b
s2p3a <- results$s2p3a
s2p3b <- results$s2p3b

s1p3a <- calc_tot_pts(s1p3a)
s1p3b <- calc_tot_pts(s1p3b)
s2p3a <- calc_tot_pts(s2p3a)
s2p3b <- calc_tot_pts(s2p3b)

## Scaling points to percent
s1p3a$score <- (s1p3a$total_points/40)*100
s1p3b$score <- (s1p3a$total_points/20)*100
s2p3a$score <- (s1p3a$total_points/20)*100
s2p3b$score <- (s1p3a$total_points/10)*100



# BASIC DENSITY PLOTS
# distributions(s1_grades, "Section_1_JMCQ", s1_grades$P1.After.Comments, 
#               s1_grades$P2.After.Comments, s1_grades$P3.After.Comments, 
#               s1_grades$Number.Grade)
# distributions(s2_grades, "Section_2_MCQ", s2_grades$P1.After.Comments, 
#               s2_grades$P2.After.Comments, s2_grades$P3.After.Comments, 
#               s2_grades$Number.Grade)

# Separating mcq points from justification points
s1p1 <- separate_comments(s1_grades, s1p1,'P1', 8)
write.csv(s1p1, file='csv-files/S1P1_Detailed.csv')
s1p2 <- separate_comments(s1_grades, s1p2, 'P2', 8)
write.csv(s1p2, file='csv-files/S1P2_Detailed.csv')
s1p3 <- separate_comments(s1_grades, s1p3, 'P3', 6)
write.csv(s1p3, file='csv-files/S1P3_Detailed.csv')
s2p1 <- separate(s2p1, 'P1', 8)
write.csv(s2p1, file='csv-files/S2P1_Detailed.csv')
s2p2 <- separate(s2p2, 'P2', 8)
write.csv(s2p2, file='csv-files/S2P2_Detailed.csv')
s2p3 <- separate(s2p3, 'P3', 6)
write.csv(s2p3, file='csv-files/S2P3_Detailed.csv')

##Plotting combined plots for MCQ only and justification only points
triple_plot(s1p1, s1p2, s1p3, 'Section 1')
triple_plot(s2p1, s2p2, s2p3, 'Section 2')

## Calculating bonuses
s1_gains <- calc_gains(s1_grades,'jmcq')
s2_gains <- calc_gains(s2_grades, 'mcq')

boxplot_df(s1_gains, "s1_gains")
boxplot_df(s2_gains, "s2_gains")

compute_stats(s1_gains, c('p1.gain','p2.gain'), 's1_mcq_gains')
compute_stats(s2_gains, c('p1.gain','p2.gain'), 's2_jmcq_gains')

t_test_result <- t.test(s1_gains$p2.gain, s2_gains$p2.gain)
print(t_test_result)

w_test_result <- wilcox.test(s1_gains$p1.gain, s2_gains$p1.gain, exact = FALSE)
print(w_test_result)



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

# Final Grades comparison
compute_stats_vec(list(s1_grades$Number.Grade, s2_grades$Number.Grade), 
                  c("jmcq-final-grades","mcq-final-grades"),"s23-final-grades")




### Pi Grade Comparisons
# Overall
compute_stats_vec(list(s1_grades$P1.After.Comments, s2_grades$P1.After.Comments), 
                  c("jmcq-p1","mcq-p1"),"s23-p1-compare")
compute_stats_vec(list(s1_grades$P2.After.Comments, s2_grades$P2.After.Comments), 
                  c("jmcq-p2","mcq-p2"),"s23-p2-compare")
compute_stats_vec(list(s1p3a$score,s2p3a$score), 
                  c("jmcq-p3a","mcq-p3a"),"s23-p3a-compare")
compute_stats_vec(list(s1p3b$score,s2p3b$score), 
                  c("jmcq-p3b","mcq-p3b"),"s23-p3b-compare")

## Plotting P3b boxplots
# Create a data frame for ggplot
df <- data.frame(
  points = c(s1p3b$score, s2p3b$score),
  group = factor(c(rep('jmcq p3b', length(s1p3b$score)), 
                   rep('mcq p3b', length(s2p3b$score))),
                 levels = c('jmcq p3b', 'mcq p3b'))
)

# Define colorblind-friendly colors
cbPalette <- brewer.pal(8, "Dark2")

# Make the plot
p <- ggplot(df, aes(x = group, y = points, fill = group)) +
  geom_boxplot() +
  scale_fill_manual(values = cbPalette[c(1, 2)]) +
  labs(x = "Group", y = "Score", fill = "Group") +
  theme_minimal()

# Save the plot
ggsave("plots/p3b-boxplot.png", p, width = 6, height = 4)


## SPlitting partials into tiers
s1p1$tier <- assign_tiers(s1p1$SC0)
s1p2$tier <- assign_tiers(s1p2$SC0)
s1p3a$tier <- assign_tiers(s1p3a$score)
s1p3b$tier <- assign_tiers(s1p3b$score)
s2p1$tier <- assign_tiers(s2p1$SC0)
s2p2$tier <- assign_tiers(s2p2$SC0)
s2p3a$tier <- assign_tiers(s2p3a$score)
s2p3b$tier <- assign_tiers(s2p3b$score)


# Combine the data frames
combined <- bind_rows(
  mutate(s1p1, period = "P1"),
  mutate(s1p2, period = "P2"),
  mutate(s1p3a, period = "P3a"),
  mutate(s1p3b, period = "P3b"),
  mutate(s2p1, period = "P1"),
  mutate(s2p2, period = "P2"),
  mutate(s2p3a, period = "P3a"),
  mutate(s2p3b, period = "P3b")
)

# Create tier summary table
summary_table <- combined %>% 
  group_by(period, tier) %>%
  summarise(n = n(), .groups = 'drop') %>%
  pivot_wider(names_from = period, values_from = n, values_fill = 0) %>%
  arrange(desc(tier))

write.csv(summary_table, file='stats/tiers_summary.csv')
