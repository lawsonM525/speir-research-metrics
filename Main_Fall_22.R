source("plotting.R")
source("tier_scores.R")
source("boxplots-tiers.R")
source("averages.R")

# Load section data
s1 <- read.csv("csv-files/F22-P1-summary - section01-P1.csv")  
s2 <- read.csv("csv-files/F22-P1-summary - section02-P1.csv")
s3 <- read.csv("csv-files/F22-P1-summary - section03-P1.csv")

# Calculate totals
s1$P1.Total <- s1$P1a.Total + s1$P1b.Total
s2$P1.Total <- s2$P1a.Total + s2$P1b.Total
s3$P1.Total <- s3$P1a.Total + s3$P1b.Total
jmcq <- rbind(s2,s3)
jmcq$P1.Score <- (jmcq$P1.Total/120)*100
mcq <- s1
mcq$P1.Score <- (mcq$P1.Total/120)*100

# Density plots
densities_single(jmcq, "JMCQ")
densities_single(mcq, "MCQ")

# Summary stats 
compute_stats(jmcq, "P1.Total", "JMCQ Stats")
compute_stats(mcq, "P1.Total", "MCQ Stats")
compute_stats_vec(list(jmcq$P1.Total, mcq$P1.Total),c("jmcq_p1","mcq_p1"),"f22_p1")

# Boxplots
boxplot_single(jmcq, mcq, "JMCQ", "MCQ") 

# Tiers
jmcq$tier <- assign_tiers(jmcq$P1.Total)
mcq$tier <- assign_tiers(mcq$P1.Total)

# Tier stats
jmcq_tiers <- tiers_scores(jmcq)
mcq_tiers <- tiers_scores(mcq)

compute_stats(jmcq_tiers$tierA, "score", "JMCQ Tier A Stats")
compute_stats(mcq_tiers$tierA, "score", "MCQ Tier A Stats")

compute_stats(jmcq_tiers$tierB, "score", "JMCQ Tier B Stats")
compute_stats(mcq_tiers$tierB, "score", "MCQ Tier B Stats")

compute_stats(jmcq_tiers$tierC, "score", "JMCQ Tier C Stats")
compute_stats(mcq_tiers$tierC, "score", "MCQ Tier C Stats")

# Table of tier counts
tier_table <- table(c(jmcq$tier, mcq$tier))
write.csv(as.data.frame(tier_table), "f22_tiers.csv")