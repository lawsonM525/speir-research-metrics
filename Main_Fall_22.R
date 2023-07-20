source("plotting.R")
source("f22_tier_scores.R")
source("boxplots-tiers.R")
source("averages.R")
source("f22-separate-j-points.R")

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
compute_stats(jmcq, "P1.Score", "JMCQ Stats")
compute_stats(mcq, "P1.Score", "MCQ Stats")
compute_stats_vec(list(jmcq$P1.Score, mcq$P1.Score),c("jmcq_p1","mcq_p1"),"f22_p1")

# Boxplots
boxplot_single(jmcq, mcq, "JMCQ", "MCQ")

# Tiers
jmcq$tier <- assign_tiers(jmcq$P1.Total)
mcq$tier <- assign_tiers(mcq$P1.Total)

# Tier stats
# jmcq_tiers <- tiers_scores(jmcq)
# mcq_tiers <- tiers_scores(mcq)
# 
# compute_stats(jmcq_tiers$tierA, "score", "JMCQ Tier A")
# compute_stats(mcq_tiers$tierA, "score", "MCQ Tier A")
# 
# compute_stats(jmcq_tiers$tierB, "score", "JMCQ Tier B")
# compute_stats(mcq_tiers$tierB, "score", "MCQ Tier B")
# 
# compute_stats(jmcq_tiers$tierC, "score", "JMCQ Tier C")
# compute_stats(mcq_tiers$tierC, "score", "MCQ Tier C Stats")

# Table of tier counts
jmcq$section <- "JMCQ" 
mcq$section <- "MCQ"

# Bind into one data frame 
df <- rbind(jmcq[c("key","P1.Score","tier","section")], 
            mcq[c("key","P1.Score","tier","section")])

# Create table 
tier_table <- table(df$section, df$tier)

# View table
tier_table  

write.csv(as.data.frame(tier_table), "f22_tiers.csv")

f22_jmcq <- separate_f22(jmcq)
compute_stats(f22_jmcq, "j_gain", "f22-j-gain")
# compute_stats(f22_jmcq,"j-cons", "f22-j-cons")


# Create plot dataframe
plot_df <- data.frame(
  gain = f22_jmcq$j_gain,
  grp = "JMCQ"  
)

# Make plot
p <- ggplot(plot_df, aes(x=grp, y=gain, color=grp)) +
  geom_boxplot() +
  geom_jitter(width=0.2, alpha=0.4) +
  scale_color_manual(values="blue") + 
  labs(title="JMCQ Justification Gain",
       x="Section",
       y="Gain (%)")

# Save plot
ggsave("plots/f22_jmcq_gain_boxplot.png", p, width=5, height=5)