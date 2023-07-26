library("ggpubr")

s1p1qp <- read.csv("csv-files/questionable_percentage - Section1 (JMCQ)-P1.csv")
s1p2qp <- read.csv("csv-files/questionable_percentage - Section1 (JMCQ)-P2.csv")
s1p3aqp <- read.csv("csv-files/questionable_percentage - Section1 (JMCQ)-P3 (1 to 4).csv")
s1p3qp <- read.csv("csv-files/questionable_percentage - Section1 (JMCQ)-P3 (1 to 6).csv")

# Shorter column names for questionable_percentage
s1p1qp$qp <- s1p1qp$questionable_percentage
s1p2qp$qp <- s1p2qp$questionable_percentage
s1p3aqp$qp <- s1p3aqp$questionable_percentage
s1p3qp$qp <- s1p3qp$questionable_percentage

# Kendall Correlation tests
p1_qp_corr <- cor.test(s1p1qp$grade, s1p1qp$qp,  method="kendall")
p2_qp_corr <- cor.test(s1p2qp$grade, s1p2qp$qp,  method="kendall")
p3a_qp_corr <- cor.test(s1p3aqp$grade, s1p3aqp$qp,  method="kendall")
p3_qp_corr <- cor.test(s1p3qp$grade, s1p3qp$qp,  method="kendall")

# Visualizing correlations

p <- ggscatter(s1p1qp, x = "grade", y = "qp", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "kendall",
          xlab = "P1 Grade", ylab = "Questionable Percentage")

ggsave("plots/p1-qp-correlation.png", p, width = 6, height = 4)

p <- ggscatter(s1p2qp, x = "grade", y = "qp", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "kendall",
          xlab = "P2 Grade", ylab = "Questionable Percentage")

ggsave("plots/p2-qp-correlation.png", p, width = 6, height = 4)

p <- ggscatter(s1p3aqp, x = "grade", y = "qp", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "kendall",
          xlab = "P3a Grade", ylab = "Questionable Percentage")

ggsave("plots/p3a-qp-correlation.png", p, width = 6, height = 4)

p <- ggscatter(s1p3qp, x = "grade", y = "qp", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "kendall",
          xlab = "P3 Full Grade", ylab = "Questionable Percentage")

ggsave("plots/p3-qp-correlation.png", p, width = 6, height = 4)