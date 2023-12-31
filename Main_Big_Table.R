# Author: Michelle Lawson
# Title: Aggregating Data to create big summary table
# Last Updated: July 26, 2023

# -----------------
# LOAD LIBRARIES
library(dplyr)
library(broom)

#------------------
# LOAD INTERNAL FILES
source("averages.R")

# -----------------
# LOAD DATA
s23s1p1 <- read.csv("csv-files/s23-cleaned - s1p1.csv")
s23s1p2 <- read.csv("csv-files/s23-cleaned - s1p2.csv")
s23s1p3 <- read.csv("csv-files/s23-cleaned - s1p3.csv")
s23s2p1 <- read.csv("csv-files/s23-cleaned - s2p1.csv")
s23s2p2 <- read.csv("csv-files/s23-cleaned - s2p2.csv")
s23s2p3 <- read.csv("csv-files/s23-cleaned - s2p3.csv")
f22s1p1 <- read.csv("csv-files/f22-cleaned - s1p1.csv")
f22s2p1 <- read.csv("csv-files/f22-cleaned - s2p1.csv")
f22s3p1 <- read.csv("csv-files/f22-cleaned - s3p1.csv")


# -----------------
# ASSIGN TIERS

# Add grade.tier columns
s23s1p1 <- s23s1p1 %>%
  mutate(grade.tier = cut(grade, breaks = quantile(grade, probs = 0:3 / 3), include.lowest = TRUE, labels = 3:1))
s23s1p2 <- s23s1p2 %>%
  mutate(grade.tier = cut(grade, breaks = quantile(grade, probs = 0:3 / 3), include.lowest = TRUE, labels = 3:1))
s23s1p3 <- s23s1p3 %>%
  mutate(grade.tier = cut(grade, breaks = quantile(grade, probs = 0:3 / 3), include.lowest = TRUE, labels = 3:1))
s23s2p1 <- s23s2p1 %>%
  mutate(grade.tier = cut(grade, breaks = quantile(grade, probs = 0:3 / 3), include.lowest = TRUE, labels = 3:1))
s23s2p2 <- s23s2p2 %>%
  mutate(grade.tier = cut(grade, breaks = quantile(grade, probs = 0:3 / 3), include.lowest = TRUE, labels = 3:1))
s23s2p3 <- s23s2p3 %>%
  mutate(grade.tier = cut(grade, breaks = quantile(grade, probs = 0:3 / 3), include.lowest = TRUE, labels = 3:1))
f22s1p1 <- f22s1p1 %>%
  mutate(grade.tier = cut(grade, breaks = quantile(grade, probs = 0:3 / 3), include.lowest = TRUE, labels = 3:1))
f22s2p1 <- f22s2p1 %>%
  mutate(grade.tier = cut(grade, breaks = quantile(grade, probs = 0:3 / 3), include.lowest = TRUE, labels = 3:1))
f22s3p1 <- f22s3p1 %>%
  mutate(grade.tier = cut(grade, breaks = quantile(grade, probs = 0:3 / 3), include.lowest = TRUE, labels = 3:1))

# Add j.effect.tier columns
s23s1p1 <- s23s1p1 %>%
  mutate(j.effect.tier = cut(j.effect, breaks = quantile(j.effect, probs = 0:3 / 3), include.lowest = TRUE, labels = 3:1))
s23s1p2 <- s23s1p2 %>%
  mutate(j.effect.tier = cut(j.effect, breaks = quantile(j.effect, probs = 0:3 / 3), include.lowest = TRUE, labels = 3:1))
s23s1p3 <- s23s1p3 %>%
  mutate(j.effect.tier = cut(j.effect, breaks = quantile(j.effect, probs = 0:3 / 3), include.lowest = TRUE, labels = 3:1))
s23s2p1 <- s23s2p1 %>%
  mutate(j.effect.tier = NA)
s23s2p2 <- s23s2p2 %>%
  mutate(j.effect.tier = NA)
s23s2p3 <- s23s2p3 %>%
  mutate(j.effect.tier = NA)
f22s1p1 <- f22s1p1 %>%
  mutate(j.effect.tier = NA)
f22s2p1 <- f22s2p1 %>%
  mutate(j.effect.tier = cut(j.effect, breaks = quantile(j.effect, probs = 0:3 / 3), include.lowest = TRUE, labels = 3:1))
f22s3p1 <- f22s3p1 %>%
  mutate(j.effect.tier = cut(j.effect, breaks = quantile(j.effect, probs = 0:3 / 3), include.lowest = TRUE, labels = 3:1))

# Add tier.avg columns
s23s1p1$tier.avg <- (as.double(as.character(s23s1p1$grade.tier)) + as.double(as.character(s23s1p1$j.effect.tier))) / 2
s23s1p2$tier.avg <- (as.double(as.character(s23s1p2$grade.tier)) + as.double(as.character(s23s1p2$j.effect.tier))) / 2
s23s1p3$tier.avg <- (as.double(as.character(s23s1p3$grade.tier)) + as.double(as.character(s23s1p3$j.effect.tier))) / 2
s23s2p1$tier.avg <- s23s2p1$grade.tier
s23s2p2$tier.avg <- s23s2p2$grade.tier
s23s2p3$tier.avg <- s23s2p3$grade.tier
f22s1p1$tier.avg <- f22s1p1$grade.tier
f22s2p1$tier.avg <- (as.double(as.character(f22s2p1$grade.tier)) + as.double(as.character(f22s2p1$j.effect.tier))) / 2
f22s3p1$tier.avg <- (as.double(as.character(f22s3p1$grade.tier)) + as.double(as.character(f22s3p1$j.effect.tier))) / 2

# -----------------
# MAKE BIG STUDENT TABLE

## Column cleaning
colnames(s23s1p3)[colnames(s23s1p3) == "J.Grade"] <- "j.grade"
colnames(s23s1p3)[colnames(s23s1p3) == "MCQ.Grade"] <- "mcq.grade"
s23s2p1$mcq.only <- s23s2p1$with.comments
s23s2p2$mcq.only <- s23s2p2$with.comments
s23s2p3$mcq.only <- s23s2p3$score_check
s23s2p1$mcq.grade <- s23s2p1$grade
s23s2p2$mcq.grade <- s23s2p2$grade
s23s2p3$mcq.grade <- s23s2p3$grade
f22s1p1$new.grade <- f22s1p1$grade
f22s2p1$new.grade <- f22s2p1$grade
f22s3p1$new.grade <- f22s3p1$grade


## Fill in j columns as NA for mcq.only sections
s23s2p1$j.only <- NA
s23s2p1$j.grade <- NA
s23s2p2$j.only <- NA
s23s2p2$j.grade <- NA
s23s2p3$j.only <- NA
s23s2p3$j.grade <- NA
f22s1p1$j.only <- NA
f22s1p1$j.grade <- NA

## Select Relevant Columns
s23s1p1 <- s23s1p1[, c("key", "mcq.only", "mcq.grade", "j.only", "j.grade", "grade", "new.grade", "grade.tier", "j.effect.tier", "tier.avg")]
s23s1p2 <- s23s1p2[, c("key", "mcq.only", "mcq.grade", "j.only", "j.grade", "grade", "new.grade", "grade.tier", "j.effect.tier", "tier.avg")]
s23s1p3 <- s23s1p3[, c("key", "mcq.only", "mcq.grade", "j.only", "j.grade", "grade", "new.grade", "grade.tier", "j.effect.tier", "tier.avg")]
s23s2p1 <- s23s2p1[, c("key", "mcq.only", "mcq.grade", "j.only", "j.grade", "grade", "new.grade", "grade.tier", "j.effect.tier", "tier.avg")]
s23s2p2 <- s23s2p2[, c("key", "mcq.only", "mcq.grade", "j.only", "j.grade", "grade", "new.grade", "grade.tier", "j.effect.tier", "tier.avg")]
s23s2p3 <- s23s2p3[, c("key", "mcq.only", "mcq.grade", "j.only", "j.grade", "grade", "new.grade", "grade.tier", "j.effect.tier", "tier.avg")]
f22s1p1 <- f22s1p1[, c("key", "mcq.only", "mcq.grade", "j.only", "j.grade", "grade", "new.grade", "grade.tier", "j.effect.tier", "tier.avg")]
f22s2p1 <- f22s2p1[, c("key", "mcq.only", "mcq.grade", "j.only", "j.grade", "grade", "new.grade", "grade.tier", "j.effect.tier", "tier.avg")]
f22s3p1 <- f22s3p1[, c("key", "mcq.only", "mcq.grade", "j.only", "j.grade", "grade", "new.grade", "grade.tier", "j.effect.tier", "tier.avg")]

## Add Meta Columns
s23s1p1$semester <- "S23"
s23s1p2$semester <- "S23"
s23s1p3$semester <- "S23"
s23s2p1$semester <- "S23"
s23s2p2$semester <- "S23"
s23s2p3$semester <- "S23"
f22s1p1$semester <- "F22"
f22s2p1$semester <- "F22"
f22s3p1$semester <- "F22"

s23s1p1$section <- 1
s23s1p2$section <- 1
s23s1p3$section <- 1
s23s2p1$section <- 2
s23s2p2$section <- 2
s23s2p3$section <- 2
f22s1p1$section <- 1
f22s2p1$section <- 2
f22s3p1$section <- 3

s23s1p1$partial <- 1
s23s1p2$partial <- 2
s23s1p3$partial <- 3
s23s2p1$partial <- 1
s23s2p2$partial <- 2
s23s2p3$partial <- 3
f22s1p1$partial <- 1
f22s2p1$partial <- 1
f22s3p1$partial <- 1

s23s1p1$treatment <- "jmcq"
s23s1p2$treatment <- "jmcq"
s23s1p3$treatment <- "jmcq"
s23s2p1$treatment <- "mcq"
s23s2p2$treatment <- "mcq"
s23s2p3$treatment <- "mcq"
f22s1p1$treatment <- "mcq"
f22s2p1$treatment <- "jmcq"
f22s3p1$treatment <- "jmcq"

s23s1p1$prof <- "P"
s23s1p2$prof <- "P"
s23s1p3$prof <- "P"
s23s2p1$prof <- "P"
s23s2p2$prof <- "P"
s23s2p3$prof <- "P"
f22s1p1$prof <- "R"
f22s2p1$prof <- "R"
f22s3p1$prof <- "P"

## Concatenate into students dataframe
students <- rbind(s23s1p1, s23s1p2, s23s1p3, s23s2p1, s23s2p2, s23s2p3, f22s1p1, f22s2p1, f22s3p1)

## Add j-effect to student table
calc_jeffect <- function(df) {
  df$j.effect <- df$grade - df$mcq.grade

  return(df)
}

students <- calc_jeffect(students)

## Add p-gain to student table
calc_gain <- function(df) {
  df$p.gain <- 100 * (df$new.grade - df$grade) / df$grade

  return(df)
}

students <- calc_gain(students)



## Save big student table
write.csv(students, "student-table.csv")


# -----------------
# MAKE BIG SUMMARY TABLE

# Group students by semester, section, partial
students_summary <- students %>%
  group_by(semester, section, partial)

# Calculate summary statistics
students_summary <- students_summary %>%
  summarise(
    grade_shapiro_p = tryCatch(shapiro.test(grade)$p.value, error = function(e) NA),
    grade_mean = mean(grade, na.rm = TRUE),
    grade_median = median(grade, na.rm = TRUE),
    grade_sd = sd(grade, na.rm = TRUE),
    mcq_grade_shapiro_p = tryCatch(shapiro.test(mcq.grade)$p.value, error = function(e) NA),
    mcq_grade_mean = mean(mcq.grade, na.rm = TRUE),
    mcq_grade_median = median(mcq.grade, na.rm = TRUE),
    mcq_grade_sd = sd(mcq.grade, na.rm = TRUE),
    j_grade_shapiro_p = tryCatch(shapiro.test(j.grade)$p.value, error = function(e) NA),
    j_grade_mean = mean(j.grade, na.rm = TRUE),
    j_grade_median = median(j.grade, na.rm = TRUE),
    j_grade_sd = sd(j.grade, na.rm = TRUE),
    j_effect_shapiro_p = tryCatch(shapiro.test(j.effect)$p.value, error = function(e) NA),
    j_effect_mean = mean(j.effect, na.rm = TRUE),
    j_effect_median = median(j.effect, na.rm = TRUE),
    j_effect_sd = sd(j.effect, na.rm = TRUE),
    p_gain_shapiro_p = tryCatch(shapiro.test(p.gain)$p.value, error = function(e) NA),
    p_gain_mean = mean(p.gain, na.rm = TRUE),
    p_gain_median = median(p.gain, na.rm = TRUE),
    p_gain_sd = sd(p.gain, na.rm = TRUE)
  )

# Save summary table
write.csv(students_summary, "big-summary.csv")


# --------------------------
# COMPARE STATS TO ANSWER QUESTIONS

compute_stats_vec(
  list(
    students$grade[students$semester == "F22" & students$section == 1],
    students$grade[students$semester == "F22" & (students$section == 2 | students$section == 3)]
  ),
  c("msec_22_grades", "jsec_22_grades"), "q1"
)

# Q: do Msec23(MCQ-only) do better than Jsec23(JMCQ) on grades?
compute_stats_vec(
  list(
    students$grade[students$semester == "S23" & students$section == 2],
    students$grade[students$semester == "S23" & (students$section == 1)]
  ),
  c("msec_23_grades", "jsec_23_grades"), "q2"
)

compute_stats_vec(
  list(
    students$grade[students$semester == "S23" & students$section == 2 & students$partial == 1],
    students$grade[students$semester == "S23" & (students$section == 1) & students$partial == 1]
  ),
  c("msec_23_p1_grades", "jsec_23_p1_grades"), "q2-p1"
)

compute_stats_vec(
  list(
    students$grade[students$semester == "S23" & students$section == 2 & students$partial == 2],
    students$grade[students$semester == "S23" & (students$section == 1) & students$partial == 2]
  ),
  c("msec_23_p2_grades", "jsec_23_p2_grades"), "q2-p2"
)

compute_stats_vec(
  list(
    students$grade[students$semester == "S23" & students$section == 2 & students$partial == 3],
    students$grade[students$semester == "S23" & (students$section == 1) & students$partial == 3]
  ),
  c("msec_23_p3_grades", "jsec_23_p3_grades"), "q2-p3"
)

# Q: do Msec22 do better than Msec23 on grades?
compute_stats_vec(
  list(
    students$grade[students$semester == "F22" & students$section == 1],
    students$grade[students$semester == "S23" & (students$section == 2)]
  ),
  c("msec_22_grades", "msec_23_grades"), "q3"
)

compute_stats_vec(
  list(
    students$grade[students$semester == "F22" & students$section == 1],
    students$grade[students$semester == "S23" & (students$section == 2) & students$partial == 1]
  ),
  c("msec_22_grades", "msec_23_p1_grades"), "q3-p1"
)

compute_stats_vec(
  list(
    students$grade[students$semester == "F22" & students$section == 1],
    students$grade[students$semester == "S23" & (students$section == 2) & students$partial == 2]
  ),
  c("msec_22_grades", "msec_23_p2_grades"), "q3-p2"
)

compute_stats_vec(
  list(
    students$grade[students$semester == "F22" & students$section == 1],
    students$grade[students$semester == "S23" & (students$section == 2) & students$partial == 3]
  ), c("msec_22_grades", "msec_23_p3_grades"), "q3-p3"
)

# Q: do Jsec22 do better than Jsec23 on grades?
compute_stats_vec(
  list(
    students$grade[students$semester == "F22" & (students$section == 2 | students$section == 3)],
    students$grade[students$semester == "S23" & (students$section == 1)]
  ),
  c("jsec_22_grades", "jsec_23_grades"), "q4"
)

compute_stats_vec(list(
  students$grade[students$semester == "F22" & (students$section == 2 | students$section == 3)],
  students$grade[students$semester == "S23" & (students$section == 1) & students$partial == 1]
), c("jsec_22_grades", "jsec_23_p1_grades"), "q4-p1")

compute_stats_vec(list(
  students$grade[students$semester == "F22" & (students$section == 2 | students$section == 3)],
  students$grade[students$semester == "S23" & (students$section == 1) & students$partial == 2]
), c("jsec_22_grades", "jsec_23_p2_grades"), "q4-p2")

compute_stats_vec(list(
  students$grade[students$semester == "F22" & (students$section == 2 | students$section == 3)],
  students$grade[students$semester == "S23" & (students$section == 1) & students$partial == 3]
), c("jsec_22_grades", "jsec_23_p3_grades"), "q4-p3")

# Q: do Msec22(MCQ-only) do better than Jsec22(MCQ-only) on MCQ-grades?
compute_stats_vec(
  list(
    students$mcq.grade[students$semester == "F22" & students$section == 1],
    students$mcq.grade[students$semester == "F22" & (students$section == 2 | students$section == 3)]
  ),
  c("msec_22_mcq_grades", "jsec_22_mcq_grades"), "q5"
)

compute_stats_vec(
  list(
    students$mcq.grade[students$semester == "F22" & students$section == 1],
    students$mcq.grade[students$semester == "F22" & (students$section == 2)]
  ),
  c("msec_22_mcq_grades", "jsec_22_sec_2_mcq_grades"), "q5-s2"
)

compute_stats_vec(
  list(
    students$mcq.grade[students$semester == "F22" & students$section == 1],
    students$mcq.grade[students$semester == "F22" & (students$section == 3)]
  ),
  c("msec_22_mcq_grades", "jsec_22_sec_3_mcq_grades"), "q5-s3"
)


# Q: do Msec23(MCQ-only) do better than Jsec23(MCQ-only) on MCQ-grades?
compute_stats_vec(
  list(
    students$mcq.grade[students$semester == "S23" & students$section == 2],
    students$mcq.grade[students$semester == "S23" & (students$section == 1)]
  ),
  c("msec_23_mcq_grades", "jsec_23_mcq_grades"), "q6"
)

compute_stats_vec(
  list(
    students$mcq.grade[students$semester == "S23" & students$section == 2 & students$partial == 1],
    students$mcq.grade[students$semester == "S23" & (students$section == 1) & students$partial == 1]
  ),
  c("msec_23_p1_mcq_grades", "jsec_23_p1_mcq_grades"), "q6-p1"
)

compute_stats_vec(
  list(
    students$mcq.grade[students$semester == "S23" & students$section == 2 & students$partial == 2],
    students$mcq.grade[students$semester == "S23" & (students$section == 1) & students$partial == 2]
  ),
  c("msec_23_p2_mcq_grades", "jsec_23_p2_mcq_grades"), "q6-p2"
)

compute_stats_vec(
  list(
    students$mcq.grade[students$semester == "S23" & students$section == 2 & students$partial == 3],
    students$mcq.grade[students$semester == "S23" & (students$section == 1) & students$partial == 3]
  ),
  c("msec_23_p3_mcq_grades", "jsec_23_p3_mcq_grades"), "q6-p3"
)


# Q: do Jsec22(MCQ-only) do better than Jsec23(MCQ-only) on MCQ-grades?
compute_stats_vec(
  list(
    students$mcq.grade[students$semester == "F22" & (students$section == 2 | students$section == 3)],
    students$mcq.grade[students$semester == "S23" & (students$section == 1)]
  ),
  c("jsec_22_mcq_grades", "jsec_23_mcq_grades"), "q7"
)

compute_stats_vec(
  list(
    students$mcq.grade[students$semester == "F22" & (students$section == 2 | students$section == 3)],
    students$mcq.grade[students$semester == "S23" & (students$section == 1) & students$partial == 1]
  ),
  c("jsec_22_mcq_grades", "jsec_23_p1_mcq_grades"), "q7-p1"
)

compute_stats_vec(
  list(
    students$mcq.grade[students$semester == "F22" & (students$section == 2 | students$section == 3)],
    students$mcq.grade[students$semester == "S23" & (students$section == 1) & students$partial == 2]
  ),
  c("jsec_22_mcq_grades", "jsec_23_p2_mcq_grades"), "q7-p2"
)

compute_stats_vec(
  list(
    students$mcq.grade[students$semester == "F22" & (students$section == 2 | students$section == 3)],
    students$mcq.grade[students$semester == "S23" & (students$section == 1) & students$partial == 3]
  ),
  c("jsec_22_mcq_grades", "jsec_23_p3_mcq_grades"), "q7-p3"
)

# Q: do Jsec22 do better than Jsec23 on J-grades?
compute_stats_vec(
  list(
    students$j.grade[students$semester == "F22" & (students$section == 2 | students$section == 3)],
    students$j.grade[students$semester == "S23" & (students$section == 1)]
  ),
  c("jsec_22_j_grades", "jsec_23_j_grades"), "q8"
)

compute_stats_vec(
  list(
    students$j.grade[students$semester == "F22" & (students$section == 2 | students$section == 3)],
    students$j.grade[students$semester == "S23" & (students$section == 1) & students$partial == 1]
  ),
  c("jsec_22_j_grades", "jsec_23_p1_j_grades"), "q8-p1"
)

compute_stats_vec(
  list(
    students$j.grade[students$semester == "F22" & (students$section == 2 | students$section == 3)],
    students$j.grade[students$semester == "S23" & (students$section == 1) & students$partial == 2]
  ),
  c("jsec_22_j_grades", "jsec_23_p2_j_grades"), "q8-p2"
)

compute_stats_vec(
  list(
    students$j.grade[students$semester == "F22" & (students$section == 2 | students$section == 3)],
    students$j.grade[students$semester == "S23" & (students$section == 1) & students$partial == 3]
  ),
  c("jsec_22_j_grades", "jsec_23_p3_j_grades"), "q8-p3"
)


# Q: do Jsec22 do better than Jsec23 on JEffects?
compute_stats_vec(
  list(
    students$j.effect[students$semester == "F22" & (students$section == 2 | students$section == 3)],
    students$j.effect[students$semester == "S23" & (students$section == 1)]
  ),
  c("jsec_22_j_effect", "jsec_23_j_effect"), "q9"
)

compute_stats_vec(
  list(
    students$j.effect[students$semester == "F22" & (students$section == 2 | students$section == 3)],
    students$j.effect[students$semester == "S23" & (students$section == 1) & students$partial == 1]
  ),
  c("jsec_22_j_effect", "jsec_23_p1_j_effect"), "q9-p1"
)

compute_stats_vec(
  list(
    students$j.effect[students$semester == "F22" & (students$section == 2 | students$section == 3)],
    students$j.effect[students$semester == "S23" & (students$section == 1) & students$partial == 2]
  ),
  c("jsec_22_j_effect", "jsec_23_p2_j_effect"), "q9-p2"
)

compute_stats_vec(
  list(
    students$j.effect[students$semester == "F22" & (students$section == 2 | students$section == 3)],
    students$j.effect[students$semester == "S23" & (students$section == 1) & students$partial == 3]
  ),
  c("jsec_22_j_effect", "jsec_23_p3_j_effect"), "q9-p3"
)

# Q: do Msec23(MCQ-only) do better than Jsec23(JMCQ) on pgains?
compute_stats_vec(
  list(
    students$p.gain[students$semester == "S23" & students$section == 2],
    students$p.gain[students$semester == "S23" & (students$section == 1)]
  ),
  c("msec_23_p_gain", "jsec_23_p_gain"), "q10"
)

compute_stats_vec(
  list(
    students$p.gain[students$semester == "S23" & students$section == 2 & students$partial == 1],
    students$p.gain[students$semester == "S23" & (students$section == 1) & students$partial == 1]
  ),
  c("msec_23_p1_p_gain", "jsec_23_p1_p_gain"), "q10-p1"
)

compute_stats_vec(
  list(
    students$p.gain[students$semester == "S23" & students$section == 2 & students$partial == 2],
    students$p.gain[students$semester == "S23" & (students$section == 1) & students$partial == 2]
  ),
  c("msec_23_p2_p_gain", "jsec_23_p2_p_gain"), "q10-p2"
)
