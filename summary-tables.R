j_gain_table <- function(){
  
  # P1 stats
  p1_mcq_mean <- mean(s1p1$mcq_points, na.rm=TRUE) 
  p1_mcq_med <- median(s1p1$mcq_points, na.rm=TRUE)
  p1_j_mean <- mean(s1p1$justification_points, na.rm=TRUE)
  p1_j_med <- median(s1p1$justification_points, na.rm=TRUE) 
  p1_total_mean <- mean(s1_grades$P1.After.Comments, na.rm=TRUE)
  p1_total_med <- median(s1_grades$P1.After.Comments, na.rm=TRUE)
  p1_jgain_mean <- mean(s1p1$j_gain, na.rm=TRUE)
  p1_jgain_med <- median(s1p1$j_gain, na.rm=TRUE)
  p1_jcons_mean <- mean(s1p1$j_cons, na.rm=TRUE)
  p1_jcons_med <- median(s1p1$j_cons, na.rm=TRUE)
  
  # P2 stats 
  p2_mcq_mean <- mean(s1p2$mcq_points, na.rm=TRUE)
  p2_mcq_med <- median(s1p2$mcq_points, na.rm=TRUE)
  p2_j_mean <- mean(s1p2$justification_points, na.rm=TRUE)
  p2_j_med <- median(s1p2$justification_points, na.rm=TRUE)
  p2_total_mean <- mean(s1_grades$P2.After.Comments, na.rm=TRUE)
  p2_total_med <- median(s1_grades$P2.After.Comments, na.rm=TRUE)
  p2_jgain_mean <- mean(s1p2$j_gain, na.rm=TRUE)
  p2_jgain_med <- median(s1p2$j_gain, na.rm=TRUE)
  p2_jcons_mean <- mean(s1p2$j_cons, na.rm=TRUE)
  p2_jcons_med <- median(s1p2$j_cons, na.rm=TRUE)
  
  # P3 stats
  p3_mcq_mean <- mean(s1p3a$mcq_points, na.rm=TRUE) 
  p3_mcq_med <- median(s1p3a$mcq_points, na.rm=TRUE)
  p3_j_mean <- mean(s1p3a$justification_points, na.rm=TRUE)
  p3_j_med <- median(s1p3a$justification_points, na.rm=TRUE)
  p3_total_mean <- mean(s1p3a$SC0, na.rm=TRUE)
  p3_total_med <- median(s1p3a$SC0, na.rm=TRUE)
  p3_jgain_mean <- mean(s1p3a$j_gain, na.rm=TRUE)
  p3_jgain_med <- median(s1p3a$j_gain, na.rm=TRUE)
  p3_jcons_mean <- mean(s1p3a$j_cons, na.rm=TRUE)
  p3_jcons_med <- median(s1p3a$j_cons, na.rm=TRUE)
  
  # Construct dataframe
  df <- data.frame(
    metric = c("MCQ Mean", "MCQ Median", "J Mean", "J Median", "Total Mean", "Total Median", "J Gain Mean", "J Gain Median", "J Cons Mean", "J Cons Median"),
    p1 = c(p1_mcq_mean, p1_mcq_med, p1_j_mean, p1_j_med, p1_total_mean, p1_total_med, p1_jgain_mean, p1_jgain_med, p1_jcons_mean, p1_jcons_med),
    p2 = c(p2_mcq_mean, p2_mcq_med, p2_j_mean, p2_j_med, p2_total_mean, p2_total_med, p2_jgain_mean, p2_jgain_med, p2_jcons_mean, p2_jcons_med),
    p3 = c(p3_mcq_mean, p3_mcq_med, p3_j_mean, p3_j_med, p3_total_mean, p3_total_med, p3_jgain_mean, p3_jgain_med, p3_jcons_mean, p3_jcons_med)
  )
  
  # Write datframe to csv
  write.csv(df, "stats/j_gain_summary.csv", row.names=TRUE)
}