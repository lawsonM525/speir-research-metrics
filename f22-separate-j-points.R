
separate_f22 <- function(partial) {
  
  
  # Calculate totals
  partial$mcq_points <- partial$P1a.MCQ + partial$P1b.MCQ
  partial$just_points <- partial$P1a.J + partial$P1b.J
  
  # Calculate j gain
  partial$j_gain <- (((partial$P1.Score - ((partial$mcq_points/60)*100))/((partial$mcq_points/60)*100))) * 100
  partial$j_cons <- partial$just_points - partial$mcq_points
  
  # Return dataframe
  return(partial)
  
}