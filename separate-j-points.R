separate <- function(partial, partialname, n){
  
  # Convert SC0 column to numeric
  partial$SC0 <- as.numeric(as.character(partial$SC0))
  
  # Loop through each question number
  for (i in 1:n) {
    
    # Identify columns for current question
    q_columns <- grep(paste0("^q", i, ".*[abc]$"), names(partial), value = TRUE)
    j_columns <- grep(paste0("^j", i, ".*[abc]$"), names(partial), value = TRUE)
    
    # Convert those columns to numeric
    partial[q_columns] <- lapply(partial[q_columns], 
                                 function(x) as.numeric(as.character(x)))
    partial[j_columns] <- lapply(partial[j_columns], 
                                 function(x) as.numeric(as.character(x)))
    
    # Calculate the row sums for current question
    partial[paste0("mcq_", i)] <- rowSums(partial[q_columns], na.rm = TRUE)
    partial[paste0("just_", i)] <- rowSums(partial[j_columns], na.rm = TRUE)
  }
  
  # Calculate the total mcq and justification points
  partial$mcq_points <- rowSums(partial[grep("^mcq_.*$", names(partial))], 
                                na.rm = TRUE)
  partial$justification_points <- rowSums(partial[grep("^just_.*$", 
                                                       names(partial))], 
                                          na.rm = TRUE)
  
  
  if(partialname =="P3"){
    partial$mcq_points <- (partial$mcq_points/30)*40
    partial$justification_points <- (partial$justification_points/30)*40
    partial$SC0 <- (partial$SC0/60)*80
  }
  
  partial$j_gain <- ((partial$SC0 - partial$mcq_points)/partial$mcq_points) * 100
  
  # Return new df
  return(partial)
  
}