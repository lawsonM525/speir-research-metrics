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
  
  partial$j_gain <- (((100*partial$SC0)/80 - (100*partial$mcq_points)/40)/((100/40)*partial$mcq_points)) * 100
  
  # Return new df
  return(partial)
  
}


## adding comment points to justification points
separate_comments <- function(grades_df, partial, partialname, n){
  
  # Convert SC0 column to numeric
  partial$SC0 <- as.numeric(as.character(partial$SC0))
  
  # Create a data frame to hold comments, aligned to 'partial' data frame
  aligned_comments <- as.numeric(grades_df[match(partial$key, grades_df$key), paste0(partialname, ".Comments")])
  
  # Convert comments to numeric and handle NA values
  aligned_comments_vector <- as.numeric(as.character(aligned_comments[[1]]))
  aligned_comments_vector[is.na(aligned_comments_vector)] <- 0
  
  # Loop through each question number
  for (i in 1:n) {
    
    # Identify columns for current question
    q_columns <- grep(paste0("^q", i, ".*[abc]$"), names(partial), value = TRUE)
    j_columns <- grep(paste0("^j", i, ".*[abc]$"), names(partial), value = TRUE)
    
    # Convert those columns to numeric
    partial[q_columns] <- lapply(partial[q_columns], function(x) as.numeric(as.character(x)))
    partial[j_columns] <- lapply(partial[j_columns], function(x) as.numeric(as.character(x)))
    
    # Calculate the row sums for current question
    partial[paste0("mcq_", i)] <- rowSums(partial[q_columns], na.rm = TRUE)
    partial[paste0("just_", i)] <- rowSums(partial[j_columns], na.rm = TRUE)
  }
  
  # Add comments to justification points
  partial$justification_points <- ((rowSums(partial[grep("^just_.*$", names(partial))], na.rm = TRUE) + aligned_comments_vector)/(n*5))*100
  is.na(partial$justification_points) <- partial$justification_points == 0
  
  # Calculate the total mcq points
  partial$mcq_points <- (rowSums(partial[grep("^mcq_.*$", names(partial))], na.rm = TRUE)/(n*5))*100
  is.na(partial$mcq_points) <- partial$mcq_points == 0
  
  
  if(partialname != "P3"){ 
    partial$SC0 <- 100*(partial$SC0/80) 
  }
  
  is.na(partial$SC0) <- partial$SC0 == 0
  
  partial$j_gain <- ((partial$SC0 - partial$mcq_points)/partial$mcq_points) * 100
  partial$j_gain[is.infinite(partial$j_gain)] <- NA
  partial$j_cons <- partial$justification_points - partial$mcq_points
  
  
  # Return new df
  return(partial)
  
}



# USed to separate P3 into P3a and P3b
separate_partials <- function(s1p3, s2p3){
  # Define columns that are common to both s1p3a and s1p3b
  common_cols <- c("X", "StartDate", "EndDate", "Progress", "Duration..in.seconds.", "Finished", 
                   "RecordedDate", "ResponseId", "key", "Section")
  
  # Define additional columns for s1p3a (Q1-Q4)
  s1p3a_cols <- c(paste0("q", 1:4, "_total"), unlist(lapply(1:4, function(i) paste0("q", i, letters[1:3]))),
                  unlist(lapply(1:4, function(i) paste0("j", i, letters[1:3]))), unlist(lapply(1:4, function(i) paste0("c", i, letters[1:3]))),
                  paste0("T", 1:4, "_First.Click:Click.Count"), paste0("mcq_", 1:4), paste0("just_", 1:4), "SC0", "score_check")
  
  # Check if each column exists in the data frame, and only keep those that do
  s1p3a_cols <- s1p3a_cols[s1p3a_cols %in% colnames(s1p3)]
  
  # Define additional columns for s1p3b (Q5-Q6)
  s1p3b_cols <- c(paste0("q", 5:6, "_total"), unlist(lapply(5:6, function(i) paste0("q", i, letters[1:3]))),
                  unlist(lapply(5:6, function(i) paste0("j", i, letters[1:3]))), unlist(lapply(5:6, function(i) paste0("c", i, letters[1:3]))),
                  paste0("T", 5:6, "_First.Click:Click.Count"), paste0("mcq_", 5:6), paste0("just_", 5:6), "SC0", "score_check")
  
  # Check if each column exists in the data frame, and only keep those that do
  s1p3b_cols <- s1p3b_cols[s1p3b_cols %in% colnames(s1p3)]
  
  # Create s1p3a data frame
  s1p3a <- s1p3[, c(common_cols, s1p3a_cols)]
  
  # Create s1p3b data frame
  s1p3b <- s1p3[, c(common_cols, s1p3b_cols)]
  
  # Define columns that are common to both s2p3a and s2p3b
  common_cols <- c("X", "StartDate", "EndDate", "Progress", "Duration..in.seconds.", "Finished", 
                   "RecordedDate", "ResponseId", "key", "Section")
  
  # Define additional columns for s2p3a (Q1-Q4)
  s2p3a_cols <- c(paste0("q", 1:4, "_total"), unlist(lapply(1:4, function(i) paste0("q", i, letters[1:3]))),
                  unlist(lapply(1:4, function(i) paste0("c", i, letters[1:3]))),
                  paste0("T", 1:4, "_First.Click:Click.Count"), paste0("mcq_", 1:4), paste0("just_", 1:4), "SC0", "score_check")
  
  # Check if each column exists in the data frame, and only keep those that do
  s2p3a_cols <- s2p3a_cols[s2p3a_cols %in% colnames(s2p3)]
  
  # Define additional columns for s2p3b (Q5-Q6)
  s2p3b_cols <- c(paste0("q", 5:6, "_total"), unlist(lapply(5:6, function(i) paste0("q", i, letters[1:3]))),
                  unlist(lapply(5:6, function(i) paste0("c", i, letters[1:3]))),
                  paste0("T", 5:6, "_First.Click:Click.Count"), paste0("mcq_", 5:6), paste0("just_", 5:6), "SC0", "score_check")
  
  # Check if each column exists in the data frame, and only keep those that do
  s2p3b_cols <- s2p3b_cols[s2p3b_cols %in% colnames(s2p3)]
  
  # Create s2p3a data frame
  s2p3a <- s2p3[, c(common_cols, s2p3a_cols)]
  
  # Create s2p3b data frame
  s2p3b <- s2p3[, c(common_cols, s2p3b_cols)]
  
  # Create a list to store all the data frames
  results <- list(s1p3a = s1p3a, s1p3b = s1p3b, s2p3a = s2p3a, s2p3b = s2p3b)
  
  # Return the list
  return(results)
}


