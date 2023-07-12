library(dplyr)

# Function to compute stats and test normality
compute_stats <- function(df, col_vector, filename){
  
  # Create an empty data frame to store results
  results <- data.frame(Column = character(),
                        Mean = numeric(),
                        Median = numeric(),
                        StdDev = numeric(),
                        P_Value = numeric(),
                        Normal = character(),
                        Wilcoxon_P_Value = numeric()) # Added column for Wilcoxon rank-sum test P-value
  
  # For each column
  for (col in col_vector){
    # Check if the column is numeric, if not convert it to numeric
    if(!is.numeric(df[[col]])){
      df[[col]] <- as.numeric(as.character(df[[col]]))
    }
    
    # Compute mean and median
    mean_val <- mean(df[[col]], na.rm = TRUE)
    median_val <- median(df[[col]], na.rm = TRUE)
    sd_val <- sd(df[[col]], na.rm = TRUE)
    
    # Initialize p-value and is_normal variables
    p_val <- NA
    is_normal <- "Not enough data"
    
    # Remove NA values from the column
    column_data <- na.omit(df[[col]])
    
    # Perform Shapiro-Wilk test for normality only if there are enough non-NA values
    if(length(column_data) >= 3 && length(column_data) <= 5000) {
      shapiro_test <- shapiro.test(column_data)
      p_val <- shapiro_test$p.value
      
      # Check if p-value is less than 0.05
      is_normal <- ifelse(p_val < 0.05, "Not Normal", "Normal")
    }
    
    # Add results to the data frame
    results <- rbind(results, data.frame(Column = col, Mean = mean_val,
                                         Median = median_val, StdDev = sd_val,
                                         P_Value = p_val, Normal = is_normal,
                                         Wilcoxon_P_Value = NA)) # Default to NA for Wilcoxon P-value
  }
  
  # If there are exactly two columns in col_vector, perform the Wilcoxon rank-sum test
  if(length(col_vector) == 2){
    col1_data <- na.omit(df[[col_vector[1]]])
    col2_data <- na.omit(df[[col_vector[2]]])
    
    # Check if there are sufficient non-missing observations
    if(length(col1_data) > 1 && length(col2_data) > 1){
      wilcox_test <- wilcox.test(col1_data, col2_data)
      wilcox_p_val <- wilcox_test$p.value
      
      # Add Wilcoxon test p-value to the results data frame
      results$Wilcoxon_P_Value <- ifelse(is.na(results$Wilcoxon_P_Value), wilcox_p_val, results$Wilcoxon_P_Value)
    }
  }
  
  # Write results to a csv file
  write.csv(results, file = paste0("stats/", filename, "_stats.csv"), row.names = FALSE)
  
  return(results)
}

