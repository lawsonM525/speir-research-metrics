library(dplyr)

# Function to compute stats and test normality
compute_stats <- function(df, col_vector, filename){
  
  # Create an empty data frame to store results
  results <- data.frame(Column = character(),
                        Mean = numeric(),
                        Median = numeric(),
                        StdDev = numeric(),
                        P_Value = numeric(),
                        Normal = character())
  
  # For each column
  for (col in col_vector){
    # Compute mean and median
    mean_val <- mean(df[[col]], na.rm = TRUE)
    median_val <- median(df[[col]], na.rm = TRUE)
    sd_val <- sd(df[[col]], na.rm = TRUE)
    
    # Perform Shapiro-Wilk test for normality
    shapiro_test <- shapiro.test(df[[col]])
    p_val <- shapiro_test$p.value
    
    # Check if p-value is less than 0.05
    is_normal <- ifelse(p_val < 0.05, "Not Normal", "Normal")
    
    # Add results to the data frame
    results <- rbind(results, data.frame(Column = col, Mean = mean_val,
                                         Median = median_val, StdDev = sd_val,
                                         P_Value = p_val, Normal = is_normal))
  }
  
  # Write results to a csv file
  write.csv(results, file = paste0("stats/", filename, "_stats.csv"), row.names = FALSE)
  
  return(results)
}
