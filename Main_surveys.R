library(ggplot2)
library(purrr)

source("averages.R")

# Load the data
pre_survey <- read.csv("csv-files/S23_Pre_Survey_Cleaned.csv")
post_survey <- read.csv("csv-files/S23_Post_Survey_Cleaned.csv")
pre_survey_num <- read.csv("csv-files/S23_Pre_Survey_Numeric.csv")
post_survey_num <-read.csv("csv-files/S23_Post_Survey_Numeric.csv")


# Function to plot and save
plot_ans <- function(data, column, df_name, scale_type) {
  
  # Define the order of levels for different scales
  scale_levels <- if (scale_type == "frequency") {
    c("Never","Rarely","Sometimes","Often","Always")
  } else if (scale_type == "time") {
    c("Less than 1 hour", "Between 1 and 2 hours", 
      "Between 2 and 5 hours", "Between 5 and 10 hours", 
      "More than 10 hours")
  } else if (scale_type == "agree") {
    c("Strongly disagree","Somewhat disagree",
      "Neither agree nor disagree","Somewhat agree", "Strongly agree")
  } else {
    stop("Unknown scale_type")
  }
  
  # Loop over sections
  for (section in unique(data$section)) {
    # Subset the data based on the section
    data_sub <- data[data$section == section, ]
    
    # Create the plot
    p <- ggplot(data_sub, aes(x = data_sub[[column]])) + 
      geom_bar(aes(fill = data_sub[[column]]), color = "black") + 
      theme_minimal() + 
      labs(title = paste("Barplot of", column, "in section", section),
           x = column,
           y = "Count") +
      scale_x_discrete(limits = scale_levels) +
      scale_fill_brewer(palette = "Set3")
    
    # Save the plot
    ggsave(
      filename = 
        paste0("survey-plots/s23/", df_name, "_", column, "_section_", section, ".png"),
      plot = p, height = 5, width = 10)
  }
}



names(pre_survey)[grep("4|5|7|6_Part2", names(pre_survey))] %>%
  walk(~plot_ans(pre_survey, ., "Pre", "agree"))
names(post_survey)[grep("4|5|7|6_Part2", names(post_survey))] %>%
  walk(~plot_ans(post_survey, ., "Post", "agree"))

names(pre_survey)[grep("6_Part1", names(pre_survey))] %>%
  walk(~plot_ans(pre_survey, ., "Pre", "frequency"))
names(post_survey)[grep("6_Part1", names(post_survey))] %>%
  walk(~plot_ans(post_survey, ., "Post", "frequency"))

names(pre_survey)[grep("6_Part3", names(pre_survey))] %>%
  walk(~plot_ans(pre_survey, ., "Pre", "time"))
names(post_survey)[grep("6_Part3", names(post_survey))] %>%
  walk(~plot_ans(post_survey, ., "Post", "time"))


## Compute stats

## For each question column call the compute stats on a new data frame 
## from the column that has a column for sec 1 and a column for sec 2

# Extract the column names for the questions
question_cols <- names(pre_survey_num)[grepl("Question", names(pre_survey_num))]

# Create an empty data frame to store results
all_results <- data.frame()

# Loop over each question column
for (q_col in question_cols) {
  
  # Split the data frame based on 'section' column
  df_s1 <- data.frame(s1 = pre_survey_num[pre_survey_num$section == 1, q_col])
  df_s2 <- data.frame(s2 = pre_survey_num[pre_survey_num$section == 2, q_col])
  
  # Calculate the difference in lengths
  len_diff <- abs(nrow(df_s1) - nrow(df_s2))
  
  # Pad the shorter vector with NA's
  if (nrow(df_s1) < nrow(df_s2)) {
    df_s1 <- rbind(df_s1, data.frame(s1 = rep(NA, len_diff)))
  } else if (nrow(df_s2) < nrow(df_s1)) {
    df_s2 <- rbind(df_s2, data.frame(s2 = rep(NA, len_diff)))
  }
  
  # Check if df_s1 and df_s2 have rows before proceeding
  if (nrow(df_s1) > 0 && nrow(df_s2) > 0) {
    # Combine both dataframes into one with separate columns for each section
    df_combined <- data.frame(s1 = df_s1$s1, s2 = df_s2$s2)
    
    # Call compute_stats function
    results <- compute_stats(df_combined, c("s1", "s2"), paste0("pre-survey/", q_col))
    
    # Add results to all_results dataframe
    all_results <- rbind(all_results, results)
  }
}




# Save all_results to a CSV file
write.csv(all_results, file = "stats/pre-survey/all_results.csv", row.names = FALSE)


## Post - Survey

question_cols <- names(post_survey_num)[grepl("Question", names(post_survey_num))]

# Create an empty data frame to store results
all_results <- data.frame()

# Loop over each question column
for (q_col in question_cols) {
  
  # Split the data frame based on 'section' column
  df_s1 <- data.frame(s1 = post_survey_num[post_survey_num$section == 1, q_col])
  df_s2 <- data.frame(s2 = post_survey_num[post_survey_num$section == 2, q_col])
  
  # Calculate the difference in lengths
  len_diff <- abs(nrow(df_s1) - nrow(df_s2))
  
  # Pad the shorter vector with NA's
  if (nrow(df_s1) < nrow(df_s2)) {
    df_s1 <- rbind(df_s1, data.frame(s1 = rep(NA, len_diff)))
  } else if (nrow(df_s2) < nrow(df_s1)) {
    df_s2 <- rbind(df_s2, data.frame(s2 = rep(NA, len_diff)))
  }
  
  # Check if df_s1 and df_s2 have rows before proceeding
  if (nrow(df_s1) > 0 && nrow(df_s2) > 0) {
    # Combine both dataframes into one with separate columns for each section
    df_combined <- data.frame(s1 = df_s1$s1, s2 = df_s2$s2)
    
    # Call compute_stats function
    results <- compute_stats(df_combined, c("s1", "s2"), paste0("post-survey/", q_col))
    
    # Add results to all_results dataframe
    all_results <- rbind(all_results, results)
  }
}




# Save all_results to a CSV file
write.csv(all_results, file = "stats/post-survey/all_results.csv", row.names = FALSE)


