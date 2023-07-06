library(dplyr)
library(stringr)

# Load the data
pre_survey <- read.csv("S23-Pre-Survey-ALL - Sheet1.csv")
post_survey <- read.csv("S23-Post-Survey-ALL - Sheet1.csv")


# Function to rename the question columns
rename_func <- function(df){
  
  # Get the column names
  names <- colnames(df)
  
  # Use stringr::str_replace_all() to replace the "Q" with "Question", 
  # and the "." with "_Part"
  new_names <- stringr::str_replace_all(names, 
                                        c("^Q" = "Question", "\\." = "_Part"))
  
  # Assign the new names to the data frame
  colnames(df) <- new_names
  
  # Check if "Question1_Part2"(what is your last name?) exists in the names 
  # and rename it to "Key" 
  if ("Question1_Part2" %in% colnames(df)) {
    colnames(df)[colnames(df) == "Question1_Part2"] <- "Key"
  }
  
  return(df)
}


# Function to clean the data
clean_data <- function(df){
  
  # Remove the first two rows of each column
  df <- df[-c(1,2),]
  
  # Remove columns that are all NA or all "###########"
  df <- df[, !apply(df, 2, function(x) all(is.na(x) | x == "##########"))]
  
  # Apply the rename function to the columns
  df <- rename_func(df)
  
  # Select only the columns named "Key" and all columns that start with "Question"
  df <- df %>% select(Key, starts_with("Question"))
  
  # Recode scale from strongly disagree to strongly agree
  df <- df %>% mutate(across(where(function(x) {
    is.character(x) && any(x %in% c("Strongly disagree", "Somewhat disagree", 
                                    "Neither agree nor disagree", "Somewhat agree", 
                                    "Strongly agree"))
  }), ~ as.numeric(recode(., "Strongly disagree" = 1,
                          "Somewhat disagree" = 2,
                          "Neither agree nor disagree" = 3,
                          "Somewhat agree" = 4,
                          "Strongly agree" = 5 ))))
}


# Apply the cleaning function to both surveys
pre_survey <- clean_data(pre_survey)
post_survey <- clean_data(post_survey)
