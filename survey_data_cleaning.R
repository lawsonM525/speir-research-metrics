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
  
  # Check if "Question1_Part2"(what is your last name?) exists in the columns 
  # and rename it to "key" 
  if ("Question1_Part2" %in% colnames(df)) {
    colnames(df)[colnames(df) == "Question1_Part2"] <- "key"
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
  df <- df %>% select(key, section, starts_with("Question"))
  
}


# Apply the cleaning function to both surveys
pre_survey <- clean_data(pre_survey)
write.csv(pre_survey, 'csv-files/S23_Pre_Survey_Cleaned.csv')
post_survey <- clean_data(post_survey)
write.csv(post_survey, 'csv-files/S23_Post_Survey_Cleaned.csv')

## Remove section 3 responses in both surveys
pre_survey <- pre_survey %>% filter(section != 3)
post_survey <- post_survey %>% filter(section != 3)

## Making numeric version of the survey responses

df_to_numeric <- function(df){
  
  df <- df %>% mutate(across(where(function(x) {
    is.character(x) && any(x %in% c("Strongly disagree", "Somewhat disagree", 
                                    "Neither agree nor disagree", "Somewhat agree", 
                                    "Strongly agree"))
  }), ~ case_when(. == "Strongly disagree" ~ 1,
                  . == "Somewhat disagree" ~ 2,
                  . == "Neither agree nor disagree" ~ 3,
                  . == "Somewhat agree" ~ 4,
                  . == "Strongly agree" ~ 5,
                  TRUE ~ as.numeric(.))))  # Keep values as they are if not matched
  
}

pre_survey_num <- df_to_numeric(pre_survey)
post_survey_num <- df_to_numeric(post_survey)

