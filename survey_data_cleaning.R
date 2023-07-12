library(dplyr)
library(stringr)

## cURRENTLY USING f22 FILES, CAN BE UPDATED TO PUT IN S23

# Load the data
pre_survey <- read.csv("F22-Pre-Survey_ALL - Sheet1.csv")
post_survey <- read.csv("F22-Post-Survey_ALL - Sheet1.csv")


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
  if ("Question2" %in% colnames(df)) {
    colnames(df)[colnames(df) == "Question2"] <- "key"
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
  
  # Select only the columns named "Key","SEction" and all columns that start with "Question"
  df <- df %>% select(key, Section, starts_with("Question"))
  
}

## Remove section 3 responses in both surveys

# Commented out for Fall 2022
#pre_survey <- pre_survey %>% filter(Section != 3)
#post_survey <- post_survey %>% filter(Section != 3)

# Apply the cleaning function to both surveys
pre_survey <- clean_data(pre_survey)
write.csv(pre_survey, 'csv-files/F22_Pre_Survey_Cleaned.csv')
post_survey <- clean_data(post_survey)
write.csv(post_survey, 'csv-files/F22_Post_Survey_Cleaned.csv')

## Making numeric version of the survey responses

df_to_numeric <- function(df){
  
  df <- df %>% mutate(across(where(function(x) {
    is.character(x) && any(x %in% c("Strongly disagree", "Somewhat disagree", 
                                    "Neither agree nor disagree", "Somewhat agree", 
                                    "Strongly agree", "Never", "Rarely", 
                                    "Sometimes", "Often", "Always", 
                                    "Less than 1 hour", "Between 1 and 2 hours", 
                                    "Between 2 and 5 hours", "Between 5 and 10 hours", 
                                    "More than 10 hours"))
  }), ~ case_when(. == "Strongly disagree" ~ 1,
                  . == "Somewhat disagree" ~ 2,
                  . == "Neither agree nor disagree" ~ 3,
                  . == "Somewhat agree" ~ 4,
                  . == "Strongly agree" ~ 5,
                  . == "Never" ~ 1,
                  . == "Rarely" ~ 2,
                  . == "Sometimes" ~ 3,
                  . == "Often" ~ 4,
                  . == "Always" ~ 5,
                  . == "Less than 1 hour" ~ 1,
                  . == "Between 1 and 2 hours" ~ 2,
                  . == "Between 2 and 5 hours" ~ 3,
                  . == "Between 5 and 10 hours" ~ 4,
                  . == "More than 10 hours" ~ 5,
                  TRUE ~ as.numeric(.))))  # Keep values as they are if not matched
  
  return(df)
}


pre_survey_num <- df_to_numeric(pre_survey)
post_survey_num <- df_to_numeric(post_survey)

## Encoding all question values as numeric
# Get column names that start with "Question"
question_cols_pre <- names(pre_survey_num)[grepl("^Question", names(pre_survey_num))]
question_cols_post <- names(post_survey_num)[grepl("^Question", names(post_survey_num))]

# Convert those columns to numeric
pre_survey_num[question_cols_pre] <- lapply(pre_survey_num[question_cols_pre], as.numeric)
post_survey_num[question_cols_post] <- lapply(post_survey_num[question_cols_post], as.numeric)


write.csv(pre_survey_num, "csv-files/F22_Pre_Survey_Numeric.csv")
write.csv(post_survey_num, "csv-files/F22_Post_Survey_Numeric.csv")
