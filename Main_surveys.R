library(ggplot2)
library(purrr)

# Function to plot and save
plot_and_save <- function(data, column) {
  
  # Create the plot
  p <- ggplot(data, aes_string(column)) + 
    geom_bar() + 
    theme_minimal() + 
    labs(title = paste("Barplot of", column),
         x = column,
         y = "Count")
  
  # Save the plot
  ggsave(filename = paste0("survey-plots/", column, ".png"), plot = p, height = 5, width = 7)
}

# Apply the function to each column in pre_survey and post_survey
names(pre_survey) %>%
  walk(~plot_and_save(pre_survey, .))
names(post_survey) %>%
  walk(~plot_and_save(post_survey, .))
