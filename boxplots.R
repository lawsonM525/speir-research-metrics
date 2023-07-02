library(ggplot2)
library(ggthemes)
library(gridExtra)
library(dplyr)

boxplot_partials <- function (section1, section2, section1name, section2name){
  # Vector of column names
  partials <- c('P1.After.Comments', 'P2.After.Comments',
                'P3.After.Comments', 'Number.Grade')
  
  partials_short <- c('P1', 'P2', 'P3', 'Final')
  
  names_map <- setNames(partials_short, partials)
  
  # Add a new column 'section' to both dataframes
  section1$section <- section1name
  section2$section <- section2name
  
  # For each partial
  for (partial in partials){
    # Create a boxplot for each section
    p1 <- ggplot(section1, aes_string(x = "section", y = partial)) + 
      geom_boxplot(outlier.shape = NA) +
      geom_jitter(width = 0.1, alpha = 0.5) +
      labs(title = paste0(section1name,"-",names_map[partial]), y = partial) +
      theme_economist() +
      coord_cartesian(ylim = c(0, 100))   # Set y-axis limits
    
    p2 <- ggplot(section2, aes_string(x = "section", y = partial)) + 
      geom_boxplot(outlier.shape = NA) +
      geom_jitter(width = 0.1, alpha = 0.5) +
      labs(title = paste0(section2name,"-",names_map[partial]), y = partial) +
      theme_economist() +
      coord_cartesian(ylim = c(0, 100))   # Set y-axis limits
    
    # Combine the boxplots
    combined_plot <- grid.arrange(p1, p2, ncol=2)
    
    # Save the combined plot as a PNG image
    ggsave(filename = paste0("plots/", partial, "_boxplots.png"), plot = combined_plot)
  }
}
