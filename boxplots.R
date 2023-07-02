library(ggplot2)
library(ggthemes)
library(gridExtra)
library(dplyr)

boxplot_partials <- function (section1, section2, section1name, section2name){
  # Vector of column names
  partials <- c('P1.After.Comments', 'P2.After.Comments', 'P3.After.Comments', 'Number.Grade')
  
  # For each partial
  for (partial in partials){
    # Create a boxplot for each section
    p1 <- ggplot(section1, aes_string(y = partial)) + 
      geom_boxplot(fill="blue", alpha=0.5) +
      labs(title = paste0(section1name), x = section1name, y = partial) +
      theme_economist() +
      ylim(0, 100)   # Set y-axis limits
    
    p2 <- ggplot(section2, aes_string(y = partial)) + 
      geom_boxplot(fill="blue", alpha=0.5) +
      labs(title = paste0(section2name), x = section2name, y = partial) +
      theme_economist() +
      ylim(0, 100)   # Set y-axis limits
    
    # Combine the boxplots
    combined_plot <- grid.arrange(p1, p2, ncol=2)
    
    # Save the combined plot as a PNG image
    ggsave(filename = paste0("plots/", partial, "_boxplots.png"), plot = combined_plot)
  }
}
