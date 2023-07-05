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

boxplot_df <- function(df, dfname){
  
  # Convert all columns to numeric
  df <- mutate_all(df, as.numeric)
  
  # Vector of column names
  cols <- colnames(df)
  
  # Calculate the global minimum and maximum values across all columns
  global_min <- min(sapply(df, min, na.rm = TRUE))
  global_max <- max(sapply(df, max, na.rm = TRUE))
  
  # Initialize an empty list to store plots
  plot_list <- list()
  
  # For each column
  for (col in cols){
    # Create a boxplot
    p <- ggplot(df, aes_string(x = 1, y = col)) + 
      geom_boxplot(outlier.shape = NA) +
      geom_jitter(width = 0.1, alpha = 0.5, aes_string(y = col)) +
      labs(title = paste0(dfname,"-", col), y = col) +
      theme_economist() +
      coord_cartesian(ylim = c(0,100))   # Set y-axis limits
    
    # Add the plot to the list
    plot_list[[col]] <- p
  }
  
  # Combine the boxplots
  combined_plot <- do.call(grid.arrange, c(plot_list, ncol=2))
  
  # Save the combined plot as a PNG image
  ggsave(filename = paste0("plots/", dfname, "_boxplots.png"),
         plot = combined_plot)
}

