library(ggplot2)
library(ggthemes)
library(dplyr)
library(gridExtra)

# Function to assign tiers
assign_tiers <- function(x){
  range_x <- range(x, na.rm = TRUE)
  breaks <- seq(from = range_x[1], to = range_x[2], length.out = 4)
  cut(x, breaks = breaks, labels = c("C", "B", "A"), include.lowest = TRUE)
}


boxplot_partials_tiers <- function (section1, section2, section1name, section2name){
  # Vector of column names
  partials <- c('P1.After.Comments', 'P2.After.Comments',
                'P3.After.Comments', 'Number.Grade')
  
  partials_short <- c('P1', 'P2', 'P3', 'Final')
  
  names_map <- setNames(partials_short, partials)
  
  # Define color mapping for each tier
  tier_colors <- c("A" = "blue", "B" = "green", "C" = "red")
  
  # For each partial
  for (partial in partials){
    # Assign tiers to each section
    section1$tier <- assign_tiers(section1[[partial]])
    section2$tier <- assign_tiers(section2[[partial]])
    
    # For each tier
    for (tier in c('A', 'B', 'C')){
      # Create a boxplot for each section for each tier
      p1 <- ggplot(section1[section1$tier == tier,], aes_string(x = "tier", y = partial, color = "tier")) + 
        geom_boxplot(outlier.shape = NA) +
        geom_jitter(width = 0.1, alpha = 0.5) +
        scale_color_manual(values = tier_colors) +
        labs(title = paste0(section1name,"-", names_map[partial])) +
        theme_economist() +
        coord_cartesian(ylim = c(
          min(c(section1[[partial]], section2[[partial]]), na.rm=TRUE),
          max(c(section1[[partial]], section2[[partial]]), na.rm=TRUE)
        ))   # Set y-axis limits
      
      p2 <- ggplot(section2[section2$tier == tier,], aes_string(x = "tier", y = partial, color = "tier")) + 
        geom_boxplot(outlier.shape = NA) +
        geom_jitter(width = 0.1, alpha = 0.5) +
        scale_color_manual(values = tier_colors) +
        labs(title = paste0(section2name,"-", names_map[partial])) +
        theme_economist() +
        coord_cartesian(ylim = c(
          min(c(section1[[partial]], section2[[partial]]), na.rm=TRUE),
          max(c(section1[[partial]], section2[[partial]]), na.rm=TRUE)
        ))   # Set y-axis limits
      
      # Combine the boxplots and save as a PNG
      combined_plot <- grid.arrange(p1, p2, ncol=2)
      ggsave(filename = paste0("plots/", names_map[partial], "_Tier_", tier, "_boxplots.png"), plot = combined_plot)
    }
  }
}


boxplot_partials_tiers_0_100 <- function (section1, section2, section1name, section2name){
  # Vector of column names
  partials <- c('P1.After.Comments', 'P2.After.Comments',
                'P3.After.Comments', 'Number.Grade')
  
  partials_short <- c('P1', 'P2', 'P3', 'Final')
  
  names_map <- setNames(partials_short, partials)
  
  # Define color mapping for each tier
  tier_colors <- c("A" = "blue", "B" = "green", "C" = "red")
  
  # For each partial
  for (partial in partials){
    # Assign tiers to each section
    section1$tier <- assign_tiers(section1[[partial]])
    section2$tier <- assign_tiers(section2[[partial]])
    
    # For each tier
    for (tier in c('A', 'B', 'C')){
      # Create a boxplot for each section for each tier
      p1 <- ggplot(section1[section1$tier == tier,], aes_string(x = "tier", y = partial, color = "tier")) + 
        geom_boxplot(outlier.shape = NA) +
        geom_jitter(width = 0.1, alpha = 0.5) +
        scale_color_manual(values = tier_colors) +
        labs(title = paste0(section1name,"-", names_map[partial])) +
        theme_economist() +
        coord_cartesian(ylim = c(0,100))   # Set y-axis limits
      
      p2 <- ggplot(section2[section2$tier == tier,], aes_string(x = "tier", y = partial, color = "tier")) + 
        geom_boxplot(outlier.shape = NA) +
        geom_jitter(width = 0.1, alpha = 0.5) +
        scale_color_manual(values = tier_colors) +
        labs(title = paste0(section2name,"-", names_map[partial])) +
        theme_economist() +
        coord_cartesian(ylim = c(0,100))   # Set y-axis limits
      
      # Combine the boxplots and save as a PNG
      combined_plot <- grid.arrange(p1, p2, ncol=2)
      ggsave(filename = paste0("plots/large_", names_map[partial], "_Tier_", tier, "_boxplots.png"), plot = combined_plot)
    }
  }
}