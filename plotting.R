# Density plot
densities_single <- function(data, section_name) {
  
  p <- ggplot(data, aes(x=P1.Total)) +
    geom_density() +
    labs(title=section_name, 
         x="Total Score",
         y="Density")
  
  # Construct file name 
  filename <- paste0("plots/F22-", section_name, "-density.png")
  
  # Save plot
  ggsave(filename, plot=p, width=6, height=4)
  
}

# Boxplot with jitter and color
boxplot_single <- function(sec1, sec2, sec1_name, sec2_name) {
  
  # Create data frame
  data <- data.frame(
    score = c(sec1$P1.Score, sec2$P1.Score),
    section = factor(c(rep(sec1_name, nrow(sec1)),  
                       rep(sec2_name, nrow(sec2))),
                     levels = c(sec1_name, sec2_name))
  )
  
  # Define colors
  colors <- c(sec1_name = "blue", sec2_name = "red") 
  
  # Plot
  p <- ggplot(data, aes(x = section, y = score, color = section)) +
    geom_boxplot() + 
    geom_jitter(width = 0.2, alpha = 0.4) +
    scale_color_manual(values = setNames(colors, levels(data$section))) +
    ggtitle("Section Boxplot") +
    theme_minimal()
  
  # File name
  filename <- paste0("plots/F22-P1-boxplot.png")
  
  # Save plot
  ggsave(filename, plot = p, width = 6, height = 4)
  
}