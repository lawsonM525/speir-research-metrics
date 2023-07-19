# Density plot
densities_single <- function(data, section_name) {
  
  p <- ggplot(data, aes(x=P1.Total)) +
    geom_density() +
    labs(title="Density Plot", 
         x="Total Score",
         y="Density")
  
  # Construct file name 
  filename <- paste0("plots/F22-", section_name, "-density.png")
  
  # Save plot
  ggsave(filename, plot=p, width=6, height=4)
  
}

# Boxplot 
boxplot_single <- function(sec1, sec2, sec1_name, sec2_name) {
  
  # Create a data frame
  data <- data.frame(
    score = c(sec1$P1.Total, sec2$P1.Total), 
    section = factor(c(rep(sec1_name, nrow(sec1)),
                       rep(sec2_name, nrow(sec2))),
                     levels = c(sec1_name, sec2_name))
  )
  
  # Plot
  p <- ggplot(data, aes(x = section, y = score)) +
    geom_boxplot() +
    ggtitle("Section Boxplot") + 
    theme_minimal()
  
  # File name 
  filename <- paste0("plots/F22-P1-boxplot.png")
  
  # Save plot
  ggsave(filename, plot = p, width = 6, height = 4)
  
}