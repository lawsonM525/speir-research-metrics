triple_plot <- function(P1, P2, P3, sectionname){
  
  for (variable in c("mcq_points", "justification_points","j_gain")) {
    p1_value <- P1[, variable]
    p2_value <- P2[, variable]
    p3_value <- P3[, variable]
    
    if(length(p2_value) < length(p1_value)) {
      p2_value <- c(p2_value, NA)
    }
    
    if(length(p3_value) < length(p1_value)) {
      p3_value <- c(p3_value, NA)
    }
    
    
    df_combined <- data.frame(P1 = p1_value, P2 = p2_value, P3 = p3_value)
    
    p <- ggplot(df_combined) +
      geom_density(aes(x = P1), fill = "blue", alpha = 0.5) +
      geom_density(aes(x = P2), fill = "red", alpha = 0.5) +
      geom_density(aes(x = P3), fill = "green", alpha = 0.5) +
      labs(title = paste0(sectionname, ": Density plot of ", variable, " on P1 (blue), P2(red) and P3(green)"), x = variable, y = "Density") +
      theme_minimal()
    
    ggsave(paste0("plots/", sectionname, "_", variable, "_combined.png"), p)
  }
}