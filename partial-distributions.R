library(ggplot2)

distributions <- function(section, section_name, p1, p2, p3, p4){
  # Traditional MCQ partial grade distributions
  p1plot <- ggplot(section, aes(p1)) + 
    geom_density(fill="blue", alpha=0.5) +
    labs(x = "Partial 1 Scores", y = "Density", 
         title = paste0(section_name," - Partial 1")) +
    xlim(0,100)
  ggsave(filename = paste0("plots/",section_name, "_partial_1.png"), 
         plot = p1plot)
  
  p2plot <- ggplot(section, aes(p2)) + 
    geom_density(fill="blue", alpha=0.5) +
    labs(x = "Partial 2 Scores", y = "Density", 
         title = paste0(section_name," - Partial 2")) +
    xlim(0,100)
  ggsave(filename = paste0("plots/",section_name, "_partial_2.png"), 
         plot = p2plot)
  
  p3plot <- ggplot(section, aes(p3)) + 
    geom_density(fill="blue", alpha=0.5) +
    labs(x = "Partial 3 Scores", y = "Density", 
         title = paste0(section_name," - Partial 3")) +
    xlim(0,100)
  ggsave(filename = paste0("plots/",section_name, "_partial_3.png"), 
         plot = p3plot)
  
  finalplot <- ggplot(section, aes(p4)) + 
    geom_density(fill="blue", alpha=0.5) +
    labs(x = "Final Scores", y = "Density", 
         paste0(section_name," - Final Grade")) +
    xlim(0,100)
  ggsave(filename = paste0("plots/", section_name, "_final.png"), 
         plot = finalplot)
}