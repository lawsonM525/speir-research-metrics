library(dplyr)
library(ggplot2)

calc_gains <- function(section, sectionname){
  
  # Calculate section bonuses
  sec_bonus <- section %>% select(P1.Real,P1.New, P2.Real,P2.New)
  
  # Cleaning
  # s2_bonus <- s2_bonus[-1,]
  
  
  calc_gain <- function(real, new){
    return(((new - real)/real)*100)
  }
  
  sec_bonus$p1.gain <- calc_gain(sec_bonus$P1.Real, sec_bonus$P1.New)
  sec_bonus$p2.gain <- calc_gain(sec_bonus$P2.Real, sec_bonus$P2.New)
  
  ## Means
  print(mean(sec_bonus$p1.gain, na.rm=TRUE))
  print(median(sec_bonus$p1.gain, na.rm=TRUE))
  print(mean(sec_bonus$p2.gain))
  print(median(sec_bonus$p2.gain))
  
  
  ## Distributions
  
  p <- ggplot(sec_bonus) +
    geom_density(aes(x = sec_bonus$p1.gain), fill = "blue", alpha = 0.5) +
    geom_density(aes(x = sec_bonus$p2.gain), fill = "red", alpha = 0.5) +
    labs(title = paste0("Density plot of",sectionname," bonus gains on P1+(blue) & P2+(red)"), x = "Gained Points", y = "Density") +
    theme_minimal()+
    xlim(0,70)
  
  ggsave(paste0("plots/",sectionname,"_gains.png"), p)
  
  return(sec_bonus %>% select(p1.gain, p2.gain))
}