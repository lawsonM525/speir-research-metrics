### Funciton to get scores for each tier in a partial

tier_scores <- function(partial, tier){
    # Get the scores for each tier
    tier_scores <- partial %>%
        filter(tier == tier) %>%
        select(score)
    
    # Return the scores
    return(tier_scores)
}

# Return tier scores as a data frame
tiers_scores <- function(partial) {

  # Get scores for each tier
  tierA <- tier_scores(partial, "A") 
  tierB <- tier_scores(partial, "B")
  tierC <- tier_scores(partial, "C")
  
  # Combine into single data frame
  tiers_df <- rbind(tierA, tierB, tierC)
  
  # Add tier column
  tiers_df$tier <- c(rep("A", nrow(tierA)), 
                     rep("B", nrow(tierB)),
                     rep("C", nrow(tierC)))
                     
  # Return data frame
  return(tiers_df)
  
}