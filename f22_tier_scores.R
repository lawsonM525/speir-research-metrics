### Function to get scores for each tier in a partial

tier_scores <- function(partial, tier){
  # Get the scores for each tier
  tier_scores <- partial %>%
    filter(tier == tier) %>%
    select(P1.Score)
  
  # Return the scores
  return(tier_scores)
}


# Return tier scores as a data frame
tiers_scores <- function(partial) {
  tiers_df <- data.frame()
  
  # Get scores for each tier
  tiers_df$tierA <- tier_scores(partial, "A") 
  tiers_df$tierB <- tier_scores(partial, "B")
  tiers_df$tierC <- tier_scores(partial, "C")
  
  # Return data frame
  return(tiers_df)
  
}