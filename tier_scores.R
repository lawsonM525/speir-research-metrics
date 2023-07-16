### Funciton to get scores for each tier in a partial

tier_scores <- function(partial, tier){
    # Get the scores for each tier
    tier_scores <- partial %>%
        filter(tier == tier) %>%
        select(score)
    
    # Return the scores
    return(tier_scores)
}

## Calling tier scores function for each tier given a partial
tiers_scores <- function(partial){
    # Get scores for each tier
    tierA <- tier_scores(partial, "A")
    tierB <- tier_scores(partial, "B")
    tierC <- tier_scores(partial, "C")

    # Return the scores
    return(list(tierA, tierB, tierC))
}