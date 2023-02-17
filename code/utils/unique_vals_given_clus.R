unique_vals_given_clus <- function(unique_vals_chain, clus_alloc_chain, best_clus)
{
    vals <- c()
    
    # cast to matrices the eventual dataframes
    if(typeof(unique_vals_chain) == "list")
    {
      nrow_ = dim(unique_vals_chain)[1]
      ncol_ = dim(unique_vals_chain)[2]
      unique_vals_chain <- matrix(unlist(unique_vals_chain), nrow=nrow_,ncol=ncol_, byrow = F)
    }

    for (h in unique(best_clus)) {
      
      data_idx <- which(best_clus == h)
      uniq_vals_idx <- clus_alloc_chain[, data_idx]
      
      clus_card <- length(data_idx)
      
      means_by_iter <- matrix(nrow = nrow(clus_alloc_chain), ncol = clus_card)
      
      for (i in 1:nrow(clus_alloc_chain)) {
        for (j in 1:length(data_idx)) {
          means_by_iter[i, j] <- unique_vals_chain[i, clus_alloc_chain[i, j]+1] / clus_card
        }
      }
      
      avg_mean_by_iter <- rowSums(means_by_iter)
      muhat <- mean(avg_mean_by_iter)
      vals <- c(vals, muhat)
    }
    
    return(vals)
}
