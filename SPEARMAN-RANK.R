get_rank_distances <- function(pi_t1, pi_t0){
  # Returns the distance between two ordered lists.
  return(c(pi_t1 - pi_t0)**2)
}

discounted_rank_dissimilarity <- function(pi_t1, pi_t0){
  # Calculates the rank dissimilarity.
  numerator <- get_rank_distances(pi_t1, pi_t0)
  denominator <- pi_t1**2
  dists <- numerator/denominator
  d <- sum(dists)
  return(d)
}

get_max_distance <- function(nv){
  # Find the maximum distance.
  curr_max <- nv
  for(i in 1:nv){
    normal <- c(1:i); revers <- c(i:1)
    d <- discounted_rank_dissimilarity(normal,revers)
    if(d > curr_max){ curr_max <- d }
  }
  return(curr_max)
}

stopping_criteria <- function(GPredict_prev, GPredict_curr){
  # Stop when the procedure ceases to learn about the landscape, when comparing 
  # the global changes in mu between two successive iterations.
  prev_df <- data.frame(GPredict_prev$complete_data)
  curr_df <- data.frame(GPredict_curr$complete_data)
  prev_df$num <- c(1:dim(prev_df)[1])
  curr_df$num <- c(1:dim(curr_df)[1])
  pi_prev_sorted <- prev_df[order(prev_df$Y_hat),]
  pi_curr_sorted <- curr_df[order(curr_df$Y_hat),]
  pi_prev_rank <- pi_prev_sorted$num
  pi_curr_rank <- pi_curr_sorted$num
  numerator <- discounted_rank_dissimilarity(pi_curr_rank, pi_prev_rank)
  denominator <- get_max_distance(length(pi_curr_rank))
  rhoXv <- 1 - numerator/denominator
  return(rhoXv)
}	