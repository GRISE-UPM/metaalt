#' Estimated treatment effect and
#' lower and upper confidence interval limits
#' using the Non Parametric Response Ratio method
#'
#' @param sample_size Number of observations
#' @param mean_e Estimated mean in experimental group
#' @param mean_c Estimated mean in control group
#' @param n_experiments Number of experiments
#'
#' @return List of: effect size, lower and upper limits of the confidence interval
#' @export
meta_nprr <- function(sample_size_e,
                                   mean_e,
                                   sample_size_c,
                                   mean_c,
                                   log_transformed = TRUE,
                                   alpha_level = 0.05){

  estimated_variance <- (sample_size_e + sample_size_c) / (sample_size_e * sample_size_c)
  rr <- mean_e / mean_c
  if(log_transformed)
    rr <- log(rr)

  global_rr <- sum(rr * estimated_variance) / sum(estimated_variance)
  left_limit_confidence_interval <- global_rr - qnorm(1 - alpha_level / 2) * sqrt(sum(estimated_variance))
  right_limit_confidence_interval <- global_rr + qnorm(1 - alpha_level / 2) * sqrt(sum(estimated_variance))

  if(log_transformed) {
    global_rr <- exp(global_rr)
    left_limit_confidence_interval <- exp(left_limit_confidence_interval)
    right_limit_confidence_interval <- exp(right_limit_confidence_interval)
  }

  return <- list(TE = global_rr,
                 lower = left_limit_confidence_interval,
                 upper = right_limit_confidence_interval)

}

#' Estimated treatment effect and
#' lower and upper confidence interval limits
#' using the Statistical Vote Counting method

#' @param sample_size Number of observations
#' @param mean_e Estimated mean in experimental group
#' @param mean_c Estimated mean in control group
#' @param n_experiments Number of experiments
#'
#' @return List of: effect size, lower and upper limits of the confidence interval
#' @export
vote_counting <- function(sample_size_e,
                          mean_e,
                          sample_size_c,
                          mean_c,
                          alpha_level = 0.05){

}
