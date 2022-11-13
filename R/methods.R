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
                      alpha_level = 0.05) {
  estimated_variance <-
    (sample_size_e + sample_size_c) / (sample_size_e * sample_size_c)
  rr <- mean_e / mean_c
  if (log_transformed)
    rr <- log(rr)

  global_rr <-
    sum(rr * estimated_variance) / sum(estimated_variance)
  left_limit_confidence_interval <-
    global_rr - qnorm(1 - alpha_level/2) * sqrt(sum(estimated_variance))
  right_limit_confidence_interval <-
    global_rr + qnorm(1 - alpha_level/2) * sqrt(sum(estimated_variance))

  if (log_transformed) {
    global_rr <- exp(global_rr)
    left_limit_confidence_interval <-
      exp(left_limit_confidence_interval)
    right_limit_confidence_interval <-
      exp(right_limit_confidence_interval)
  }

  return <- list(TE = global_rr,
                 lower = left_limit_confidence_interval,
                 upper = right_limit_confidence_interval)

}

#' Estimated treatment effect and
#' lower and upper confidence interval limits
#' using the Statistical Vote Counting method
#'
#' The approach used is based on the large sample
#' approximation of the binomial distribution. See
#' Hedges & Olkin (1983), Chapter 4, section C.1.
#'
#' Later, the chi-square approximation and the 2-tailed
#' tests will be added.

#' @param common_sample_size Number of observations. All experiments should have the same number
#' @param significant_differences Boolean (TRUE or FALSE). Whether significant differences have been obtained
#'
#' @return List of: effect size, lower and upper limits of the confidence interval
#' @export
vote_counting <- function(common_sample_size,
                          significant_differences,
                          alpha_level = 0.05,
                          test = "one-tailed",
                          method = "normal") {

  number_of_studies <- length(significant_differences)

  proportion_of_significant_differences <-
    sum(significant_differences[significant_differences == TRUE]) /
    number_of_studies

  critical_value = qnorm(1 - alpha_level/2)

  variance <-
    proportion_of_significant_differences * (1 - proportion_of_significant_differences) /
    number_of_studies

  left_limit_confidence_interval_not_transformed <-
    proportion_of_significant_differences -
    critical_value * sqrt(variance)

  right_limit_confidence_interval_not_transformed <-
    proportion_of_significant_differences -
    critical_value * sqrt(variance)

  proportion_of_significant_differences_transformed <- trasnform(proportion_of_significant_differences)
  left_limit_confidence_interval_transformed <- trasnform(left_limit_confidence_interval_not_transformed)
  right_limit_confidence_interval_transformed <- trasnform(rigth_limit_confidence_interval_not_transformed)

  return <- list(TE = proportion_of_significant_differences_transformed,
                 lower = left_limit_confidence_interval_transformed,
                 upper = right_limit_confidence_interval_transformed)
}

#' Calculate the effect size using Table 2 of
#' Hedges & Olkin (1983), Chapter 4.
#'
#' @param proportion A proportion obtained in the vote_counting function
#'
#' @return The corresponding effect size
transform <- function(proportion) {

}
