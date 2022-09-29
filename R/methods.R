#' Estimated treatment effect and
#' lower and upper confidence interval limits
#' using the Non Parametric Response Ratio method
#'
#' For one experiment or study the individual
#' effect is calculated, for more than one
#' the overall effect is calculated.
#'
#' While variance is not required for this method,
#' it is still needed to generate the samples.
#'
#' @param sample_size Number of observations
#' @param mean_e Estimated mean in experimental group
#' @param mean_c Estimated mean in control group
#' @param sd_e Standard deviation (for sample generation purposes)
#' @param n_experiments Number of experiments
#' @param distribution Distribution used to generate sample
#'
#' @return List of: effect size, lower and upper intervals
#' @export
#'
#' @examples
#' non_parametric_rr_meta(10, 6, 5, 3, 3)
non_parametric_rr_meta <- function(sample_size,
                                   mean_e,
                                   mean_c,
                                   sd_e,
                                   n_experiments,
                                   distribution = "normal"){

  e <- list()
  c <- list()

  samples <- sample_generator(n_experiments,
                              sample_size,
                              mean_e,
                              sd_e,
                              mean_c,
                              sd_c,
                              distribution)
  e <- samples[[1]]
  c <- samples[[2]]


  overall_sample_size <- rep(sample_size, n_experiments)

  overall_mean_e <- sapply(e, mean)
  overall_mean_c <- sapply(c, mean)
  overall_sd_e   <- sapply(e, sd)
  overall_sd_c   <- sapply(c, sd)

  # d <-

  result <- list(d$TE.fixed, d$lower.fixed, d$upper.fixed)

}

#' Estimated treatment effect and
#' lower and upper confidence interval limits
#' using the Statistical Vote Counting method
#'
#' For one experiment or study the individual
#' effect is calculated, for more than one
#' the overall effect is calculated.
#'
#' While variance is not required for this method,
#' it is still needed to generate the samples.
#'
#' @param sample_size Number of observations
#' @param mean_e Estimated mean in experimental group
#' @param mean_c Estimated mean in control group
#' @param sd_e Standard deviation (for sample generation purposes)
#' @param n_experiments Number of experiments
#' @param distribution Distribution used to generate sample
#'
#' @return List of effect size, lower and upper intervals
#' @export
#'
#' @examples
#' vote_counting(10, 6, 5, 3, 3)
vote_counting <- function(sample_size,
                           mean_e,
                           mean_c,
                           sd_e,
                           n_experiments,
                           distribution = "normal"){

  e <- list()
  c <- list()

  samples <- sample_generator(n_experiments,
                              sample_size,
                              mean_e,
                              sd_e,
                              mean_c,
                              sd_c,
                              distribution)
  e <- samples[[1]]
  c <- samples[[2]]


  overall_sample_size <- rep(sample_size, n_experiments)

  overall_mean_e <- sapply(e, mean)
  overall_mean_c <- sapply(c, mean)
  overall_sd_e   <- sapply(e, sd)
  overall_sd_c   <- sapply(c, sd)

  # d <-

  result <- list(d$TE.fixed, d$lower.fixed, d$upper.fixed)

}
