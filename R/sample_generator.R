#' Generate samples. This method supports multiple experiments or studies,
#' for each of them generates a sample of given expentancy and
#' variance. The generation follows a normal distribution by default,
#' but it may be specified to use a lognormal or truncated distribution.
#'
#' @param n_experiments Number of experiments
#' @param sample_size Number of samples in each experiment
#' @param mean_e Expentancy of experiment group
#' @param sd_e Variance of experiment group
#' @param mean_c Expentancy of control group
#' @param sd_c Variance of control group
#' @param distribution Distribution used to generate sample. "normal" by
#'  default, options "truncated" and "lognormal"
#'
#' @return a list of experiment group sample and control group sample
#' @export
#'
#' @examples sample_generator(5,40,6,3.5,4,3.5,"lognormal")
sample_generator <- function(n_experiments,
                             sample_size,
                             mean_e,
                             sd_e,
                             mean_c,
                             sd_c,
                             distribution = "normal"){

  e <- list()
  c <- list()

  if (distribution == "normal") {
    for (i in 1:n_experiments) {
      e <- c(e, list(stats::rnorm(sample_size, mean_e, sd_e)))
      c <- c(c, list(stats::rnorm(sample_size, mean_c, sd_e)))
    }
  } else if (distribution == "lognormal") {
    for (i in 1:n_experiments) {
      e <- c(e, list(stats::rlnorm(sample_size, mean_e, sd_e)))
      c <- c(c, list(stats::rlnorm(sample_size, mean_c, sd_e)))
    }
  } else if (distribution == "truncated") {
    for (i in 1:n_experiments) {
      e <- c(e, list(EnvStats::rnormTrunc(sample_size, mean_e, sd_e, min = 0)))
      c <- c(c, list(EnvStats::rnormTrunc(sample_size, mean_e, sd_e, min = 0)))
    }
  }

  list(e, c)
}
