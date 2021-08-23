#' t-Test Function
#'
#' Performs one and two sample t-tests on vectors of data.
#'
#' @param x A numeric vector of data.
#' @param alternative a character string specifying the alternative hypothesis.
#'   This should only accept "two.sided", "less", or "greater".
#'   Otherwise, your function should throw an informative error.
#' @param mu a number indicating the null hypothesis value of the mean.
#' @keywords inference
#'
#' @return test_stat: the numeric test statistic.
#' @return df: the degrees of freedom.
#' @return alternative: the value of the \code{alternative}.
#' @return p_val: the numeric p-value.
#'
#' @examples
#' my_t.test(y, alternative = "two.sided", 0)
#' my_t.test(y, alternative = "lass", 0)
#'
#' @export
my_t.test <- function(x, alternative, mu) {
  # find the standard error of the vector x
  standard_error <- sd(x)/ sqrt(length(x))
  # calculate the t value
  test_stat <- (mean(x) - mu)/ standard_error
  # find the degree of freedom
  df_t <- length(x) - 1

  if (alternative == "two.sided") {
    # two sided situation
    p_val <- pt(abs(test_stat), df_t, lower.tail = FALSE)
    p_val <- p_val * 2
  } else if (alternative == "less") {
    # left sided
    p_val <- pt(test_stat, df_t, lower.tail = TRUE)
  } else if (alternative == "greater") {
    # right sided
    p_val <- pt(abs(test_stat), df_t, lower.tail = FALSE)
  } else {
    # only accept "two.sided", "less", or "greater"
    stop("Only accept 'two.sided', 'less', or 'greater'.")
  }
  # create a list for the final output
  my_list <- list(test_stat, df_t, alternative, p_val)
  return(my_list)
}
