#' Fitting Linear Models
#'
#' Used to fit linear models. It can be used to carry out regression,
#'   single stratum analysis of variance and analysis of covariance.
#'
#' @param formula: a formula class object, similar to lm().
#' @param data: input data frame.
#'
#' @keywords prediction & inference
#'
#' @return table similar to the coefficient table from summary()
#'   with rows for each coefficient (including the (Intercept)!)
#'   and columns for the Estimate, Std. Error, t value, and Pr(>|t|).
#'   There should be row and column names.
#'
#' @examples
#' my_lm(mpg ~ hp + wt, data = mtcars)
#'
#' @export
my_lm <- function(formula, data) {
  # create a matrix  with the relevant variables
  x <- model.matrix(formula, data)
  # create a data frame with relevant variables
  z <- model.frame(formula, data)
  # create a value set
  y <- model.response(z)
  # find the beta value and the intercept
  beta <- solve(t(x) %*% x) %*% t(x) %*% y
  # find the degree of freedom
  df_lm <- nrow(x) - ncol(x)
  # calculate the sigma value
  sigma_sq <- sum((y-(x %*% beta))^2 / df_lm)
  # find the standard error
  se <- abs(sqrt(diag(sigma_sq * (solve(t(x) %*% x)))))
  # calculate the t-value
  t_val <- beta / se
  # find the p-value
  p_val_lm <- pt(abs(t_val) * 2, df_lm, lower.tail = FALSE)
  # combine the beta, standard error, t-value, and p-value into one matrix
  my_mat <- cbind(beta, se, t_val, p_val_lm)
  # rename the columns
  colnames(my_mat) <- c('Estimate', 'Std.error', 't_value', 'Pr(>|t|)')
  # change it to a table format
  my_table <- as.data.frame(my_mat)
  # return the value
  return(my_table)
}
