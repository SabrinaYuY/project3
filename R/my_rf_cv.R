#' Random Forest Function
#'
#' This function would use random forest algorithm to predict quantitive output
#'
#' @param k Numeric input indicates number of folds.
#' @keywords prediction
#'
#' @return a numeric representing the mean standard error across all the folds.
#'
#' @examples
#' my_rf_cv(5)
#' my_rf_cv(10)
#'
#' @export
my_rf_cv <- function(k) {

  # select the columns that needed
  clean <- na.omit(penguins)
  data_peng <- clean[, c("bill_length_mm", "bill_depth_mm",
                         "flipper_length_mm", "body_mass_g")]
  test <- clean[, c("species")]

  # Split data in k_cv parts, randomly
  fold <- sample(rep(1:k, length = nrow(data_peng)))
  predictions <- c()
  model <- c()
  # Iterate through the folds
  for (i in 1:k) {
    # split train data by folds
    train_trainfold <- as.data.frame(data_peng[fold != i, ])
    train_valfold <- as.data.frame(data_peng[fold == i, ])

    # model the data of trainfold
    model1 <- randomForest(body_mass_g ~ bill_length_mm + bill_depth_mm +
                             flipper_length_mm, data = train_trainfold,
                           ntree = 100)

    # model the data of valfold
    model2 <- randomForest(body_mass_g ~ bill_length_mm + bill_depth_mm +
                             flipper_length_mm, data = train_valfold,
                           ntree = 100)

    # predict the body mass for trainfold
    predictions1 <- predict(model1, train_trainfold[, -4])
    # calculate the difference for trainfold
    difference1 <- (predictions1 - train_trainfold[, 4])^2

    # predict the body mass for valfold
    predictions2 <- predict(model2, train_valfold[, -4])
    # calculate the difference for valfold
    difference2 <- (predictions2 - train_valfold[, 4])^2
  }
  # sum the difference
  difference <- sum(difference1, difference2)
  # calculate the MSE
  mse <- difference/ (length(difference1) + length(difference2))
  return(mse)
}
