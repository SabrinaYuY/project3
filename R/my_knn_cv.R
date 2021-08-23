#' k-Nearest Neighbors Cross-Validation
#'
#' Predict output class species using covariates
#'   bill_length_mm, bill_depth_mm, flipper_length_mm, and body_mass_g.
#' @param train: input data frame.
#' @param cl: true class value of your training data.
#' @param k_nn: integer representing the number of neighbors.
#' @param k_cv: integer representing the number of folds.
#'
#' @keywords prediction
#'
#' @return class: a vector of the predicted class Ŷ i for all observation.
#' @return cv_err: a numeric with the cross-validation misclassification error.
#'
#' @examples
#' my_knn_cv(data_peng, test, k_nn = 1, k_cv = 5)
#'
#' @export
my_knn_cv=function (train, cl, k_nn, k_cv) 
{
  if (is.data.frame(train) == FALSE) {
    stop("Input train is not a dataframe")
  }
  if (!is.numeric(c(k_nn, k_cv))) {
    stop("k_nn and k_cv are non-numeric")
  }
  fold <- sample(rep(1:k_cv, length = nrow(train)))
  data <- cbind(train, cl, fold)
  pred_class <- vector()
  cv_rate <- numeric()
  train_err <- numeric()
  for (i in 1:k_cv) {
    data_train <- data %>% filter(fold != i)
    data_test <- data %>% filter(fold == i)
    cl_train <- data_train[, 5]
    cl_test <- data_test[, 5]
    prediction_train <- knn(data_train[, 1:4], data_train[, 
                                                          1:4], cl_train, k_nn)
    prediction_test <- knn(data_train[, 1:4], data_test[, 
                                                        1:4], cl_train, k_nn)
    pred_class[fold == i] <- prediction_test
    cv_rate[i] <- mean(prediction_test != unlist(cl_test))
    train_err[i] <- mean(prediction_train != unlist(cl_train))
  }
  class <- cbind(data[, 1:4], as.data.frame(pred_class))
  cv_rate <- sum(cv_rate)/k_cv
  cv_err <- mean((pred_class - data$species)^2)
  train_err <- sum(train_err)/k_cv
  return(list(as.vector(class), cv_err, train_err))
}
