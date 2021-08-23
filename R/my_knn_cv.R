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
my_knn_cv <- function(train, cl, k_nn, k_cv){
  # Split data in k_cv parts, randomly
  fold <- sample(rep(1:k_cv, length = nrow(train)))
  misclassification <- c()

  # Iterate through the folds
  for (i in 1:k_cv) {
    predictions <- NULL

    # split train data by folds
    train_trainfold <- as.data.frame(train[fold != i, ])
    train_valfold <- as.data.frame(train[fold == i, ])

    # split testing data by folds
    test_trainfold <- as.data.frame(cl[fold != i, ])[,1]
    test_valfold <- as.data.frame(cl[fold == i, ])[,1]

    # predict the classes for this fold
    predictions <- knn(train = train_trainfold, test = train_valfold,
                       cl = test_trainfold, k = k_nn)

    # store the errors for each fold
    misclassification[i] <- sum(predictions != test_valfold) /
      length(test_valfold)
  }
  # calculate the mean cv error
  avg_cverr <- mean(misclassification)
  # put the class and the cv_err together
  cvvlist <- list(predictions, "cv_err" = avg_cverr)
  return(cvvlist)
}
