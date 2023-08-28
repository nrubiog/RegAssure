#' Get Predictions and Errors
#'
#' @description
#' This function generates predictions from a model and calculates the errors between actual and predicted values, resulting in a dataframe containing the predicted values, actual values, and the residuals.
#'
#' @param modelo Fitted model object
#' @param datos Data frame containing predictor variables.
#' @param real Vector of actual response values.
#' @param n Number of digits to round the columns. Default
#'
#' @return A data frame containing actual, predicted, and error values.
#' @export

get_predict <- function(modelo, datos, real, n = NULL) {

  predicciones <- predict(modelo, datos)
  actual <- data.frame(reales = real, predichos = predicciones)
  actual$Error <- actual$reales - actual$predichos

  # Redondear columnas si se especifica el número de decimales
  if (!is.null(n)) {
    actual$reales <- round(actual$reales, n)
    actual$predichos <- round(actual$predichos, n)
    actual$Error <- round(actual$Error, n)
  }

  return(actual)
}
