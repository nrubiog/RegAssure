#' Check Linear Regression Model Assumptions
#'
#' @description
#' This function verifies assumptions for various linear regression models, including OLS, WLS, censored, and truncated models.
#'
#' @param model Object of class 'lm'. The fitted linear regression model.
#' @param m Number of digits to round the results to. Default is NULL.
#'
#' @return A list containing the results of assumption checks.
#'
#' @importFrom lmtest bptest dwtest
#'
#' @importFrom car vif durbinWatsonTest
#'
#' @importFrom stats shapiro.test ks.test sd
#'
#' @export


check_lm_assumptions <- function(model, m = NULL){

  m = m # m = Número de decimales para redondear

  # Linearity
  mean_residuals <- mean(model$residuals)
  if (!is.null(m)) {
    cat("Linearity:\n Mean of residuals: ", round(mean_residuals, m), "\n")
  } else{
    cat("Linearity:\n Mean of residuals: ", mean_residuals, "\n")
  }
  cat("\n")

  # Homoscedasticity
  bptest_result <- bptest(model)
  if (!is.null(m)) {
    cat("Homoscedasticity: Breusch-Pagan Test:\n p-value: ", round(bptest_result$p.value, m), "\n")
  } else{
    cat("Homoscedasticity:\n p-value: ", bptest_result$p.value, "\n")
  }
  cat("\n")

  # Independence

  if (!is.null(model$weights)) {
    dwtest_result <- durbinWatsonTest(model)
    if (!is.null(m)){
      cat("Independence:\n Durbin-Watson test statistic: ", round(dwtest_result$dw, m), "\n")
    } else {
      cat("Independence:\n Durbin-Watson test statistic: ", dwtest_result$dw, "\n")
    }
  } else {
    dwtest_result <- dwtest(model, alternative = "two.sided", iterations = 1000)
    if (!is.null(m)) {
      cat("Independence:\n p-value: ", round(dwtest_result$p.value, m),
          "\n Durbin-Watson test statistic: ", round(dwtest_result$statistic, m), "\n")
    } else {
      cat("Independence:\n p-value: ", dwtest_result$p.value,
          "\n Durbin-Watson test statistic: ", dwtest_result$statistic, "\n")
    }
  }
  cat("\n")

  # Normality

  if (length(model$residuals) < 50) {
    normality_result <- shapiro.test(model$residuals)
    if (!is.null(m)) {
      cat("Normality:\n Shapiro-Wilk test:\n  p-value: ", round(normality_result$p.value, m),
          "\n  W-statistic: ", round(normality_result$statistic, m), "\n")
    } else {
      cat("Normality:\n Shapiro-Wilk test:\n  p-value: ", normality_result$p.value,
          "\n  W-statistic: ", normality_result$statistic, "\n")
    }
  } else {
    normality_result <- ks.test(model$residuals, "pnorm", mean = mean(model$residuals), sd = sd(model$residuals))
    if (!is.null(m)) {
      cat("Normality:\n Kolmogorov-Smirnov test:\n  p-value: ", round(normality_result$p.value, m), "\n")
    } else {
      cat("Normality:\n Kolmogorov-Smirnov test:\n  p-value: ", normality_result$p.value, "\n")
    }
  }
  if (anyDuplicated(model$residuals)) {
    cat("Normality:\n Ties detected. Using Shapiro-Wilk test instead of Kolmogorov-Smirnov.\n")
    normality_result <- shapiro.test(model$residuals)
    if (!is.null(m)) {
      cat("Normality:\n Shapiro-Wilk test:\n  p-value: ", round(normality_result$p.value, m),
          "\n  W-statistic: ", round(normality_result$statistic, m), "\n")
    } else {
      cat("Normality:\n Shapiro-Wilk test:\n  p-value: ", normality_result$p.value,
          "\n  W-statistic: ", normality_result$statistic, "\n")
    }
    if (length(model$residuals) < 3 || length(model$residuals) > 5000) {
      cat("Normality:\n Sample size outside the range for normality tests.\n")
      cat("  Please inspect residuals visually for normality.\n")
    }
  }
  cat("\n")

  # Multicollinearity

  vif_result <- vif(model)
  if (!is.null(m)) {
    cat("Multicollinearity: Variance Inflation Factor:\n")
        print(round(vif_result, m))
  } else {
    cat("Multicollinearity: Variance Inflation Factor:\n")
        print(vif_result)
  }
  cat("\n")

    # Crear una lista con los resultados de los tests
    results_list <- list(
      linearity = mean_residuals,
      homoscedasticity = bptest_result,
      independence = dwtest_result,
      normality = normality_result,
      multicollinearity = vif_result
    )

    # Mensaje en inglés
    cat("The assumption tests have been completed and the results are available in a list.\n")
    cat("Enjoy it\n")

    # Mensaje en español
    cat("Las pruebas de supuestos han sido completadas y los resultados estan disponibles en una lista.\n")
    cat("Disfrutalo\n\n")

    cat("The list of results will be displayed below:\n")

  return(results_list)
}
