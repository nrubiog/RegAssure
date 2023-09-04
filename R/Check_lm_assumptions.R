#' Check Linear Regression Model Assumptions
#'
#' @description
#' This function verifies assumptions for various linear regression models, including OLS, WLS, censored, and trunmessageed models.
#'
#' @param model Object of class 'lm'. The fitted linear regression model.
#' @param studentize Argument of bptest function. Default is TRUE.
#' @param iterations Argument of dwtest function. Default is 1000.
#' @param exact Argument of ks.test function. Default is NULL.
#' @param simulate.p.value Argument of ks.test function. Default is FALSE.
#' @param simulate Argument of durbinWatsonTest function. Default is TRUE.
#' @param B Argument of ks.test function. Default is 2000.
#'
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


check_lm_assumptions <- function(model, studentize = TRUE, iterations = 1000, exact = NULL,
                                 simulate.p.value = FALSE, B = 2000, simulate = TRUE) {

  # Linearity

  linearity_result <- mean(model$residuals)

  linearity_msg <- suppressMessages(message("Linearity: \nMean of residuals", linearity_result))

  # Homoscedasticity


    bptest_result <- bptest(model, studentize = studentize)

    homoscedasticity_msg <- suppressMessages(message("Homocedasticity: \n
                              Alternative hypothesis: heteroscedasticity exists.\n", bptest_result))


  # Independence

  if (!is.null(model$weights)) {
    dwtest_result <- (durbinWatsonTest(model, simulate = simulate))

    independence_msg <- suppressMessages(message("Independence:\n
                            Durbin-Watson test: ", dwtest_result$alternative,"\n
                            p-value: ",dwtest_result$simulate,
                            "Statistic: ", dwtest_result$dw,"\n"))
    } else {
    dwtest_result <- dwtest(model, alternative = "two.sided", iterations = iterations)
    }

    independence_msg <- suppressMessages(message("Independence:\n
                            Durbin-Watson test: \n
                            Alternative hypothesis: true autocorrelation is not 0.\n
                            p-value: ",dwtest_result$p.value,
                            "Statistic: ", dwtest_result$statistic,"\n"))

  # Normalidad

  if (length(model$residuals) < 3 || length(model$residuals) > 5000) {
    normality_msg <- warning("Normality:\n sample size outside the range for normality tests.\n Please inspect residuals visually for normality.")

    } else if (length(model$residuals) < 50 || anyDuplicated(model$residuals)) {
      normality_result <- shapiro.test(model$residuals)

    normality_msg <- suppressMessages(message(
      "Normality: \n
      Shapiro-Wilk test:
      Alternative hypothesis: sample does not come from a normal distribution.
      p-value: ", normality_result$p.value,
      "W-statistic: ", normality_result$statistic))
    } else {
      normality_result <- ks.test(model$residuals, "pnorm", mean = mean(model$residuals), sd = sd(model$residuals), exact = exact, simulate.p.value = simulate.p.value, B = B)
        }
      normality_msg <- suppressMessages(message("Normality: Kolmogorov-Smirnov test:\n
                           Alternative hypothesis: sample does not come from a normal distribution.\n",
                           "p-value: ", normality_result$p.value))

  # Multicolinealidad

    vif_result <- vif(model)


  multicollinearity_msg <- suppressMessages(
    message("Multicollinearity: \n",
    "Variance Inflation Factor: \n",
    vif_result)
    )

  # Lista simple

  concise_results_list <- list(
    Linearity = linearity_result,
    Homoscedasticity = bptest_result,
    Independence = dwtest_result,
    Normality = normality_result,
    Multicollinearity = vif_result
  )

  message("\nThe assumption tests have been completed and the results are available in a list. Enjoy it :)\n")
  message("Las pruebas de supuestos han sido completadas y los resultados estan disponibles en una lista. Disfrutalo :)\n")

      return(concise_results_list)
  }

