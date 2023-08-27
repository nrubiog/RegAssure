#' Check Assumptions for Logit Models
#'
#' @description
#' This function performs assumption checks for logistic regression models, including binary, multinomial, and ordered models. It conducts various tests to assess the validity of the model assumptions.
#'
#' @param logit_model Fitted logistic regression model.
#' @param data Dataframe containing predictor variables.
#' @param tipo_modelo Type of logistic regression model: ("binario", "multinomial", "ordenado", "binary", "multinomial", "ordered", "ordinal")
#' @param vars_numericas Numeric variables to be used in Box-Tidwell test. Default is NULL.
#' @param y Response variable for ROC test. Default is NULL.
#'
#' @return  A list containing the results of assumption checks.
#' @export

check_logit <- function(logit_model, data, tipo_modelo, vars_numericas = NULL, y = NULL) {

  # Convertir vars_numericas en un vector de nombres de variables

  if (!is.null(vars_numericas)) {
    if (is.character(vars_numericas)) {
      vars_numericas <- c(vars_numericas)
    } else if (is.list(vars_numericas)) {
      vars_numericas <- unlist(vars_numericas)
    }
  }

  # Prueba de linealidad (test de Box-Tidwell) para variables numericas continuas

  resultado_box_tidwell <- NULL

  if (!tipo_modelo %in% c("ordenada", "ordenado", "ordered", "ordinal")) {
    if (!is.null(vars_numericas) && length(vars_numericas) > 0) {
      # Crear una formula para la prueba de linealidad
      formula_linealidad <- logit_model$linear.predictors

      # Realizar la prueba de linealidad
      resultado_box_tidwell <- suppressWarnings(boxTidwell(formula_linealidad, data[, vars_numericas]))
    }
  } else {
    resultado_box_tidwell <- NULL
  }
  # Prueba de multicolinealidad

  if (length(coef(logit_model)) < 3) {
    vif_result <- NULL
  } else{
    vif_result <- vif(logit_model)
  }

  # Prueba de Brant para modelos logit ordenados
  prueba_brant <- function() {
    if (tipo_modelo %in% c("ordenada", "ordenado", "ordered", "ordinal")) {
      resultado_brant <- brant(logit_model)
    } else {
      return(NULL)
    }
  }

  # Prueba de ROC para modelos logit binomiales

  prueba_roc <- function() {
    if (tipo_modelo %in% c("binario", "binaria" ,"binomial")) {
      pred_logit <- predict(logit_model, type = "response")
      curva_roc <- roc(data[[y]], pred_logit, smooth = T)
      roc_resultado <- auc(curva_roc)
      return(roc_resultado)
    } else {
      return(NULL)
    }
  }
  # Matriz de confusion para los modelos logit binarios

  if (tipo_modelo %in% c("binario","binaria","binomial")) {

    matriz_confusion <- function() {
      if (tipo_modelo %in% c("binario", "binaria" ,"binomial")) {
        pred_logit <- predict(logit_model, type = "response")
        predicciones <- ifelse(pred_logit > 0.5, 1, 0)
        matriz_confusion_logit <- table(data[[y]], predicciones, dnn = c("Variable", "Predicciones"))
        return(matriz_confusion_logit)
      } else {
        return(NULL)
      }
    }
  }

  # Realizar las pruebas correspondientes segun el tipo de modelo

  if (tipo_modelo %in% c("binario", "binaria", "binomial")) {
    cat("\n")
    message("Tests performed for binary/binomial model.")
    cat("\n")

    # Linearity
    cat("Linearity: Box Tidwell test:\n")
    print(resultado_box_tidwell$result)
    cat("\n")

    # Accuracy using ROC Curve
    cat("Classification Accuracy (ROC Curve):\n")
    print(prueba_roc())
    cat("\n")

    # Accuracy using Confusion Matrix
    cat("Classification Accuracy (Confusion Matrix):\n")
    confusion_matrix <- matriz_confusion()
    print(confusion_matrix)

    # Multicollinearity
    if (is.null(vif_result)) {
      cat("Multicollinearity:\n\nVIF test cannot be applied since the model contains fewer than 2 independent variables.\n")
    } else {
      cat("Multicollinearity: VIF:\n")
      print(vif_result)
    }

  } else if (tipo_modelo %in% c("multinomial", "multinominal")) {
    cat("\n")
    message("Tests performed for multinomial model.")
    cat("\n")

    # Linearity
    cat("Linearity: Box Tidwell test:\n")
    print(resultado_box_tidwell$result)
    cat("\n")

    # Multicollinearity
    if (is.null(vif_result)) {
      cat("Multicollinearity:\n\nVIF test cannot be applied since the model contains fewer than 2 independent variables.\n")
    } else {
      cat("Multicollinearity: VIF:\n")
      print(vif_result)
    }

  } else if (tipo_modelo %in% c("ordenada", "ordenado", "ordered", "ordinal")) {
    cat("\n")
    message("Tests performed for ordinal model.")
    cat("\n")

    # Parallelism
    cat("Parallelism: Brant test:\n")
    cat("\n")
    print(prueba_brant())
    cat("\n")

    # Multicollinearity
    # Multicollinearity
    if (is.null(vif_result)) {
      cat("Multicollinearity:\n\nVIF test cannot be applied since the model contains fewer than 2 independent variables.\n")
    } else {
      cat("Multicollinearity: VIF:\n")
      print(vif_result)
    }

  } else {
    stop("Invalid model type. It must be 'binary', 'multinomial', or 'ordinal'.")
    stop("Tipo de modelo no valido. Debe ser 'binario', 'multinomial' u 'ordenado'.")
  }
  return(logit_model)
}
