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
#' @param auc roc function argument.
#' @param ci roc function argument.
#' @param ret roc function argument.
#'
#' @return  A list containing the results of assumption checks.
#' @export


check_logit <- function(logit_model, data, tipo_modelo, vars_numericas = NULL, y = NULL, auc = NULL, ci = NULL, ret = NULL) {

  # Convertir vars_numericas en un vector de nombres de variables

  if(is.null(vars_numericas)){
      cat("Linearity:\nBox Tidwell test cannot be applied since numeric variables have not been specified.")
      cat("\n\n")
  }

  if (!is.null(vars_numericas)) {
    if (is.character(vars_numericas)) {
      vars_numericas <- c(vars_numericas)
    } else if (is.list(vars_numericas)) {
      vars_numericas <- unlist(vars_numericas)
    }
  }

  # Prueba de linealidad (test de Box-Tidwell) para variables numericas continuas

  resultado_box_tidwell <- NULL

  tryCatch(
    {
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
    },
    error = function(e) {
      cat("Linearity: Box Tidwell test cannot be applied.\n")
      cat("Reason:",e$message, "\n")
      cat("\n")
    }
  )

  # Prueba de multicolinealidad

  vif_result <- NULL

  if(length(coef(logit_model)) >= 3) {

    design_matrix <- model.matrix(logit_model)

    # Identificar las columnas correspondientes a las variables independientes

    independent_vars <- design_matrix[, -1] # Excluir la columna de intercepto

    vars_names <- colnames(independent_vars)

    vars_names2 <- vars_names[vars_names %in% colnames(data)]

    numeric_var <- length(vars_names2)

    if (numeric_var >= 2) {

      is_num <- sapply(data[, vars_names2], is.numeric)

      numeric_indices <- which(is_num)

      is_num <- is_num[numeric_indices]
    } else {is_num <- NULL}

    # Verificar si al menos dos variables independientes son numéricas

    if(length(is_num) >=2) {
      vif_result <- vif(logit_model)
    } else {
      vif_result <- NULL
    }
  }

  # Prueba de Brant para modelos logit ordenados

  prueba_brant <- function() {
    if (tipo_modelo %in% c("ordenada", "ordenado", "ordered", "ordinal")) {
      resultado_brant <- brant(logit_model)
    } else {
      return(NULL)
    }
  }

  # Curva de ROC

  prueba_roc <- function() {
    if (tipo_modelo %in% c("binario", "binaria", "binomial")) {
      pred_logit <- predict(logit_model, type = "response")
      curva_roc <- NULL

      # Intentar calcular con smooth = TRUE
      tryCatch({
        curva_roc <- suppressMessages(roc(data[[y]], pred_logit, smooth = TRUE, auc = TRUE, ci = TRUE, ret = TRUE))
      }, error = function(err1) {
        if (grepl("ROC curve not smoothable", err1$message)) {
          cat("Warning: The curve cannot be smoothed with smooth = TRUE.\n")
          cat("Trying with smooth = FALSE.\n")
        } else {
          cat("\nFailed to calculate ROC curve with smooth = TRUE. Changing smooth to FALSE.\n")
          # Intentar calcular con smooth = FALSE
          curva_roc <- try(suppressMessages(roc(data[[y]], pred_logit,
                                                smooth = FALSE, auc = TRUE, ci = TRUE, ret = TRUE)), silent = TRUE)
        }
        return(curva_roc)
      }
      )}
  }

  # Matriz de confusion para los modelos logit binarios

      matriz_confusion <- function() {

        design_matrix <- model.matrix(logit_model)
        independent_vars <- design_matrix[, -1]
        vars_names <- colnames(independent_vars)
        vars_names2 <- vars_names[vars_names %in% colnames(data)]
        equal_row <- sapply(data[, vars_names2], length)

        matriz_confusion_logit <- NULL

          if (tipo_modelo %in% c("binario", "binaria" ,"binomial") && all(equal_row == equal_row[1])) {
            pred_logit <- predict(logit_model, type = "response")
            predicciones <- ifelse(pred_logit > 0.5, 1, 0)
            matriz_confusion_logit <- table(data[[y]], predicciones, dnn = c("Variable", "Predicciones"))
            return(matriz_confusion_logit)

          } else {
            return(matriz_confusion_logit)
          }
      }

  # Realizar las pruebas correspondientes segun el tipo de modelo

  if (tipo_modelo %in% c("binario", "binaria", "binomial")) {
    message("Tests performed for binary/binomial model.")
    cat("\n")

    # Linearity
    if (!is.null(vars_numericas) && !is.null(resultado_box_tidwell)) {
    cat("Linearity: Box Tidwell test:\n")
    print(resultado_box_tidwell$result)
    cat("\n")
    }

    # Multicollinearity
    if (is.null(vif_result)) {
      cat("Multicollinearity:\n\nVIF test cannot be applied since the model contains fewer than 2 independent numeric variables.\n")
      cat("\n")
    } else {
      cat("Multicollinearity: VIF:\n")
      print(vif_result)
      cat("\n")
    }

    # Accuracy using Confusion Matrix

    cat("Classification Accuracy (Confusion Matrix):\n")
    confusion_matrix <- matriz_confusion()
    if(!is.null(confusion_matrix)){
    print(confusion_matrix) } else {
    cat("All variables must have the same number of observations:\n")
    }
    cat("\n")

    # Accuracy using ROC Curve
    if(!is.null(prueba_roc)){
    cat("Classification Accuracy (ROC Curve):\n")
    print(prueba_roc())} else{
      cat("\nROC Curve cannot be calculated.\n")
    }
    cat("\n")

  } else if (tipo_modelo %in% c("multinomial", "multinominal")) {
    cat("\n")
    message("Tests performed for multinomial model.")
    cat("\n")

    # Linearity
    if (!is.null(vars_numericas) && !is.null(resultado_box_tidwell)) {
      cat("Linearity: Box Tidwell test:\n")
      print(resultado_box_tidwell$result)
      cat("\n")
    }

    # Multicollinearity
    if (is.null(vif_result)) {
      cat("Multicollinearity:\n\nVIF test cannot be applied since the model contains fewer than 2 independent numeric variables.\n")
      cat("\n")
    } else {
      cat("Multicollinearity: VIF:\n")
      print(vif_result)
      cat("\n")
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
    if (is.null(vif_result)) {
      cat("Multicollinearity:\n\nVIF test cannot be applied since the model contains fewer than 2 independent numeric variables.\n")
      cat("\n")
    } else {
      cat("Multicollinearity: VIF:\n")
      print(vif_result)
      cat("\n")
    }

  } else {
    stop("Invalid model type. It must be 'binary', 'multinomial', or 'ordinal'.")
    stop("Tipo de modelo no valido. Debe ser 'binario', 'multinomial' u 'ordenado'.")
  }
  return_model <- logit_model
  coef_length <- length(logit_model$coefficients)
  if (coef_length > 5) {
    return_model$coefficients <- return_model$coefficients[1:5]
    cat("Number of coefficients:", coef_length,"but only the first 5 are shown for clarity.\n")
  }
  return(return_model)
}

