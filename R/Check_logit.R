#' Check Assumptions for Logit Models
#'
#' @description
#' This function performs assumption checks for logistic regression models, including binary, multinomial, and ordered models. It conducts various tests to assess the validity of the model assumptions.
#'
#' @param logit_model Fitted logistic regression model.
#' @param data dataframe containing predictor variables.
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

  # Verificar si el dataframe tiene valores faltantes
  df_name <- deparse(substitute(logit_model))
  model_length <- length(logit_model$model[, 1])
  data_length <- length(logit_model$data[, 1])

  if (model_length < data_length) {
    new_data <- na.omit(logit_model$data)
    cat(df_name, "has missing values.\nA new dataframe will be created without affecting", df_name, ".\n\n")
  } else {
    cat(df_name, "has no missing values.\n")
    new_data <- logit_model$data
  }

  # Convertir vars_numericas en un vector de nombres de variables
  if (!is.null(vars_numericas)) {
    vars_numericas <- unlist(vars_numericas)
  }

  # Prueba de linealidad (test de Box-Tidwell) para variables numericas continuas

  resultado_box_tidwell <- NULL

  box_tidwell <- function() {

  if (!tipo_modelo %in% c("ordenada", "ordenado", "ordered", "ordinal") &&
      !is.null(vars_numericas) && length(vars_numericas) > 0) {

    numeric_vars <- new_data[, vars_numericas]

    if (all(!is.na(numeric_vars)) && all(numeric_vars > 0)) {
      tryCatch({
        # Crear una formula para la prueba de linealidad
        formula_linealidad <- logit_model$linear.predictors

        # Realizar la prueba de linealidad (ajusta según la función que estés usando)
        resultado_box_tidwell <- suppressWarnings(boxTidwell(formula_linealidad, numeric_vars))
      }, error = function(e) {
        if (grepl("the variables to be transformed must have only positive values", e$message)) {
          warning("Linearity: Box Tidwell test cannot be applied.\nReason:", e$message, "\n\n")
        } else {
          resultado_box_tidwell <- NULL
        }
      })
    } else {
      warning("Linearity: Box Tidwell test cannot be applied due to non-positive values or missing values in numeric variables.\n\n")
    }
  } else {
    warning("Linearity: Box Tidwell test cannot be applied since numeric variables have not been specified.\n\n")
  }

  return(resultado_box_tidwell)
    }

  # Prueba de multicolinealidad

  vif_result <- NULL

  if(length(coef(logit_model)) >= 3) {

    design_matrix <- model.matrix(logit_model)

    # Identificar las columnas correspondientes a las variables independientes

    independent_vars <- design_matrix[, -1] # Excluir la columna de intercepto

    vars_names <- colnames(independent_vars)

    vars_names2 <- vars_names[vars_names %in% colnames(new_data)]

    numeric_var <- length(vars_names2)

    if (numeric_var >= 2) {

      is_num <- sapply(new_data[, vars_names2], is.numeric)

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
        curva_roc <- suppressMessages(roc(new_data[[y]], pred_logit, smooth = TRUE, auc = TRUE, ci = TRUE, ret = TRUE))
      }, error = function(err1) {
        if (grepl("ROC curve not smoothable", err1$message)) {
          warning("Warning: The curve cannot be smoothed with smooth = TRUE.\n")
          warning("Trying with smooth = FALSE.\n")
        } else {
          warning("\nFailed to calculate ROC curve with smooth = TRUE. Changing smooth to FALSE.\n")
          # Intentar calcular con smooth = FALSE
          curva_roc <- try(suppressMessages(roc(new_data[[y]], pred_logit,
                                                smooth = FALSE, auc = TRUE, ci = TRUE, ret = TRUE)), silent = TRUE)
        }
        return(curva_roc)
      }
      )}
  }

  # Matriz de confusion para los modelos logit binarios

  if (tipo_modelo %in% c("binario","binaria","binomial")) {

    matriz_confusion <- function() {
      if (tipo_modelo %in% c("binario", "binaria" ,"binomial")) {
        pred_logit <- predict(logit_model, type = "response")
        predicciones <- ifelse(pred_logit > 0.5, 1, 0)
        matriz_confusion_logit <- table(new_data[[y]], predicciones, dnn = c("Variable", "Predicciones"))
        return(matriz_confusion_logit)
      } else {
        return(NULL)
      }
    }
  }

    # Realizar las pruebas correspondientes segun el tipo de modelo

  # Lista para almacenar los resultados
  resultados_list <- list()

  if (tipo_modelo %in% c("binario", "binaria", "binomial")) {
    message("\nTests performed for binary/binomial model.\n")

    resultados_list$model_type <- "binary/binomial"

    # Linearity
    if (!is.null(vars_numericas) && !is.null(resultado_box_tidwell))
      {
    resultados_list$Linearity <- resultado_box_tidwell
    } else {
      warning("Box-Tidwell Test cannot be done.\n\n")
    }

    # Multicollinearity
    if (is.null(vif_result)) {
      warning("Variance Inflation Factor Test cannot be done.\n\n")
    } else {
      resultados_list$Multicollinearity <- vif_result
    }

    # Accuracy using Confusion Matrix

    confusion_matrix <- matriz_confusion()
    if(!is.null(confusion_matrix)){
    resultados_list$Confusion <- confusion_matrix
    } else {
      warning("Confusion Matrix cannot be calculated.\n\n")
    }

    # Accuracy using ROC Curve

    if(!is.null(prueba_roc))
      {
    resultados_list$ROC <- prueba_roc()
    } else {
      warning("ROC Curve cannot be calculated.\n\n")
    }

  } else if (tipo_modelo %in% c("multinomial", "multinominal")) {

    message("\nTests performed for multinomial model.\n")

    resultados_list$model_type <- "Multinomial"

    # Linearity
    if (!is.null(vars_numericas) && !is.null(resultado_box_tidwell))
    {
      resultados_list$Linearity <- resultado_box_tidwell
    } else {
      warning("Box-Tidwell Test cannot be done.\n\n")
    }

    # Multicollinearity
    if (is.null(vif_result)) {
      warning("Variance Inflation Factor Test cannot be done.\n\n")
    } else {
      resultados_list$Multicollinearity <- vif_result
    }

  } else if (tipo_modelo %in% c("ordenada", "ordenado", "ordered", "ordinal")) {

    message("\nTests performed for ordinal model.\n")

    resultados_list$model_type <- "Ordinal"

    # Parallelism

    if(!is.null(prueba_brant())){
      prueba_brant()
    } else {
        warning("Brant Test cannot be calculated.\n\n")
      }

    # Multicollinearity
    if (is.null(vif_result)) {
      warning("Variance Inflation Factor Test cannot be done.\n\n")
    } else {
      resultados_list$Multicollinearity <- vif_result
    }

  } else {
    stop("Invalid model type. It must be 'binary', 'multinomial', or 'ordinal'.\n")
    stop("Tipo de modelo no valido. Debe ser 'binario', 'multinomial' u 'ordenado'.")
  }

  message("\nThe assumption tests have been completed and the results are available in a list. Enjoy it :)\n")
  message("Las pruebas de supuestos han sido completadas y los resultados estan disponibles en una lista. Disfrutalo :)\n")

  return(resultados_list)
}

