
<!-- README.md is generated from README.Rmd. Please edit that file -->

# RegAssure

<!-- badges: start -->

[![R-CMD-check](https://github.com/nrubiog/RegAssure/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/nrubiog/RegAssure/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The RegAssure package is designed to simplify and enhance the process of
validating regression model assumptions in R. It provides a
comprehensive set of tools for evaluating key assumptions such as
linearity, homoscedasticity, independence, normality, and collinearity,
contributing to the reliability of analytical results.

## Installation

You can easily install RegAssure from GitHub using the devtools package:

``` r
# install.packages("devtools")
devtools::install_github("nrubiog/RegAssure")
```

## Example: Linear Regression

Here’s a basic example showcasing how RegAssure can be used to enhance
linear regression analysis:

``` r
# Install the package
# devtools::install_github("nrubiog/RegAssure")

# Load the package
library(RegAssure)

# Create a regression model
lm_model <- lm(mpg ~ wt + hp, data = mtcars)

# Check assumptions
check_lm_assumptions(lm_model)
#> 
#> The assumption tests have been completed and the results are available in a list. Enjoy it :)
#> Las pruebas de supuestos han sido completadas y los resultados están disponibles en una lista. Disfrútalo :)
#> $Linearity
#> [1] 1.075529e-16
#> 
#> $Homoscedasticity
#> 
#>  studentized Breusch-Pagan test
#> 
#> data:  model
#> BP = 0.88072, df = 2, p-value = 0.6438
#> 
#> 
#> $Independence
#> 
#>  Durbin-Watson test
#> 
#> data:  model
#> DW = 1.3624, p-value = 0.04123
#> alternative hypothesis: true autocorrelation is not 0
#> 
#> 
#> $Normality
#> 
#>  Shapiro-Wilk normality test
#> 
#> data:  model$residuals
#> W = 0.92792, p-value = 0.03427
#> 
#> 
#> $Multicollinearity
#>       wt       hp 
#> 1.766625 1.766625
```

## Example: Logistic Regression

Here’s an additional example demonstrating the use of RegAssure with
logistic regression:

``` r
# Load the package

library(RegAssure)
library(titanic)

# Load the dataframe
titanic <- titanic_train

# Create a binary logistic regression model
logit_model <- glm(Survived ~ Pclass + Sex, data = titanic, family = "binomial")

# Check assumptions for binary logistic regression
check_logit(logit_model, data = titanic, tipo_modelo = "binario", vars_numericas = "Pclass", y = "Survived")
#> logit_model has no missing values.
#> 
#> Tests performed for binary/binomial model.
#> Warning in check_logit(logit_model, data = titanic, tipo_modelo = "binario", : Box-Tidwell Test cannot be done.
#> Warning in check_logit(logit_model, data = titanic, tipo_modelo = "binario", : Variance Inflation Factor Test cannot be done.
#> 
#> The assumption tests have been completed and the results are available in a list. Enjoy it :)
#> Las pruebas de supuestos han sido completadas y los resultados están disponibles en una lista. Disfrútalo :)
#> $model_type
#> [1] "binary/binomial"
#> 
#> $Confusion
#>         Predicciones
#> Variable   0   1
#>        0 468  81
#>        1 109 233
#> 
#> $ROC
#> 
#> Call:
#> roc.default(response = new_data[[y]], predictor = pred_logit,     smooth = TRUE, auc = TRUE, ci = TRUE, ret = TRUE)
#> 
#> Data: pred_logit in 549 controls (new_data[[y]] 0) < 342 cases (new_data[[y]] 1).
#> Smoothing: binormal 
#> Area under the curve: 0.8453
#> 95% CI: 0.8115-0.8729 (2000 stratified bootstrap replicates)
```

## Example: storing data

Here’s an example of how to use the get_predict() function to compare
real and predicted values from a model:

``` r
# Load the package
library(RegAssure)

# Create a regression model
lm_model <- lm(mpg ~ wt + hp, data = mtcars)

# Get predictions and compare with real values
predictions <- get_predict(lm_model, mtcars, mtcars$mpg, n = 3)

# Print the results
print(predictions[1:7,])
#>                   reales predichos  Error
#> Mazda RX4           21.0    23.572 -2.572
#> Mazda RX4 Wag       21.0    22.583 -1.583
#> Datsun 710          22.8    25.276 -2.476
#> Hornet 4 Drive      21.4    21.265  0.135
#> Hornet Sportabout   18.7    18.327  0.373
#> Valiant             18.1    20.474 -2.374
#> Duster 360          14.3    15.599 -1.299
```

By incorporating RegAssure into your workflow, you can streamline the
process of assessing and addressing regression model assumptions,
leading to more informed decision-making.
