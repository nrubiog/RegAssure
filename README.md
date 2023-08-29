
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
#> Linearity:
#>  Mean of residuals:  1.075529e-16 
#> 
#> Homoscedasticity:
#>  p-value:  0.6438038 
#> 
#> Independence:
#>  p-value:  0.0412251 
#>  Durbin-Watson test statistic:  1.362399 
#> 
#> Normality:
#>  Shapiro-Wilk test:
#>   p-value:  0.03427476 
#>   W-statistic:  0.9279165 
#> 
#> Multicollinearity:
#>  VIF:  1.766625 1.766625
#> 
#> Call:
#> lm(formula = mpg ~ wt + hp, data = mtcars)
#> 
#> Coefficients:
#> (Intercept)           wt           hp  
#>    37.22727     -3.87783     -0.03177
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
#> Tests performed for binary/binomial model.
#> 
#> Linearity: Box Tidwell test:
#>  MLE of lambda Score Statistic (t) Pr(>|t|)
#>       1.435966           -1.222135 0.221981
#> 
#> Classification Accuracy (ROC Curve):
#> Setting levels: control = 0, case = 1
#> Setting direction: controls < cases
#> Area under the curve: 0.8453
#> 
#> Classification Accuracy (Confusion Matrix):
#>         Predicciones
#> Variable   0   1
#>        0 468  81
#>        1 109 233
#> Multicollinearity: VIF:
#>   Pclass      Sex 
#> 1.085996 1.085996
#> 
#> Call:  glm(formula = Survived ~ Pclass + Sex, family = "binomial", data = titanic)
#> 
#> Coefficients:
#> (Intercept)       Pclass      Sexmale  
#>      3.2946      -0.9606      -2.6434  
#> 
#> Degrees of Freedom: 890 Total (i.e. Null);  888 Residual
#> Null Deviance:       1187 
#> Residual Deviance: 827.2     AIC: 833.2
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
