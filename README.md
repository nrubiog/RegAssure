
<!-- README.md is generated from README.Rmd. Please edit that file -->

# RegAssure

<!-- badges: start -->
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
logit_model <- glm(Survived ~ PassengerId, data = titanic, family = "binomial")

# Check assumptions for binary logistic regression
check_logit(logit_model, data = titanic, tipo_modelo = "binario", vars_numericas = "PassengerId", y = "Survived")
#> Tests performed for binary/binomial model.
#> 
#> Linearity: Box Tidwell test:
#>  MLE of lambda Score Statistic (t)     Pr(>|t|)
#>              1            3.359283 0.0008147289
#> 
#> Classification Accuracy (ROC Curve):
#> Setting levels: control = 0, case = 1
#> Setting direction: controls < cases
#> Area under the curve: 0.5064
#> 
#> Classification Accuracy (Confusion Matrix):
#>         Predicciones
#> Variable   0
#>        0 549
#>        1 342
#> Multicollinearity:
#> 
#> VIF test cannot be applied since the model contains fewer than 2 independent variables.
#> 
#> Call:  glm(formula = Survived ~ PassengerId, family = "binomial", data = titanic)
#> 
#> Coefficients:
#> (Intercept)  PassengerId  
#>  -4.554e-01   -4.003e-05  
#> 
#> Degrees of Freedom: 890 Total (i.e. Null);  889 Residual
#> Null Deviance:       1187 
#> Residual Deviance: 1187  AIC: 1191
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
print(predictions)
#>                     reales predichos  Error
#> Mazda RX4             21.0    23.572 -2.572
#> Mazda RX4 Wag         21.0    22.583 -1.583
#> Datsun 710            22.8    25.276 -2.476
#> Hornet 4 Drive        21.4    21.265  0.135
#> Hornet Sportabout     18.7    18.327  0.373
#> Valiant               18.1    20.474 -2.374
#> Duster 360            14.3    15.599 -1.299
#> Merc 240D             24.4    22.887  1.513
#> Merc 230              22.8    21.994  0.806
#> Merc 280              19.2    19.979 -0.779
#> Merc 280C             17.8    19.979 -2.179
#> Merc 450SE            16.4    15.725  0.675
#> Merc 450SL            17.3    17.044  0.256
#> Merc 450SLC           15.2    16.850 -1.650
#> Cadillac Fleetwood    10.4    10.355  0.045
#> Lincoln Continental   10.4     9.363  1.037
#> Chrysler Imperial     14.7     9.192  5.508
#> Fiat 128              32.4    26.599  5.801
#> Honda Civic           30.4    29.312  1.088
#> Toyota Corolla        33.9    28.046  5.854
#> Toyota Corona         21.5    24.586 -3.086
#> Dodge Challenger      15.5    18.811 -3.311
#> AMC Javelin           15.2    19.141 -3.941
#> Camaro Z28            13.3    14.552 -1.252
#> Pontiac Firebird      19.2    16.757  2.443
#> Fiat X1-9             27.3    27.627 -0.327
#> Porsche 914-2         26.0    26.037 -0.037
#> Lotus Europa          30.4    27.770  2.630
#> Ford Pantera L        15.8    16.546 -0.746
#> Ferrari Dino          19.7    20.925 -1.225
#> Maserati Bora         15.0    12.739  2.261
#> Volvo 142E            21.4    22.984 -1.584
```

By incorporating RegAssure into your workflow, you can streamline the
process of assessing and addressing regression model assumptions,
leading to more informed decision-making.
