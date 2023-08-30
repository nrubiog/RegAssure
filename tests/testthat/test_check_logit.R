
# Text to Logit Model assumptions
test_that("It calculates assumptions correctly", {

  expect_equal(check_logit(glm(am ~ wt + hp, data = mtcars, family = "binomial"), data = mtcars, tipo_modelo = "binario", vars_numericas = "wt", y = "am"),
               check_logit(glm(am ~ wt + hp, data = mtcars, family = "binomial"), data = mtcars, tipo_modelo = "binario", vars_numericas = "wt", y = "am"))
})

