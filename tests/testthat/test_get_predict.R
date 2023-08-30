
# Text to Get predicted values
test_that("It calculates assumptions correctly", {

  expect_equal(get_predict(lm_model <- lm(mpg ~ wt + hp, data = mtcars), mtcars, mtcars$mpg, n = 3),
               get_predict(lm_model <- lm(mpg ~ wt + hp, data = mtcars), mtcars, mtcars$mpg, n = 3))
})

