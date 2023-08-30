
# Text to LRM assumptions
test_that("It calculates assumptions correctly", {

  expect_equal(check_lm_assumptions(lm(mpg ~ wt + hp, data = mtcars)), check_lm_assumptions(lm(mpg ~ wt + hp, data = mtcars)))
})
