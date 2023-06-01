test_that("calc_entropy requires a counts matrix", {
  expect_error(calc_entropy())
})
