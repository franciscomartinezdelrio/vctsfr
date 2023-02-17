test_that("check class of ts parameter", {
  expect_error(plot_ts(3), "Parameter ts should be of class ts")
})

test_that("check different lengths of future and prediction parameters", {
  expect_warning(plot_ts(USAccDeaths, future = 1, prediction = c(1, 1)),
               "Length of prediction and future parameters are different")
})
