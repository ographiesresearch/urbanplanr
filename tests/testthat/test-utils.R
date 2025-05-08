testthat::test_that("utils_is_county works", {
  testthat::expect_true(utils_is_county(c("Saginaw County, MI", "Orleans Parish, LA")))
  testthat::expect_false(utils_is_county(c("Saginaw, MI", "New Orleans, LA")))
})

testthat::test_that("utils_is_state works", {
  testthat::expect_true(utils_is_state(c("MI", "LA")))
  testthat::expect_false(utils_is_state(c("Saginaw, MI", "New Orleans, LA")))
})