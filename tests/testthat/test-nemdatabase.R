test_that("nemdatabase shiny works", {
  expect_equal(class(.nemdatabase), "shiny.appobj")
})
