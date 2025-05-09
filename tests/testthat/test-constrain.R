test_that("constrain tests", {
  c1 <- constrain("test$age >= 18", name = "ic_age")
  expect_s3_class(c1, "mldesign_constraint")
})
