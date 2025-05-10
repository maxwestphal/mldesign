test_that("misc functions work", {
  # concat pipe:
  x <- 1 %,% 2 %,% 3
  expect_equal(x, 1:3)

  # print functions:
  print(co("test$county != train$country"))
  print(cc(co("test$county != train$country"), co("test$county - train$year == 1")))
  print(specify_estimand(co("test$county != train$country")))
  print(specify_method("cv", n_folds = 5))
  print(specify_nested(specify_method("cv", n_folds = 5)))
})
