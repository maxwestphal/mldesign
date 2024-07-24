test_that("testing derive_splits()", {
  set.seed(123)
  data <- generate_data(n_obs=2000)


# data splitting based on estimand ------------------------------------------------------------
  estimand <- specify_estimand(
    ## test:
    "test$age >= 18   #name=inclusion_criterion_age #type=p",
    "complete.cases(test)   #name=requires_complete_data #type=a",
    ## relation:
    "test$clinic == train$clinic   #name=trained_in_same_clinic #type=c",
    "test$country == train$country   #name=trained_in_same_country #type=c",
    "(test[['year']] - train[['year']]) %in% 1:2   #name=trained_on_data_from_last_two_years #type=c",
    ## train:
    "nrow(train) >= 10   #name=enough_train_obs #type=a",
    "mean(train[['response']]) >= 0.05   #name=enought_train_cases #type=a",
    "mean(train[['response']]) <= 0.95   #name=enought_train_controls #type=a"
  )

  splits_e <- derive_splits(estimand, data)

  expect_s3_class(estimand, "mldesign_spec")
  expect_s3_class(estimand, "mldesign_estimand")
  expect_s3_class(splits_e, "mldesign_splits")
  expect_length(splits_e, 80)


# data splitting based on (conventional) method -----------------------------------------------
  method <- specify_method("cv", default=TRUE)
  splits_m <- derive_splits(method, data)
  expect_s3_class(method, "mldesign_spec")
  expect_s3_class(method, "mldesign_method")
  expect_s3_class(splits_m, "mldesign_splits")
  expect_length(splits_m, 5)

  # data splitting based on (conventional) method -----------------------------------------------
  method <- specify_method("extensive_cv", default=TRUE)
  splits_m <- derive_splits(method, data)
  expect_s3_class(method, "mldesign_spec")
  expect_s3_class(method, "mldesign_method")
  expect_s3_class(splits_m, "mldesign_splits")
  expect_length(splits_m, 6)


})


