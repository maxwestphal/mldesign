test_that("testing derive_splits()", {
  set.seed(123)
  data <- generate_data(n_obs=2000)

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

  splits <- derive_splits(estimand, data)

  expect_s3_class(estimand, "mldesign_estimand")
  expect_s3_class(splits, "mldesign_splits")
  expect_length(splits, 80)

})


