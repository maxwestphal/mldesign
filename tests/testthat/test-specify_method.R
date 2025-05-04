test_that("specify_method", {

  specify_method("cv", default=TRUE) %>%
    expect_s3_class("mldesign_method")

  specify_method("cv", n_folds=5, strata="RESPONSE") %>%
    expect_s3_class("mldesign_method")

  specify_method("extensive_cv", default=TRUE) %>%
    expect_s3_class("mldesign_method")

  specify_method("extensive_cv", n_folds=5, strata="RESPONSE") %>%
    expect_s3_class("mldesign_method")

})
