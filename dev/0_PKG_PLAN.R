# ISSUES (0.2.0) ------------------------------------------------------------------------------


# TODO: Update README + new NEWS.md for changes via usethis::use_news_md()
# ---> changes: nested data splitting, changed representation
# TODO: Update vignette

# TODO: check all below
# TODO: check def of variable "subset" in <mldesign_split>$info

# TODO:
# based on get_info(splits), combine splits with identical idx_train
# introduce grouping variable idx_split (idx_test)

# TODO: HP TUNING
# TODO: STRATA FOR TEST SET
# https://mlr3book.mlr-org.com/chapters/chapter3/evaluation_and_benchmarking.html
# MLR3 feature

# TODO: train
# TODO: score/eval (grouped)
# -> see: https://mlr3book.mlr-org.com/chapters/chapter3/evaluation_and_benchmarking.html

# TODO: ADD UNIT TESTS
# TODO: DELETE CODE BELOW, UPDATE ISSUES FOR 0.3.0



library(mlr3verse)
library(mlr3tuningspaces)

set.seed(123)
data <- generate_data(2000)
spec <- specify_estimand(co(~train$country != test$country))

splits <- derive_splits(spec, data)
splits <- derive_splits(nest(spec), data)


# Create a task
task <- mlr3::as_task_classif(response~age+comorbidity, data=data)
#task$set_col_roles("country", "stratum") # TODO: add column with idx_split ------------------------------------------------------
task$col_roles


train_sets <- get_splits(splits, set="train")
test_sets <- get_splits(splits, set="test")

# Instantiate Resampling
resampling <- rsmp("custom")
resampling$instantiate(task, train_sets, test_sets)
resampling$train_set(1)
resampling$test_set(1)


mlr3::mlr_learners
#mlr3::mlr_learners$items$classif.glmnet
#mlr3::mlr_learners$items$classif.cv_glmnet
learner <- mlr3::lrn("classif.ranger", predict_type = "prob")


set.seed(1337)
instance = tune(
  tnr("random_search"),
  task = task,
  learner = learner,
  resampling = resampling,
  measure = msr("classif.auc"),
  term_evals = 7
)

splits$info

instance
instance$archive$predictions(7)


# ---------------------------------------------------------------------------------------------






# Version 2 converted sets --------------------------------------------------------------------







set.seed(123)
data <- generate_data(2000)
spec <- specify_estimand(co(~train$country != test$country))



splits <- derive_splits(spec, data)
splits %>% get_sets()
splits$sets <- get_sets(splits)
splits <- splits %>% add_mlr3_vars()
#splits <- derive_splits(nest(spec), data) %>% add_mlr3_vars()
str(splits, 2)
str(splits$mlr3$sets$test)


# Create a task
task <- mlr3::as_task_classif(outcome~age+comorbidity, data=data)
#task$set_col_roles("country", "stratum") # TODO: add column with idx_split ------------------------------------------------------
task$col_roles


# Instantiate Resampling
resampling <- rsmp("custom")
resampling$instantiate(task, get_mlr3_sets_train(splits), get_mlr3_sets_test(splits))
resampling$train_set(1)
resampling$test_set(1)


mlr3::mlr_learners
#mlr3::mlr_learners$items$classif.glmnet
#mlr3::mlr_learners$items$classif.cv_glmnet
learner <- mlr3::lrn("classif.ranger", predict_type = "prob")


set.seed(1337)
instance = tune(
  tnr("random_search"),
  task = task,
  learner = learner,
  resampling = resampling,
  measure = msr("classif.auc"),
  term_evals = 7
)

?mlr3::PredictionClassif

xx <- instance$archive$predictions(1)
mlr3measures::auc(xx)

splits_mlr3$map

instance
instance$archive$predictions(7)

splits

preds <- get_mlr3_predictions(NULL, splits, instance)

preds %>%
  dplyr::group_by(idx_model, idx_split) %>%
  dplyr::summarize(acc = mlr3measures::acc(truth, response),
                   bacc = mlr3measures::bacc(truth, response),
                   auc = mlr3measures::auc(truth, prob.1, "1"))



# pROC::auc(pROC::roc(response=truth, predictor=prob.1))





# ISSUES (0.3.0) ------------------------------------------------------------------------------
# TODO: specify_estimand: warning vs. error on EMPTY "relation" arg
# TODO: better parsing of variable names, e.g. in case of "train$year)"
# TODO: splitting functions cv, holdout, etc. should they be in dictonary instead of "global" funs?

# TODO: generate_data: should be more flexible, i.e. more args (n_clinic, n_country, ...)
# TODO: generate_data: allow for between year, clinic, region etc. heterogeneity (SIMPLE model with couple of features)

# TODO: filter_splits rename old fun -> new fun: allow filtering by arbitrary constraints

# TODO: sort out filter_split, expand_split, subset_split (which should be exported)
# TODO: check that train set should be based on all points not only test_eligible

# TODO: pkgdown site

# TODO: add attributes to derive_split (like n_test_obs (included/excluded), n_train_sets)

# TODO: Double check default args for 'methods' e.g. specify_method("cv") throws error- expeccted?

# TODO: move data.table everywhere

# TODO: re-organise/cleanup R folder
# TODO: re-organise/cleanup unit tests

# TODO: overhaul/optimize all function names









# ISSUES (0.4.0) ------------------------------------------------------------------------------
# -> FOR CRAN release: documentation, tests


# TODO: vignette for data augmentation (idx_obs, idx_origin)

# TODO: refactor get_vars() to avoid purrr and stringi dependence
# TODO: special case: no vars at all
# case a: no constraints defined: expect length 0
# case b: only constraint(s) operating on complete data defined, want to continue computations...
#   -> return( data[,"obs_id", drop=FALSE] ) ???

# TODO: ??? plan for parse_constraint_old -> implement short form again later? -> old version when "::" prefix

# TODO: nested data splitting optimization: calc outer and inner splits independently, then build intersection

# TODO: construct_splits: optimize "which" expression -> train= (R@j[R@i == k-1]) +1
# TODO: data.table refactoring
# TODO: refactor with checkmate, styler

# TODO: examples for all exported functions
# TODO: more unit tests

# TODO: 2nd vignette: more complex example, including (synthetic) heterogeneous dataset and ML,
# different estimands
# - simple ML: glmnet::cv.glmnet() (as inner data splitting is not yet implemented)

# TODO: revision of "overview" vignette / UML diagram + clean up

# TODO eval_constraint
## TODO: check if index set (i,j) can be optimized,
## e.g. for SYMMETRIC, e.g. equal/unequal, <, >: only need to check i<=j

## TODO:
## (1) for "equal":
## ---> clear that i <- j <- 1:n; x=TRUE
## (2) for !=, <, > (in general: conditions known to be anti-symmetric, i.e. cond(i,j) => !cond(j,i))
## ---> i,j such that only comb i<j (or <= ???) are evaluated
## (3) for other (e.g. <=, >=)
## ---> similar to (2)
## (4) for unknown: do as already implemented



# ISSUES (LATER) ------------------------------------------------------------------------------

# TODO: efficiency tests + main algo more efficient (Rcpp)
# - in build_splits while loop:
#   - at start of while loop: potentially abort, if all entries of L are FALSE at any step
#     (is this expensive to eval? control via arg to apply_design)
# TODO: derive_splits: implement equivalence classes, before relation step?!?

# TODO: random data splitting cv/holdout/bootstrap (common args, stratify, grouped...)
# TODO: nested cv, inner/outer splits
t
# TODO: splits[[3, 4]] index for nested cv
# TODO: summary and/or describe_split or similar
# --> get some description of the datasets, i.e. train_sets, test_sets and relations
#' @export
#' @importFrom utils str
summary.mldesign_estimand <- function(object, ...){
  utils::str(object, 1) # TODO: overview of number of constraints
}

#' @export
#' @importFrom utils str
summary.mldesign_splits <- function(object, ...){
  utils::str(object, 2) # TODO: overview of number of n_train/n_test
}
# TODO: time splicing, i.e. derive discrete time steps (per overall, per splits (defined so far))
# TODO: get_inner(_idx)/outer(_idx)






