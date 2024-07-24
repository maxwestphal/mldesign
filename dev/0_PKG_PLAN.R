


# ISSUES (0.1.0) ------------------------------------------------------------------------------
# -> BEFORE BiomColl2024
# DOCUMENTATION

# TODO: GIT PUSH (RESET GIT FIRST, i.e. new "initial commit")

# PUBLISH
# TODO: GH: protect "main" branch
# TODO: add "About" at Github
# TODO: GH: github actions R CMD CHECK + README.Rmd
# TODO: finalize README (e.g. batches, see cases package)
# TODO: GITHUB Release?!?


# ISSUES (0.2.0) ------------------------------------------------------------------------------
# -> AFTER BiomColl2024: functionality

# TODO: data.table refactoring

# TODO: splitting functions cv, holdout, etc. should they be in dictonary instead of "global" funs?
# TODO: summary object (ntrain, ntest) and which vars_relation are included in test set per split
# which obs_idx are in none, multiple test sets

# TODO: in functions constrain() and/or specify_estimand: rename arg "name" to "label"?!?
# TODO: derive_splits: messages after each stage
# TODO: specify_estimand: warning vs. error on EMPTY "relation" arg
# TODO: specify_estimand: mandatory name arg

# TODO: export_split (mlr3, tidymodels, sklearn)
export_split <- function(split, format = c("table", "mlr3", "tidymodels", "sklearn")){

  # add param to add obs_id instead of row_id
  # add param to add raw data?!?
  # table -> rows: obs, cols: split; values: train/tune/test

  stop("not yet implemented")
}
# TODO: better parsing of variable names, e.g. in case of "train$year)"

# TODO: filter_splits rename old fun -> new fun: allow filtering by arbitrary constraints
# TODO: sort out filter_split, expand_split, subset_split (which should be exported)
# TODO: check that train set should be based on all points not only test_eligible
# TODO: plan for parse_constraint_old -> implement short form again later? -> old version when "::" prefix

# TODO: generate_data: should be more flexible, i.e. more args (n_clinic, n_country, ...)
# TODO: generate_data: allow for between year, clinic, region etc. heterogeneity (SIMPLE model with couple of features)

# TODO: specify_method(name=c("holdout", "cv"), ..., strata=NULL, groups=NULL, n_rep=1)
# -> output: mldesign_spec / mldesign_method
# -> alternative / addition to specify_method: holdout(pr_test = 0.2)... cv(n_fold=5)

# TODO: refactor derive_splits to derive_splits_estimand + derive_splits_method

# TODO: new fun: derive_splits_nested(data, spec_outer, spec_inner, obs_id="obs_id")
# TODO: reduce data to vars = union of 3 diff vars, BUT ATTEION:
#       for "complete.cases" / apply(data, 1, fun) or similar constraints,
#       vars needs to cover _all_ variables in dataset

# TODO: refactor get_vars() to avoid purrr and stringi dependence
# TODO: special case: no vars at all
# case a: no constraints defined: expect length 0
# case b: only constraint(s) operating on complete data defined, want to continue computations...
#   -> return( data[,"obs_id", drop=FALSE] ) ???


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

# TODO: construct_splits: optimize "which" expression -> train= (R@j[R@i == k-1]) +1

# ISSUES (0.3.0) ------------------------------------------------------------------------------
# -> FOR CRAN release: documentation, tests

# TODO: refactor with checkmate, styler

# TODO: examples for all exported functions
# TODO: more unit tests

# TODO: 2nd vignette: more complex example, including (synthetic) heterogeneous dataset and ML, different estimands
# - simple ML: glmnet::cv.glmnet() (as inner data splitting is not yet implemented)

# TODO: revision of "overview" vignette / UML diagram + clean up





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






