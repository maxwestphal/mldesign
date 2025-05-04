#' @name specify_nested
#' @title Specify a nested design
#'
#' @param outer (mldesign_estimand | mldesign_method) \cr outer design specification
#' @param inner (mldesign_estimand | mldesign_method) \cr inner design specification
#' @param name (character) \cr an optional name for the design
#'
#' @return (mldesign_splits)
#'
#' @examples
#' nest(
#' specify_estimand(constrain(~train$country != test$country)),
#' specify_method("cv", n_folds=5)
#' )
#' @importFrom checkmate assert_multi_class
#' @importFrom checkmate assert_character


#' @export
#' @rdname specify_nested
specify_nested <- function(outer, inner=outer, name=NULL){

  ## argument checks:
  checkmate::assert_multi_class(outer, c("mldesign_estimand", "mldesign_method"))
  checkmate::assert_multi_class(inner, c("mldesign_estimand", "mldesign_method"))
  checkmate::assert_character(name, min.chars = 2, null.ok = TRUE)

  ## default name, if name is NULL:
  if(is.null(name)){
    name <- get_default_name_nested(outer, inner)
  }

  ## return spec:
  define_nested(name=name, outer=outer, inner=inner)

}


#' @export
#' @rdname specify_nested
nest <- specify_nested

get_default_name_nested <- function(outer, inner){
  paste0("[outer='", outer$name, "'][", "inner='", inner$name, "']")
}


# TESTING -------------------------------------------------------------------------------------
# data <- generate_data(100)
# spec <- nest(
#   specify_estimand("train$country != test$country"),
#   specify_method("cv", n_folds=3)
# )
# r <- derive_splits_nested(spec, data)
# r$info
# r$sets %>% length()
# r$sets %>% sapply(length)

# ---------------------------------------------------------------------------------------------

#' @importFrom data.table data.table
#' @importFrom dplyr mutate
#' @importFrom dplyr across
#' @importFrom tidyr starts_with
#' @importFrom tidyr replace_na
derive_splits_nested <- function(nested, data){

  spec <- nested

  ## derive outer splits:
  splits_outer <- derive_splits(spec=spec$outer, data=data)

  ## init algorithm:
  info <- list()
  sets <- list(train=list(), test=list())

  idx_split <- 1
  idx_train <- 1
  idx_test <- 1

  ## outer loop:
  for(idx_outer in 1:nrow(get_info(splits_outer))){

    idx_outer_obs_train <- get_splits(splits_outer, idx_outer)$train
    idx_outer_obs_test <- get_splits(splits_outer, idx_outer)$test

    info_folds <- NULL

    # (1) add type='outer' split (subset=FALSE)
    split_new <- list(train = idx_outer_obs_train, test = idx_outer_obs_test)
    sets$train[[idx_train]] <- split_new$train
    sets$test[[idx_test]] <- split_new$test
    info[[length(info)+1]] <- get_split_info(split = split_new,
                                             idx_split, idx_outer, 0,
                                             type = 'outer', subset=FALSE,
                                             idx_train, idx_test,
                                             info_folds = NULL)

    idx_split <- idx_split + 1
    idx_train <- idx_train + 1

    ## inner loop:

    ## derive inner splits:
    splits_inner <- mldesign::derive_splits(spec$inner, data[idx_outer_obs_train, ])
    info_folds <- attr(splits_inner, "info_folds")

    delta_idx <- 1

    for(idx_inner in  1:nrow(get_info(splits_inner))){

      idx_inner_obs_train_rel <- get_splits(splits_inner, idx_inner)$train
      idx_inner_obs_train <- idx_outer_obs_train[idx_inner_obs_train_rel]

      idx_inner_obs_test_rel <- get_splits(splits_inner, idx_inner)$test
      idx_inner_obs_test <- idx_outer_obs_train[idx_inner_obs_test_rel]

      # (2) add type='outer' split (subset=TRUE):
      split_new <- list(train = idx_inner_obs_train, test = idx_outer_obs_test)
      sets$train[[idx_train]] <- split_new$train
      sets$test[[idx_test]] <- split_new$test
      info[[length(info)+1]] <- get_split_info(split = split_new,
                                               idx_split, idx_outer, idx_inner,
                                               type = 'outer', subset=TRUE,
                                               idx_train, idx_test,
                                               info_folds = info_folds[idx_inner,-1])
      idx_split <- idx_split + 1

      # (3) add type='inner' split (subset=NA):
      split_new <- list(train =  idx_inner_obs_train, test = idx_inner_obs_test)
      sets$test[[idx_test+delta_idx]] <- split_new$test
      info[[length(info)+1]] <- get_split_info(split = split_new,
                                               idx_split, idx_outer, idx_inner,
                                               type = 'inner', subset=FALSE,
                                               idx_train, idx_test + delta_idx,
                                               info_folds = info_folds[idx_inner,-1])

      idx_split <- idx_split + 1
      idx_train <- idx_train + 1
      delta_idx <- delta_idx + 1

    }

    idx_test <- idx_test + delta_idx

  }


  ## compile results:
  info <- data.table::rbindlist(info, fill=TRUE) %>%
    dplyr::mutate(dplyr::across(tidyr::starts_with("fold_"), \(x) tidyr::replace_na(x, replace="train")))
  info$idx_test <- info$idx_test + length(sets$train)

  define_splits(info=info, sets=unlist(sets, FALSE, FALSE), splits=NULL) %>% return()

}


get_split_info <- function(split,
                           idx_split, idx_outer, idx_inner,
                           type, subset,
                           idx_train, idx_test,
                           info_folds){

  info <- data.frame(
    idx_split = idx_split,
    idx_outer = idx_outer,
    idx_inner = idx_inner,
    type = type,
    subset = subset,
    idx_train = idx_train,
    idx_test = idx_test,
    n_train = length(split$train),
    n_test = length(split$test),
    n_shared = length(intersect(split$train, split$test))
  )

  if(!is.null(info_folds)){
    stopifnot(nrow(info_folds) == 1)
    info <- cbind(info, info_folds)
  }

  return(info)

}

