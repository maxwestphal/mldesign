#' Get predictions after a mlr3 training/tuning with an custom data splitting
#'
#' @param idx_model (NULL | numeric) \cr either NULL to select all models or an integer (vector) in the range
#' 1:instance$archive$n_evals
#' @param splits (mldesign_splits) \cr ... TODO ...
#' @param instance (...) \cr ... TODO: what classes are allowed? ...
#'
#' @returns (data.table)
#' @export
# TODO: show workflow with mlr3 (example and/or vignette) ...
get_mlr3_predictions <- function(idx_model=NULL, splits, instance){
  if(is.null(idx_model)){idx_model <- 1:instance$archive$n_evals}
  lapply(idx_model, \(i) cbind(idx_model=i, get_mlr3_predictions_1(i, splits, instance))) %>%
    data.table::rbindlist() %>%
    return()
}

#' @importFrom data.table as.data.table
#' @importFrom data.table rbindlist
get_mlr3_predictions_1 <- function(idx_model=1, splits, instance){
  lapply(seq_along(get_mlr3_map(splits)),
         \(i) cbind(get_mlr3_map(splits)[[i]],
                    instance$archive$predictions(idx_model)[[i]] %>% data.table::as.data.table() )) %>%
    data.table::rbindlist() %>%
    return()
}
