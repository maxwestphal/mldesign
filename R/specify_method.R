derive_splits_method <- function(method, data){
  do.call(method$name, args=c(list(data=data), method$args))
}

method_names <- c("cv")

#' Specify a data splitting method
#'
#' @param name (character) \cr name of the data splitting method
#' @param ... (any) \cr named arguments to function name
#' @param default (logical) \cr Should default arguments be used? (default: FALSE)
#'
#' @return an object of class \code{mldesign_method}
#' @export
#'
#' @examples
#' \dontrun{specify_method()}
#' specify_method("cv")
#' specify_method("cv", default=TRUE)
#' specify_method("cv", n_folds=10)
specify_method <- function(name, ..., default=FALSE){

  if(missing(name)){name <- NA}

  if(!( name %in% method_names )){
    stop(paste0("[mldesign] method needs to be one of the following: ",
                paste0("'", method_names, "'", collapse = "")))
  }

  args <- list(...)
  args_default <- formals(name) %>% "["(-1) %>% as.list()


  if(length(args)==0 & !default){
    message("[mldesign] please supply the following arguments for selected splitting method '", name, "':")
    args_default %>% str(1)
    return(invisible(NULL))
  }

  if(length(args)==0 & default){
    message("[mldesign] using default arguments of selected splitting method '", name, "':")
    args <- args_default
  }

  if(length(args)>0){
    if(any( sort(names(args)) != sort(names(args_default)) )){
      stop(paste0("[mldesign] all args of method ", name, "need to be supplied via '...'" ))
    }
  }

  list(name=name, args=args) %>%
    add_class("mldesign_method") %>%
    add_class("mldesign_spec") %>%
    return()

}



#' @importFrom caret createFolds
cv <- function(data, n_folds=5){
  n <- nrow(data)
  folds <- caret::createFolds(1:n, k = n_folds)
  splits <- lapply(folds, function(x) list(train = setdiff(1:n, x), test=x))
  names(splits) <- NULL
  return(splits)
}
