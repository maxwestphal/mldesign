derive_splits_method <- function(method, data){
  splits <- do.call(method$method, args=c(list(data=data), method$args))

  define_splits(info=NULL, sets=NULL, splits=splits) %>% return()
}



#' Specify a data splitting method
#'
#' @param method (character) \cr
#' @param ... (any) \cr named arguments to function name
#' @param default (logical) \cr Should default arguments be used? (default: FALSE)
#' @param name (character) \cr name of the data splitting method, equal to \code{method} by default.
#'
#' @return an object of class \code{mldesign_method}
#' @export
#'
#' @examples
#' \dontrun{specify_method()}
#' specify_method("cv")
#' specify_method("cv", default=TRUE)
#' specify_method("cv", n_folds=10)
specify_method <- function(method, ..., default=FALSE, name=method){

  if(!( method %in% valid_methods )){
    stop(paste0("[mldesign] method needs to be one of the following: ",
                paste0("'", valid_methods, "'", collapse = "")))
  }

  stopifnot(is.character(name))
  stopifnot(is.logical(default))

  args <- list(...)
  args_default <- formals(method) %>% "["(-1) %>% as.list()


  if(length(args)==0 & !default){
    message("[mldesign] please supply the following arguments for selected splitting method '", method, "':")
    message(str(args_default, 1))
    return(invisible(NULL))
  }

  if(length(args)==0 & default){
    message("[mldesign] using default arguments of selected splitting method '", method, "':")
    message(str(args_default, 1))
    args_final <- args_default
  }

  if(length(args)>0 & !default){
    if(!all(names(args) %in% names(args_default))){
      message(paste0("[mldesign] some args supplied via '...' do not match args of method ", method, ":" ))
      message(paste(
        names(args)[sapply(names(args), \(x) {! (x %in% names(args_default))}) ],
        collapse=", "
      ))
    }

    args_final <- args_default
    args_valid <- names(args)[names(args) %in% names(args_default)]
    args_final[args_valid] <- args[args_valid]

    args_missing <- setdiff(names(args_default), names(args))

    if(length(args_missing) > 0){
      message("[mldesign] using defaults for the following arguments of splitting method '", method, "':")
      message(str(args_default[args_missing], 1))
    }

  }

  if(length(args)>0 & default){
    stop("[mldesign] please either use default=TRUE or supply arguments via '...' (not both!)")
  }

  ## return:
  define_method(name=name, method=method, args=args_final)

}

