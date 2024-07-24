valid_methods <- c("cv", "extensive_cv")

#' @importFrom splitTools partition
cv <- function(data, n_folds=5, strata=NULL){

  folds <- draw_folds(data=data, n_folds=n_folds, strata=strata)
  n_obs <- nrow(data)
  splits <- lapply(folds, function(x) list(train = setdiff(1:n_obs, x), test=x))
  names(splits) <- NULL

  ## add fold_info attribute:
  fi <- matrix("train", n_folds, n_folds)
  fi[row(fi) == col(fi)] <- "test"
  colnames(fi) <- paste0("fold_", LETTERS[1:n_folds])
  fold_info <- cbind(split_idx = 1:n_folds, as.data.frame(fi))

  attr(splits, "fold_info") <- fold_info

  return(splits)
}

#' @importFrom utils combn
#' @importFrom magrittr set_colnames
#' @importFrom splitTools partition
extensive_cv <- function(data, n_folds=3, strata=NULL, incl_all_train = FALSE){

  folds <- draw_folds(data=data, n_folds=n_folds, strata=strata)

  fold_idx <- do.call(c, lapply(1:(n_folds-as.numeric(!incl_all_train)), \(x){
    utils::combn(1:n_folds, x, simplify=FALSE)
  })) %>%
    lapply(\(x) {list(train = x, test=setdiff(1:n_folds, x))})

  splits <- lapply(fold_idx, \(s){ sapply(s, \(x){ do.call(c, folds[x]) }) })

  ## add fold_info attribute:
  fi <- lapply(fold_idx, \(x){
    y <- rep("train", n_folds)
    y[x$test] <- "test"
    y
  } )
  fi <- do.call(rbind, fi) %>%
    magrittr::set_colnames(paste0("fold_", LETTERS[1:n_folds]))
  fold_info <- cbind(split_idx = 1:length(fold_idx), as.data.frame(fi))

  attr(splits, "fold_info") <- fold_info

  return(splits)
}



draw_folds <- function(data, n_folds, strata){
  ## check args:
  stopifnot(is.data.frame(data))
  stopifnot(n_folds >= 2 & (n_folds %% 1 == 0))

  ## prepare data splitting:
  n_obs <- nrow(data)

  if(!is.null(strata)){
    stopifnot(length(strata)==1)
    stopifnot(strata %in% names(data))
    y <- data[, strata]
    type <- "stratified"
  }else{
    y <- 1:n_obs
    type <- "basic"
  }

  ## conduct data splitting:
  splitTools::partition(y=y, p=rep(1/n_folds, n_folds), type=type)
}
