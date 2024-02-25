#' @export
#' @importFrom utils str
print.mldesign_constraint <- function(x, ...){
  utils::str(x, 1)
}

print.mldesign_constraints <- function(x, ...){
  utils::str(x, 2)
}

#' @export
#' @importFrom utils str
print.mldesign_estimand <- function(x, ...){
  utils::str(x, 3)
}

#' @export
#' @importFrom utils str
print.mldesign_splits <- function(x, ...){
  utils::str(x, 2)
}
