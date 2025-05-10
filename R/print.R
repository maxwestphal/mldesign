#' @export
#' @importFrom utils str
print.mldesign_constraint <- function(x, ...) {
  utils::str(x, 1)
}

#' @export
#' @importFrom utils str
print.mldesign_constraints <- function(x, ...) {
  utils::str(x, 2)
}

#' @export
#' @importFrom utils str
print.mldesign_estimand <- function(x, ...) {
  utils::str(x, 3)
}

#' @export
#' @importFrom utils str
print.mldesign_method <- function(x, ...) {
  utils::str(x, 1)
}

#' @export
#' @importFrom utils str
print.mldesign_nested <- function(x, ...) {
  utils::str(x, 2)
}

#' @export
#' @importFrom utils str
print.mldesign_splits <- function(x, ...) {
  message("[mldesign] mldesign_splits object with the following properties:")
  print(x$info)
}
