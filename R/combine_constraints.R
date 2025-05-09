#' @title Combine multiple constraints
#' @name combine_constraints
#' @description
#' ...
#'
#' @param ... (character | mldesign_constraint | mldesign_constraints) ...
#' @param constraints (list) \cr further arguments not passed via \code{...} (with same allowed classes).
#' @param target (NULL | character) \cr ...
#' @param type (NULL | character) \cr ...
#'
#' @return
#' (mldesign_constraints)
#' @export
#'
#' @examples
#' combine_constraints(
#'   constrain("test$age > 50"),
#'   constrain("test$risk == 'high'")
#' )

#' @export
#' @rdname combine_constraints
combine_constraints <- function(..., constraints = list(), type = NULL, target = NULL) {
  args <- c(list(...), constraints)

  if (length(args) == 0) {
    return(define_constraints())
  }

  idx_character <- sapply(args, methods::is, class2 = "character") %>% which()
  idx_constraint <- sapply(args, methods::is, class2 = "mldesign_constraint") %>% which()
  idx_constraints <- sapply(args, methods::is, class2 = "mldesign_constraints") %>% which()

  idx_combined <- c(idx_character, idx_constraint, idx_constraints)

  ## check if class identification was unambiguous (should never happen):
  if (any(duplicated(idx_combined))) {
    stop("[mldesign] some argument has two or more of the allowed classes...")
  }

  ## check if all args have admissible class:
  if (!all(sort(idx_combined) == 1:length(args))) {
    stop("[mldesign] not all arguments have suitable class, see ?combine_constraints")
  }

  clist <- list()

  if (length(idx_character) > 0) {
    clist <- clist %>% c(
      do.call(c, lapply(args[idx_character], parse_constraints))
    )
  }

  if (length(idx_constraint) > 0) {
    clist <- clist %>% c(
      args[idx_constraint]
    )
  }

  if (length(idx_constraints) > 0) {
    clist <- clist %>% c(
      do.call(c, args[idx_constraints])
    )
  }


  ## combine results and compile to constraints object:
  constraints <- do.call(define_constraints, clist)

  stopifnot(check_constraints(constraints, "type", type))
  stopifnot(check_constraints(constraints, "target", target))

  return(constraints)
}

#' @export
#' @rdname combine_constraints
cc <- combine_constraints


check_constraints <- function(constraints, what, value) {
  stopifnot(length(value) %in% 0:1)
  if (is.null(value)) {
    return(TRUE)
  }
  if (is.na(value)) {
    return(all(is.na(get_all(constraints, what))))
  }
  isTRUE(all(get_all(constraints, what) == value))
}



split_constraints <- function(constraints, by) {
  base::split(constraints, f = get_all(constraints, by)) %>% lapply(add_class, "mldesign_constraints")
}
