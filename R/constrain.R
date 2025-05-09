#' @name constrain
#'
#' @title Specify constraints on data sets or observations
#' @description
#' A short description...
#'
#' @param expr (character | formula) \cr ...
#' @param name (character) \cr ...
#' @param vars (character) \cr ...
#' @param target (character) \cr ...
#' @param type (character) \cr ...
#'
#' @return (mldesign_constraint) \cr ...
#' @export
#'
#' @examples
#' constrain("test$age > 18", type = "population")
#' constrain(~ (test$year - train$year) %>% dplyr::between(1, 3), type = "context")
#' constrain("nrow(train) > 100")

#' @rdname constrain
#' @export
constrain <- function(expr,
                      name = to_character(expr),
                      vars = detect_vars(expr),
                      target = detect_target(expr),
                      type = c(NA, "context", "population", "admissibility")) {
  expr <- preproc_expr(expr = expr, target = target)

  stopifnot(is.character(name))
  stopifnot(length(name) == 1)

  stopifnot(is.character(vars) | is.null(vars))

  type <- match.arg(type)
  target <- match.arg(target)

  ## return
  define_constraint(name = name, expr = expr, vars = vars, type = type, target = target) %>%
    return()
}

#' @export
#' @rdname constrain
co <- constrain


detect_target <- function(expr) {
  expr <- to_character(expr)

  ## check if correct datasets are included in expr:
  test_keyword <- "test"
  train_keyword <- "train"

  test_incl <- grepl(test_keyword, expr)
  train_incl <- grepl(train_keyword, expr)

  result <- c("test", "relation", "train")[
    c(test_incl & (!train_incl), test_incl & train_incl, (!test_incl) & train_incl)
  ]

  if (length(result) == 0) {
    stop(paste0(
      "[mldesign] expr needs to contain keywords '",
      train_keyword, "' or/and '", test_keyword, "'."
    ))
  }

  return(result)
}





#' @importFrom stats as.formula
to_character <- function(expr) {
  ## make sure expr is a admissible character:
  stopifnot(methods::is(expr, "character") | methods::is(expr, "formula"))
  if (methods::is(expr, "character")) {
    ## if expr does not start with '~', add it to string:
    if (!grepl("^~", trimws(expr))) {
      expr <- paste("~", expr)
    }
    expr <- stats::as.formula(expr)
  }
  expr <- as.character(expr)[2]
  if (grepl("~", expr)) {
    ## warn if there is still an additional '~' character left:
    warning("[mldesign] additional '~' detected in expr; only RHS formula or string (without '~') allowed.")
  }

  return(expr)
}

#' Preprocess expr argument in constrain()
#'
#' @param expr (character | formula) expression to be checked
#' @param target (character) \cr either "test", "relation" or "train"
#'
#' @return (character) \cr the preprocessed expression
preproc_expr <- function(expr, target = c("test", "relation", "train")) {
  ## argument checks:
  target <- match.arg(target)

  expr <- to_character(expr)

  ## check if correct datasets are included in expr:
  ok <- target == detect_target(expr)

  if (!ok) {
    stop("[mldesign] (not) detected keywords 'train' or 'test' that were (not) anticipated - see ?constrain...")
  }

  return(expr)
}



detect_vars <- function(expr, warn = FALSE) {
  ## case 1 - "df[['xyz']]" syntax:
  vars1 <-
    stringr::str_match_all(expr, "\\[\\[(.*?)\\]\\]")[[1]][, 2] %>%
    stringr::str_remove_all("\"") %>%
    stringr::str_remove_all("'")

  ## case 2 - "df$xyz" syntax:
  vars2 <- stringr::str_match_all(expr, "\\$(.*?)(\\s|$)")[[1]][, 2]

  ## compile results:
  vars <- sort(unique(c(vars1, vars2)))

  return(vars)
}



eval_expr <- function(expr, data) {
  eval(parse(text = expr), envir = data)
}
