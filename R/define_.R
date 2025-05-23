define_constraint <- function(name, expr, vars, type, target) {
  list(
    name = name,
    expr = expr,
    vars = vars,
    type = type,
    target = target
  ) %>%
    add_class("mldesign_constraint") %>%
    return()
}


define_constraints <- function(...) {
  args <- list(...)

  if (length(args) == 0) {
    list() %>%
      add_class("mldesign_constraints") %>%
      return()
  }

  stopifnot(methods::is(args, "list"))

  stopifnot(all(sapply(args, methods::is, class2 = "mldesign_constraint")))

  args %>%
    fix_names() %>%
    add_class("mldesign_constraints") %>%
    return()
}


define_estimand <- function(name, test, relation, train) {
  list(
    name = name,
    test = test,
    relation = relation,
    train = train
  ) %>%
    add_class("mldesign_estimand") %>%
    add_class("mldesign_spec")
}


define_method <- function(name, method, args) {
  list(
    name = name,
    method = method,
    args = args
  ) %>%
    add_class("mldesign_method") %>%
    add_class("mldesign_spec")
}


define_nested <- function(name, outer, inner) {
  list(
    name = name,
    outer = outer,
    inner = inner
  ) %>%
    add_class("mldesign_nested") %>%
    add_class("mldesign_spec")
}


define_splits <- function(info = NULL, sets = NULL, splits) {
  ## input checks:
  # check if either sets or splits argument is supplied as list:
  stopifnot(is.list(sets) | is.list(splits))
  stopifnot(is.null(sets) | is.null(splits))

  # if info is NULL, then derive it based on splits automatically:
  if (is.null(info)) {
    stopifnot(is.list(splits))

    info <- data.table::data.table(
      idx_split = seq_along(splits),
      idx_outer = seq_along(splits),
      idx_inner = 0,
      type = "outer",
      type_test = "outer",
      type_train = "outer",
      idx_train = seq_along(splits),
      idx_test = seq_along(splits) + length(splits),
      n_train = sapply(splits, \(x) length(x$train)),
      n_test = sapply(splits, \(x) length(x$test))
    )
  }
  stopifnot(is.data.frame(info))

  ## compile result:
  list(info = info, sets = sets, splits = splits) %>%
    add_class("mldesign_splits") %>%
    return()
}
