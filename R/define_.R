define_constraint <- function(name, expr, vars, type, target){

  list(name = name,
       expr = expr,
       vars = vars,
       type = type,
       target = target) %>%
    add_class("mldesign_constraint") %>%
    return()
}

define_constraints <- function(...){

  args <- list(...)

  if(length(args) == 0){
    list() %>% add_class("mldesign_constraints") %>% return()
  }

  stopifnot(methods::is(args, "list"))

  stopifnot(all(sapply(args, methods::is, class2="mldesign_constraint")))

  args %>% fix_names() %>%  add_class("mldesign_constraints") %>% return()
}

define_estimand <- function(name, test, relation, train){

  list(name = name,
       test = test,
       relation = relation,
       train = train) %>%
    add_class("mldesign_estimand") %>%
    add_class("mldesign_spec") %>%
    return()
}


define_splits <- function(splits){

  stopifnot(is.list(splits))

  splits %>%
    add_class("mldesign_splits") %>%
    return()
}
