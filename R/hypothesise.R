#' Run a hypothesis test from a mable
#' 
#' This function will return the results of a hypothesis test for each model in 
#' the mable.
#' 
#' @param x A mable.
#' @param ... Arguments for model methods.
#' 
#' @examplesIf requireNamespace("fable", quietly = TRUE)
#' library(fable)
#' library(tsibbledata)
#' 
#' olympic_running %>%
#'   model(lm = TSLM(log(Time) ~ trend())) %>% 
#'   hypothesize()
#' 
#' @rdname hypothesize.mdl_df
#' @importFrom generics hypothesize
#' @export
hypothesize.mdl_df <- function(x, ...){
  mbl_vars <- mable_vars(x)
  x <- mutate(as_tibble(x), 
              dplyr::across(all_of(mbl_vars), function(x) lapply(x, hypothesize, ...)))
  x <- pivot_longer(x, all_of(mbl_vars), names_to = ".model", values_to = ".hypothesis")
  unnest(x, ".hypothesis")
}

#' @param tests a list of test functions to perform on the model
#' @rdname hypothesize.mdl_df
#' @export
hypothesize.mdl_ts <- function(x, tests = list(), ...){
  if(is_function(tests)){
    tests <- list(tests)
  }
  vctrs::vec_rbind(
    !!!map(tests, calc, x$fit, ...),
    .names_to = ".test"
  )
}