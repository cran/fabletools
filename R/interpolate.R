#' Interpolate missing values
#' 
#' Uses a fitted model to interpolate missing values from a dataset.
#' 
#' @param object A mable containing a single model column.
#' @param new_data A dataset with the same structure as the data used to fit the model.
#' @param ... Other arguments passed to interpolate methods.
#' 
#' @examples 
#' if (requireNamespace("fable", quietly = TRUE)) {
#' library(fable)
#' library(tsibbledata)
#' 
#' # The fastest running times for the olympics are missing for years during 
#' # world wars as the olympics were not held.
#' olympic_running
#' 
#' olympic_running %>% 
#'   model(TSLM(Time ~ trend())) %>% 
#'   interpolate(olympic_running)
#' }
#' 
#' @rdname interpolate
#' @export
interpolate.mdl_df <- function(object, new_data, ...){
  if(length(object%@%"models") > 1){
abort("Interpolation can only be done using one model. 
Please use select() to choose the model to interpolate with.")
  }
  kv <- key_vars(object)
  object %>%
    bind_new_data(new_data) %>% 
    as_tibble %>% 
    transmute(
      !!!syms(kv),
      interpolated = map2(!!sym(object%@%"models"), new_data, interpolate, ...)
    ) %>% 
    unnest_tsbl("interpolated", parent_key = kv)
}

#' @rdname interpolate
#' @export
interpolate.mdl_ts <- function(object, new_data, ...){
  # Compute specials with new_data
  object$model$stage <- "interpolate"
  object$model$add_data(new_data)
  specials <- tryCatch(parse_model_rhs(object$model)$specials,
                       error = function(e){
                         abort(sprintf(
                           "%s
Unable to compute required variables from provided `new_data`.
Does your interpolation data include all variables required by the model?", e$message))
                       }, interrupt = function(e) {
                         stop("Terminated by user", call. = FALSE)
                       })
  
  object$model$remove_data()
  object$model$stage <- NULL
  
  resp <- map2(seq_along(object$response), object$response, function(i, resp){
    expr(object$transformation[[!!i]](!!resp))
  }) %>% 
    set_names(map_chr(object$response, as_string))
  
  new_data <- transmute(new_data, !!!resp)
  
  interpolate(object[["fit"]], new_data = new_data, specials = specials, ...)
}