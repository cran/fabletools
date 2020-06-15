## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(fabletools)

## -----------------------------------------------------------------------------
#' Seasonal mean models
#' 
#' Add the rest of your documentation here.
#' Typically this includes a "Specials" section
#' 
#' @export
SMEAN <- function(formula, ...) {
  # Create a model class which combines the training method, specials, and data checks
  model_smean <- new_model_class("smean",
    # The training method (more on this later)
    train = train_smean,
    # The formula specials (the next section)
    specials = specials_smean,
    # Any checks of the unprocessed data, like gaps, ordered, regular, etc.
    check = function(.data) { 
      if (!tsibble::is_regular(.data)) stop("Data must be regular") 
    }
  )
  
  # Return a model definition which stores the user's model specification
  new_model_definition(model_smean, {{formula}}, ...)
}

## -----------------------------------------------------------------------------
specials_smean <- new_specials(
  season = function(period = NULL) {
    # Your input handling code here.
    get_frequencies(period, self$data, .auto = "smallest")
  },
  xreg = function(...) {
    # This model doesn't support exogenous regressors, time to error.
    stop("Exogenous regressors aren't supported by `SMEAN()`")
  },
  # This model requires `season()`
  # Adding this allows `SMEAN(y)` to automatically include the `season()` special
  .required_specials = "season"
)

## -----------------------------------------------------------------------------
train_smean <- function(.data, specials, ...){
  # Extract a vector of response data
  mv <- tsibble::measured_vars(.data)
  if(length(mv) > 1) stop("SMEAN() is a univariate model.")
  y <- .data[[mv]]
  
  # Pull out inputs from the specials
  if(length(specials$season) > 1) stop("The `season()` special of `SMEAN()` should only be used once.")
  m <- specials$season[[1]]
  
  # Compute the seasonal averages
  season_id <- seq(0, length(y) - 1) %% m
  season_y <- split(y, season_id)
  season_avg <- vapply(season_y, FUN = mean, FUN.VALUE = numeric(1L), 
                       USE.NAMES = FALSE)
  
  # Compute fitted values and residuals
  fit <- season_avg[season_id+1]
  e <- y - fit
  
  # Create S3 model object
  # It should be small, but contain everything needed for methods below
  structure(
    list(
      coef = season_avg,
      n = length(y),
      y_name = mv,
      fitted = fit,
      residuals = e,
      sigma2 = var(e, na.rm = TRUE)
    ),
    class = "model_smean"
  )
}

## ----first-mable--------------------------------------------------------------
fit <- tsibbledata::aus_production %>%
  model(SMEAN(Beer))
fit

## ---- echo = FALSE------------------------------------------------------------
tibble::tribble(
  ~ Method, ~ Value, ~ Description,
  "`model_sum()`", "`character(1L)`", "A short summary of the model to display in the mable",
  "`report()`", "console output", "A detailed summary of the model, similar to `summary()`",
  "`equation()`", "character(1L)", "The mathematical equation for the fitted model",
  "`forecast()`", "distribution", "Produce forecasts from the model",
  "`stream()`", "updated model", "Extend the fit of the model with additional data",
  "`generate()`", "tsibble", "Generate potential reponse values at certain times from the model",
  "`interpolate()`", "tsibble", "Interpolate missing values using the model",
  "`refit()`", "refitted model", "Apply the model to a new dataset",
  "`tidy()`", "tibble of coefficients", "Extract coefficients from the model",
  "`glance()`", "tibble of statistics", "Extract summary statistics from the model",
  "`augment()`", "tibble of data", "Augment a dataset with information from the model",
  "`components()`", "dable of components", "Extract decomposed elements from the model",
  "`fitted()`", "numeric", "Extract fitted values from the model",
  "`residuals()`", "numeric", "Extract residuals from the model"
) %>% 
  knitr::kable()

## -----------------------------------------------------------------------------
#' @importFrom fabletools model_sum
#' @export
model_sum.model_smean <- function(x){
  sprintf("SMEAN[%i]", length(x$coef))
}

fit

## -----------------------------------------------------------------------------
#' @importFrom fabletools report
#' @export
report.model_smean <- function(x){
  m <- length(x$coef)
  
  cat("\n")
  cat(paste("Seasonal period:", m))
  cat("\n\n")
  cat("Seasonal averages:\n")
  
  print.default(
    setNames(x$coef, paste0("s", seq_len(m))),
    print.gap = 2
  )
  cat(paste("\nsigma^2:", round(x$sigma2, 4), "\n"))
}

report(fit)

## -----------------------------------------------------------------------------
#' @importFrom fabletools tidy
#' @export
tidy.model_smean <- function(x){
  tibble::tibble(
    term = paste0("season_", seq_along(x$coef)), 
    estimate = x$coef
  )
}

tidy(fit)

## -----------------------------------------------------------------------------
#' @importFrom fabletools glance
#' @export
glance.model_smean <- function(x){
  tibble::tibble(
    sigma2 = x$sigma2
  )
}

glance(fit)

## -----------------------------------------------------------------------------
#' @importFrom fabletools forecast
#' @export
forecast.model_smean <- function(object, new_data, ...){
  # Extract required parameters
  h <- NROW(new_data)
  n <- object$n
  m <- length(object$coef)
  coef <- object$coef
  
  # Compute forecast variance
  season_id <- seq(0, n - 1) %% m
  season_e <- split(object$residuals, season_id)
  season_sd <- vapply(season_e, FUN = sd, FUN.VALUE = numeric(1L), 
                       USE.NAMES = FALSE, na.rm = TRUE)
  
  # Create forecast distributions
  fc_id <- (seq(0, h-1) + n %% m) %% m + 1
  mu <- coef[fc_id]
  sigma <- season_sd[fc_id]
  distributional::dist_normal(mu, sigma)
}

forecast(fit)

## -----------------------------------------------------------------------------
#' @importFrom fabletools stream
#' @export
stream.model_smean <- function(object, new_data, specials, ...){
  # Extract a vector of response data
  mv <- tsibble::measured_vars(new_data)
  y <- new_data[[mv]]
  
  # Compute the new seasonal averages
  m <- length(object$coef)
  season_id <- (seq(0, length(y) - 1) + object$n %% m) %% m
  season_y <- split(y, season_id)
  season_avg <- vapply(season_y, FUN = mean, FUN.VALUE = numeric(1L), 
                       USE.NAMES = FALSE)
  weight_new <- vapply(season_y, FUN = length, FUN.VALUE = integer(1L),
                       USE.NAMES = FALSE)
  
  # Update coefficients to include new estimates
  weight_orig <- rep(object$n %/% m, m) + c(rep(1, object$n %% m), rep(0, m - object$n %% m))
  new_coef <- (object$coef * weight_orig + season_avg * weight_new) / (weight_orig + weight_new)
  coef_change <- new_coef - object$coef
  
  # Update model
  new_fits <- new_coef[season_id+1]
  new_e <- y - new_fits
  object$coef <- new_coef
  object$fitted <- c(object$fitted + rep_len(coef_change, object$n), new_fits)
  object$residuals <- c(object$residuals - rep_len(coef_change, object$n), new_e)
  object$n <- object$n + length(y)
  object$sigma2 <- var(object$residuals, na.rm = TRUE)
  
  # Return updated model object
  object
}

us_acc_deaths <- as_tsibble(USAccDeaths)
fit_stream <- us_acc_deaths %>% 
  dplyr::slice(1:60) %>% 
  model(SMEAN(value))
report(fit_stream)

# Update the model with new data
us_acc_deaths_new <- us_acc_deaths %>% dplyr::slice(61:72)
fit_stream <- fit_stream %>% 
  stream(us_acc_deaths_new)
report(fit_stream)

# Check that it matches a model of the full data
us_acc_deaths %>% 
  model(SMEAN(value)) %>% 
  report()

## -----------------------------------------------------------------------------
#' @importFrom fabletools fitted
#' @export
fitted.model_smean <- function(object, ...){
  object$fitted
}

fitted(fit)

## -----------------------------------------------------------------------------
#' @importFrom fabletools residuals
#' @export
residuals.model_smean <- function(object, ...){
  object$residuals
}

residuals(fit)

## ---- eval=FALSE--------------------------------------------------------------
#  #' @importFrom fabletools components
#  #' @export
#  components.model_smean <- function(object, ...){
#    # Create a tsibble of the components
#    dcmp <- tibble::tibble(
#      !!object$y_name := fitted(object) + residuals(object),
#      season = fitted(object),
#      remainder = residuals(object)
#    )
#  
#    # Describe how the components combine into other columns
#    aliases <- tibble::lst(!!object$y_name := quote(season + remainder))
#  
#    # Define the behaviour of seasonal components
#    # This is used for automatic modelling of seasonal components in `decomposition_model()`
#    # It may also be used for plotting in the future.
#    seasonalities <- list(season = list(period = length(object$coef)))
#  
#    # Return a dable
#    as_dable(
#      dcmp,
#      resp = !!sym(object$y_name), method = model_sum(object),
#      seasons = seasonalities, aliases = aliases
#    )
#  }
#  
#  components(fit) # Need to store index somewhere. This workflow should improve.

