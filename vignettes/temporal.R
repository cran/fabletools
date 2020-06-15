## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(fabletools)

## -----------------------------------------------------------------------------
library(ggplot2)
library(lubridate)
library(tidyr)
granular <- tibble::tibble(
  interval = ordered(c("hour", "day", "week", "fortnight", "month"), levels = c("hour", "day", "week", "fortnight", "month")),
  times = list(
    seq(ymd_hms("1970-01-01 00:00:00"), ymd_hms("1970-03-01 00:00:00"), by = "1 hour"),
    seq(ymd_hms("1970-01-01 00:00:00"), ymd_hms("1970-03-01 00:00:00"), by = "1 day"),
    seq(ymd_hms("1970-01-01 00:00:00"), ymd_hms("1970-03-01 00:00:00"), by = "1 week"),
    seq(ymd_hms("1970-01-01 00:00:00"), ymd_hms("1970-03-01 00:00:00"), by = "2 weeks"),
    seq(ymd_hms("1970-01-01 00:00:00"), ymd_hms("1970-03-01 00:00:00"), by = "1 month")
  )
) %>% 
  unnest(times)

granular %>% 
  ggplot(aes(x = times, y = interval)) + 
  geom_point()

