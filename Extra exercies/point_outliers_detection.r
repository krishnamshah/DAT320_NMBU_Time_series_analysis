library(dplyr)
library(imputeTS)
data("gold")
gold <- gold %>% na_locf() # impute missing values

# global LOF
library(dbscan)
glob_lof <- lof(as.matrix(gold))

# temporal windowing with z-scores (window size 10)
library(runner)
runner(gold,f = function(x){return ((x - mean(x)) / sd(x))}, # z-scores
  k = 10, at = seq(8, length(gold), by = 10),
  na_pad = FALSE) %>% unlist ()

# rolling mean (window length 5)
library(zoo)
rollapply(gold , width = 5, weighted.mean , w = c(1,1,0,1,1),fill = NA) - gold


library(forecast)

# ARIMA ( estimated)
auto.arima(gold)$residuals

# ARIMA (prediction , 1-step ahead with initial traininginterval 10)
runner(gold , f = function(x){m <- Arima(x, order = c(0,1,1))
  pred <- forecast(m, 1)$mean
  return(pred)},
  at = 10:( length(gold) - 1)) - gold [ -(1:10)]

# Histogram (bin width 30)
time_histogram <- function(ts , bin_width)
{
  bins <- c(seq(0, length(ts), by = bin_width), length(ts))
  h <- runner(ts, f = mean , at = bins , k = bin_width) %>% na.omit()
  return(mean(abs(ts - rep(h, diff(bins)))))
}
bin_width = 30
full <- time_histogram(gold , bin_width)
res <- full - sapply (1: length(gold), function(x){return(time_histogram(gold[-x], bin_width))})
