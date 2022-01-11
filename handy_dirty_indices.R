library(tidymodels)
library(data.table)
library(tidyposterior)
library(tsibble)  #tsibble for time series based on tidy principles
library(fable)  #for forecasting based on tidy principles
library(ggfortify)  #for plotting timeseries
library(forecast)  #for forecast function
library(tseries)
library(chron)
library(lubridate)
library(directlabels)
library(zoo)
library(lmtest)
library(TTR)  #for smoothing the time series
library(MTS)
library(vars)
library(fUnitRoots)
library(lattice)
library(grid)
library(readr)
library(rio)
library(summarytools)
library(fpp3)
library(tidyverse)


df <- read_csv("df.csv")


farima <- function(x, h){
  forecast(auto.arima(x), h=h)
}

fnnetar <- function(x, h){
  forecast(nnetar(x), h = h, PI = F)
}

