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



df <- df %>% dplyr::select(1,2)


df %>% dplyr::select(-1) %>% ts(frequency = 52, start = c(2003, 6)) %>% farima(100) %>% forecast() %>% autoplot()

updatets <- df %>% dplyr::select(-1) %>% ts(frequency = 52, start = c(2003, 6))
fit_mod <- updatets %>% farima(100)

fit_mod %>% forecast() %>% plot()






plot(myts)


apply(myts, 2, adf.test)

difmyts <- diffM(myts)

plot.ts(difmyts)


VARselect(difmyts,
          type = "none",
          lag.max = 10)


#function as_tsibble 1:
Fastsibble <- function(df){
    df %>% mutate(Date = yearweek(Date)) %>% as_tsibble()
}


#function decomposition :
Fdecomp <- function(df,i){
   df %>% classical_decomposition(df[,i], type = "additive") %>% components() %>% autoplot() + labs(title = "Classical additive decomposition")
}




# function forcasting :
Fforcast <- function(df){
fit_df <- df %>% model(stlf = decomposition_model(
    STL(Handy_Odessa_Agioi_30 ~ trend(window = 7), robust = TRUE),
    NAIVE(season_adjust)
  ))

fit_df %>%  forecast() %>%
  autoplot(df) +
  labs(title = "Forecasting with classical additive decomposition of Handy_Odessa_Agioi_30")+ scale_y_continuous(breaks=seq(-60000,60000,10000))
}



# function getting residuals:
Fresiduals <- function(fit_df){
    fit_df %>% gg_tsresiduals()
}

