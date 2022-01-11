
library(tidyverse)
library(generics)
library(fpp3)

df <- read_csv("df.csv")

df1 <- df %>%
  select(1,2)

df2 <- df %>%
  select(1,3)

df3 <- df %>%
  select(1,4)

#function as_tsibble 1:
Fastsibble <- function(df){
    df %>% mutate(Date = yearweek(Date)) %>% as_tsibble()
}


#function as_tsibble 2:
Fastsibble2 <- function(df2){
    df2 %>% mutate(Date = yearweek(Date)) %>% as_tsibble()
}

#function as_tsibble 3:
Fastsibble3 <- function(df3){
    df3 %>% mutate(Date = yearweek(Date)) %>% as_tsibble()
}

#function decomposition 1:
Fdecomp <- function(df,x){
   x <- df %>% select(all_of(x))
   df %>% model(classical_decomposition(x, type = "additive")
  ) %>% components() %>% autoplot() + labs(title = "Classical additive decomposition")
}

#function decomposition 2:
Fdecomp2 <- function(df2){
   df2 %>% model(classical_decomposition(Handy_Fawley_Rotterdam_30, type = "additive")
  ) %>% components() %>% autoplot() + labs(title = "Classical additive decomposition")
}

#function decomposition 3:
Fdecomp3 <- function(df3){
   df3 %>% model(classical_decomposition(Handy_Lavera_Augusta_30, type = "additive")
  ) %>% components() %>% autoplot() + labs(title = "Classical additive decomposition")
}


# function forcasting 1:
Fforcast1 <- function(df1){
fit_df1 <- df1 %>% model(stlf = decomposition_model(
    STL(Handy_Odessa_Agioi_30 ~ trend(window = 7), robust = TRUE),
    NAIVE(season_adjust)
  ))

fit_df1 %>%  forecast() %>%
  autoplot(df1) +
  labs(title = "Forecasting with classical additive decomposition of Handy_Odessa_Agioi_30")+ scale_y_continuous(breaks=seq(-60000,60000,10000))
}

# function forcasting 2:
Fforcast2 <- function(df2){
fit_df2 <- df2 %>% model(stlf = decomposition_model(
    STL(Handy_Fawley_Rotterdam_30 ~ trend(window = 7), robust = TRUE),
    NAIVE(season_adjust)
  ))

fit_df2 %>%  forecast() %>%
  autoplot(df2) +
  labs(title = "Forecasting with classical additive decomposition of Handy_Fawley_Rotterdam_30")+ scale_y_continuous(breaks=seq(-60000,60000,10000))
}

# function forcasting 3:
Fforcast3 <- function(df3){
fit_df3 <- df3 %>% model(stlf = decomposition_model(
    STL(Handy_Lavera_Augusta_30 ~ trend(window = 7), robust = TRUE),
    NAIVE(season_adjust)
  ))

fit_df3 %>%  forecast() %>%
  autoplot(df3) +
  labs(title = "Forecasting with classical additive decomposition of Handy_Lavera_Augusta_30")+ scale_y_continuous(breaks=seq(-60000,60000,10000))
}



# function getting residuals:
Fresiduals <- function(fit_df){
    fit_df %>% gg_tsresiduals()
}


############################################################

df %>% Fastsibble() %>% Fdecomp(x="Handy_Lavera_Augusta_30")







