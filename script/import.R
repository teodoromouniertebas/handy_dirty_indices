library(readxl)
library(tidyverse)
library(rio)
library(summarytools)
library(fpp3)

#Importamos la base de datos
df <- read_excel("data/handy-dirty-indices.xlsx",
    col_types = c("date", "numeric", "numeric",
        "numeric", "numeric", "numeric", "numeric", "numeric",
        "numeric", "numeric", "numeric", "numeric"), skip = 7)

#Cambiamos el nombre de las variables para facilitar el analysis
names(df) <- c("Date", "MR_Lanuf_Immingham_45", "Handy_Odessa_Agioi_30", "Handy_Lavera_Augusta_30", "Handy_Fawley_Rotterdam_30", "MR_Zawia_Milazzo_45", "MR_Zawia_Houston_45", "MR_Zawia_Amsterdam_45", "Handy_Sillamae_Amsterdam_30", "MR_Odessa_Agioi_45", "MR_Lavera_Augusta_45", "MR_Sillamae_Amsterdam_45")

#Selection de variables interessantes:
df <- df %>% select("Date", "Handy_Odessa_Agioi_30","Handy_Fawley_Rotterdam_30","Handy_Lavera_Augusta_30")

#Quitamos las observaciones donde no tenemos informacion:
df <- df %>% drop_na()


#Exportamos :
export(df,"data/df.csv")





df_Handy_Fawley_Rotterdam_30 <- df %>% select(1,4) %>% drop_na()
p3 <- ggplot(df_Handy_Fawley_Rotterdam_30,aes(x = Date, y = Handy_Fawley_Rotterdam_30)) +
    geom_line() +
    theme_minimal() +
    labs(title = "Gráfico 3: Handy_Fawley_Rotterdam_30",
       y = "$/Day") + xlab(NULL)
p3




#df as time series and studing decomposition of the series

df <- df %>%
  select(-c(2,3))


df <- df %>%
  mutate(Date = yearweek(Date)) %>%
  as_tsibble()


autoplot(df, Handy_Lavera_Augusta_30)

df %>% model(
    classical_decomposition(Handy_Lavera_Augusta_30, type = "additive")
  ) %>%
  components() %>%
  autoplot() +
  labs(title = "Classical additive decomposition")


# Simple forcasting for Handy_Lavera_Augusta_30

fit_df <- df %>%
  model(stlf = decomposition_model(
    STL(Handy_Lavera_Augusta_30 ~ trend(window = 7), robust = TRUE),
    NAIVE(season_adjust)
  ))

fit_df %>%
  forecast() %>%
  autoplot(df) +
  labs(title = "Forecasting with classical additive decomposition of Handy_Fawley_Rotterdam_30")


fit_df %>% gg_tsresiduals()



# Simple forcasting for Handy_Lavera_Augusta_30

fit_df1 <- df1 %>%
  model(stlf = decomposition_model(
    STL(Handy_Lavera_Augusta_30 ~ trend(window = 7), robust = TRUE),
    NAIVE(season_adjust)
  ))

fit_df1 %>%
  forecast() %>%
  autoplot(df1) +
  labs(title = "Forecasting with classical additive decomposition of Handy_Lavera_Augusta_30") + scale_y_continuous(breaks=seq(-55000,55000,10000))

fit_df1 %>% gg_tsresiduals()
