library(readxl)
library(tidyverse)
library(summarytools)


#Importamos la base de datos
df <- read_excel("data/handy-dirty-indices.xlsx",
    col_types = c("date", "numeric", "numeric",
        "numeric", "numeric", "numeric", "numeric", "numeric",
        "numeric", "numeric", "numeric", "numeric"), skip = 7)


#Cambiamos el nombre de las variables para facilitar el analysis
names(df) <- c("Date", "MR_Lanuf_Immingham_45", "Handy_Odessa_Agioi_30", "Handy_Lavera_Augusta_30", "Handy_Fawley_Rotterdam_30", "MR_Zawia_Milazzo_45", "MR_Zawia_Houston_45", "MR_Zawia_Amsterdam_45", "Handy_Sillamae_Amsterdam_30", "MR_Odessa_Agioi_45", "MR_Lavera_Augusta_45", "MR_Sillamae_Amsterdam_45")

#Quitamos las observaciones donde no tenemos informacion:
df <- df %>% slice(2:1147)

#Reordenamos las columnas:
df <- df %>% select(1,2,5,6:8,3,10,4,11,9,12)


#Observacion de los datos (NA):
DataExplorer::plot_missing(df)
naniar::gg_miss_upset(df)

view(summarytools::dfSummary(df))


#Plotting the data
df_MR_Lanuf_Immingham_45 <- df %>% select(1,2) %>% drop_na()
p1 <- ggplot(df_MR_Lanuf_Immingham_45,aes(x = Date, y = MR_Lanuf_Immingham_45)) +
    geom_line() +
    theme_minimal() +
    labs(title = "Gráfico 1: MR_Lanuf_Immingham_45",
       y = "$/Day") + xlab(NULL)
p1

df_Handy_Fawley_Rotterdam_30 <- df %>% select(1,3) %>% drop_na()
p2 <- ggplot(df_Handy_Fawley_Rotterdam_30,aes(x = Date, y = Handy_Fawley_Rotterdam_30)) +
    geom_line() +
    theme_minimal() +
    labs(title = "Gráfico 2: Handy_Fawley_Rotterdam_30",
       y = "$/Day") + xlab(NULL)
p2

df_MR_Zawia_Milazzo_45 <- df %>% select(1,4) %>% drop_na()
p3 <- ggplot(df_MR_Zawia_Milazzo_45,aes(x = Date, y = MR_Zawia_Milazzo_45)) +
    geom_line() +
    theme_minimal() +
    labs(title = "Gráfico 3: MR_Zawia_Milazzo_45",
       y = "$/Day") + xlab(NULL)
p3

df_MR_Zawia_Houston_45 <- df %>% select(1,5) %>% drop_na()
p4 <- ggplot(df_MR_Zawia_Houston_45,aes(x = Date, y = MR_Zawia_Houston_45)) +
    geom_line() +
    theme_minimal() +
    labs(title = "Gráfico 4: MR_Zawia_Houston_45",
       y = "$/Day") + xlab(NULL)
p4

df_MR_Zawia_Amsterdam_45 <- df %>% select(1,6) %>% drop_na()
p5 <- ggplot(df_MR_Zawia_Amsterdam_45,aes(x = Date, y = MR_Zawia_Amsterdam_45)) +
    geom_line() +
    theme_minimal() +
    labs(title = "Gráfico 5: MR_Zawia_Amsterdam_45",
       y = "$/Day") + xlab(NULL)
p5

df_Handy_Odessa_Agioi_30 <- df %>% select(1,7) %>% drop_na()
p6 <- ggplot(df_Handy_Odessa_Agioi_30,aes(x = Date, y = Handy_Odessa_Agioi_30)) +
    geom_line() +
    theme_minimal() +
    labs(title = "Gráfico 6: Handy_Odessa_Agioi_30",
       y = "$/Day") + xlab(NULL)
p6

df_MR_Odessa_Agioi_45 <- df %>% select(1,8) %>% drop_na()
p7 <- ggplot(df_MR_Odessa_Agioi_45,aes(x = Date, y = MR_Odessa_Agioi_45)) +
    geom_line() +
    theme_minimal() +
    labs(title = "Gráfico 7: MR_Odessa_Agioi_45",
       y = "$/Day") + xlab(NULL)
p7

df_Handy_Lavera_Augusta_30 <- df %>% select(1,9) %>% drop_na()
p8 <- ggplot(df_Handy_Lavera_Augusta_30,aes(x = Date, y = Handy_Lavera_Augusta_30)) +
    geom_line() +
    theme_minimal() +
    labs(title = "Gráfico 8: Handy_Lavera_Augusta_30",
       y = "$/Day") + xlab(NULL)
p8

df_MR_Lavera_Augusta_45 <- df %>% select(1,10) %>% drop_na()
p9 <- ggplot(df_MR_Lavera_Augusta_45,aes(x = Date, y = MR_Lavera_Augusta_45)) +
    geom_line() +
    theme_minimal() +
    labs(title = "Gráfico 9: MR_Lavera_Augusta_45",
       y = "$/Day") + xlab(NULL)
p9

df_Handy_Sillamae_Amsterdam_30 <- df %>% select(1,11) %>% drop_na()
p10 <- ggplot(df_Handy_Sillamae_Amsterdam_30,aes(x = Date, y = Handy_Sillamae_Amsterdam_30)) +
    geom_line() +
    theme_minimal() +
    labs(title = "Gráfico 10: Handy_Sillamae_Amsterdam_30",
       y = "$/Day") + xlab(NULL)
p10

df_MR_Sillamae_Amsterdam_45 <- df %>% select(1,12) %>% drop_na()
p11 <- ggplot(df_MR_Sillamae_Amsterdam_45,aes(x = Date, y = MR_Sillamae_Amsterdam_45)) +
    geom_line() +
    theme_minimal() +
    labs(title = "Gráfico 11: MR_Sillamae_Amsterdam_45",
       y = "$/Day") + xlab(NULL)
p11

