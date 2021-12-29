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







