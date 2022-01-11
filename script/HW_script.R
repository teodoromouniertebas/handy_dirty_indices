
#Script with the data of Indices and the code of the book of Hadley whicham (https://es.r4ds.hadley.nz/muchos-modelos.html)

library(modelr)
library(tidyverse)
library(datos)

#Importing Data:
paises <- paises
df <- read_csv("df.csv")

#Plotting the data
paises %>%
  ggplot(aes(anio, esperanza_de_vida, group = pais)) +
  geom_line(alpha = 1 / 3)

df %>%
  ggplot() +
  geom_line(aes(Date,Handy_Odessa_Agioi_30)) +
  geom_line(aes(Date,Handy_Fawley_Rotterdam_30)) +
  geom_line(aes(Date,Handy_Lavera_Augusta_30))


#Para un Pais :
nz <- filter(paises, pais == "Nueva Zelanda")
nz %>%
  ggplot(aes(anio, esperanza_de_vida)) +
  geom_line() +
  ggtitle("Datos completos = ")

nz_mod <- lm(esperanza_de_vida ~ anio, data = nz)
nz %>%
  add_predictions(nz_mod) %>%
  ggplot(aes(anio, pred)) +
  geom_line() +
  ggtitle("Tendencia lineal + ")

nz %>%
  add_residuals(nz_mod) %>%
  ggplot(aes(anio, resid)) +
  geom_hline(yintercept = 0, colour = "white", size = 3) +
  geom_line() +
  ggtitle("Patrón restante")

#Para un indice
HOA <- df %>% select(c("Date","Handy_Odessa_Agioi_30"))
HOA %>%
  ggplot(aes(Date, Handy_Odessa_Agioi_30)) +
  geom_line() +
  ggtitle("Datos completos = ")

HOA_mod <- lm(Handy_Odessa_Agioi_30 ~ Date, data = HOA)
HOA %>%
  add_predictions(HOA_mod) %>%
  ggplot(aes(Date, pred)) +
  geom_line() +
  ggtitle("Tendencia lineal - ")

HOA %>%
  add_residuals(HOA_mod) %>%
  ggplot(aes(Date, resid)) +
  geom_hline(yintercept = 0, colour = "white", size = 3) +
  geom_line() +
  ggtitle("Patrón restante")


#Datos anididos:
por_pais <- paises %>%
  group_by(pais, continente) %>%
  nest()
a <- paises %>%
  nest(data = anio:pib_per_capita)





#aplicar un modelo :
library(forecast)
library(smooth)
df1 <- df %>% select(1:2)
fit = auto.ces(df1$Handy_Odessa_Agioi_30)
pred = forecast(fit, 200)
plot(pred)

fit = auto.arima(df1$Handy_Odessa_Agioi_30)
pred = forecast(fit, 200)
plot(pred)
