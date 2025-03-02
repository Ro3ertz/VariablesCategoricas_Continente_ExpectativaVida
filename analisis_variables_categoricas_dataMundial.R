#Regresion con una variable categórica
#¿Cuánto incide el continente en la expectativa de vida de los países?

# Carga de datos
library(tidyverse)
data_mundial <- read.csv("https://bitsandbricks.github.io/data/gapminder.csv")
summary(data_mundial)

#Los datos se encuentran en perfecto estado, la limpieza no es necesaria

#Esta grafica presenta el problema de "overplotting"
data_mundial_2007 <- data_mundial %>% filter(anio == 2007)
ggplot(data = data_mundial_2007) +
  geom_point(aes(x = continente, y = expVida, color = continente)) +
  labs(title = "Expectativa de vida por continente",
       y = "expectativa de vida")

#Uso de geom_jitter
ggplot(data = data_mundial_2007) +
  geom_jitter(aes(x = continente, y = expVida, color = continente)) +
  labs(title = "Expectativa de vida por continente",
       y = "expectativa de vida")

#Uso del facetado
ggplot(data=data_mundial_2007) +
  geom_histogram(aes(x= expVida, fill = continente))+
  facet_wrap(~continente) +
  labs(title = "Expectativa de vida por continente",
       subtitle = "histogramas",
       x = "expectativa de vida",
       y = "cantidad")

#La interpretacion del modelo es la siguiente:
#Se toma con intercepcion el promedio de la expectativa de vida de Africa,
# los siguientes datos son los años más que se suman a la variable de expectativa,
# de vida por continente.
modelo_exp_continente <- lm(expVida ~ continente, data = data_mundial_2007)
modelo_exp_continente

data_mundial_2007 <- data_mundial_2007 %>% 
  mutate(residuo_ml = residuals(modelo_exp_continente))


#Durante el analisis de residuos se encontro un valor atipico que debe ser revisado
ggplot(data_mundial_2007) +
  geom_jitter(aes(x = continente, y = residuo_ml), width = 0.1) +
  geom_hline(yintercept = 0, col = "blue") +
  labs(x = "año", y = "residuo del modelo lineal")

#El pais resulto ser afganistan 
data_mundial_2007 %>%
  filter(continente=="Asia") %>%
  arrange(expVida) %>%
  head()

data_afganistan <- data_mundial %>% filter(pais == "Afghanistan")

ggplot(data_afganistan) + 
  geom_line(aes(x = anio, y = expVida)) +
  labs(title = "Expectativa de vida en Afganistán",
       y = "expectativa de vida")
