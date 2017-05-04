# ----------------------------------------------
# R course - By Erick Gordon
# ----------------------------------------------
# Date: 29Abr2017
# Tema 2: Carga y manipulacion de datos
# ----------------------------------------------
library(dplyr)
library(tidyr)
# ----------------------------------------------
# carga de archivo
data <- read.csv("./Data/2015.csv", 
                 header = TRUE, 
                 stringsAsFactors = FALSE)
str(data)
head(data, 10)
View(data)
# ----------------------------------------------
#
# operadores lógicos 
# &   (and - y)
# |   (or - o)
# ==  (igual a)
# !=  (distinto de)
# ----------------------------------------------
#
# dplyr library

# 1. top 10 
data[1:10, ]

data %>%
  top_n(10)

# --------------------
# filter : filtra los registros según las condiciones indicadas
# el país más feliz  
data %>%
  filter(Happiness.Rank == min(data$Happiness.Rank))
# el menos feliz 
data %>%
  filter(Happiness.Rank == min(data$Happiness.Rank)) %>%
  select(Country, Happiness.Score)

# --------------------
# arrange : ordena 
data %>%
  arrange(Happiness.Score) %>%
  select(Country, Happiness.Score)

data %>%
  arrange(desc(Happiness.Score)) %>%
  select(Country, Happiness.Score)

# --------------------
# distinct: selecciona valores únicos, es decir si la variable tiene 10 registros pero solo cuatro
# categorías, listará las 4. 
data %>%
  distinct(Region)

# --------------------
# mutate: Transforma una variable en otra agregandola al data.frame actual
data %>%
  mutate(se = Standard.Error * 100) %>%
  top_n(10)

# transmute: transforma a una nueva estructura, la anterior desaparece
data %>%
  transmute(se = Standard.Error * 100) %>%
  top_n(10)

# rename: cambia el nombre de una columna por otro nombre
data %>%
  rename(se = Standard.Error) %>%
  top_n(10)


# --------------------
# sumarize: Suma los valores de una variable
data %>%
  summarize(Total=n(), Maximo=max(Happiness.Score) )

# group_by: agrupa 
data %>%
  group_by(Region) %>%
  summarize(Total=n(), Maximo=max(Happiness.Score) )

data %>%
  group_by(Region) %>%
  summarize(Total=n(), Maximo=max(Happiness.Score) ) %>%
  arrange(desc(Total))

# --------------------
# sample 
# muestra aleatoria por cantidad 
data %>%
  sample_n(10)

# muestra aleatoria por fracción
data %>%
  sample_frac(0.10)


