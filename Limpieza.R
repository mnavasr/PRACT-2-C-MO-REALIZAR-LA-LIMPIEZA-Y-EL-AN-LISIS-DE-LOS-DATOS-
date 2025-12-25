---
title: "¿Cómo realizar la limpieza y análisis de los datos"
author: "Eric Lechiguero Caparrós y Marta Navas Romero"
date: "Enero 2025"
output:
  pdf_document: default
  html_document: default
---
```{r packages}
library("ggplot2")
library("fitdistrplus")
library("MASS")
library("survival")
library("ggstatsplot")
library("tidyverse")
```
```{r load data}
vuelos=read.csv("D:\\MASTER CIENCIA DE DATOS\\SEGUNDO SEMESTRE\\TIPOLOGÍA Y CICLO DE VIDA DE LOS DATOS\\RETO 2\\llegadas_aeropuerto_vlc_20251109_20251122.csv", stringsAsFactors = FALSE, colClasses = "character")
dim(vuelos)
summary(vuelos)
```

sapply(vuelos, class)

# Convertimos cada columna al tipo adecuado, SIN convertir todo el data frame a character
vuelos$fecha  <- as.Date(vuelos$fecha, format = "%Y-%m-%d")  # formato correcto ISO
vuelos$hora   <- as.character(vuelos$hora)                   # dejamos como texto
vuelos$vuelo  <- as.character(vuelos$vuelo)                 # identificador
vuelos$origen <- as.character(vuelos$origen)                # texto (categoría)
vuelos$estado <- as.factor(vuelos$estado)                   # categoría
   # categoría

#Vamos a comprobar uno a uno los campos 
#Las fechas deben estar comprendidas en un período de 15 días
table(vuelos$fecha)

#Comprobamos las horas, que deben estar comprendidas entre 00:00 y 23:59
table(vuelos$hora)

#Comprobamos los vuelos, que deben ser identificadores que los tres primeros valores
#sean letras y los cuatro últimos números

vuelos_cod <- vuelos$vuelo
check <- substr(vuelos_cod,1,3) == toupper(substr(vuelos_cod,1,3))  # primeras 3 letras mayúsculas
vuelos_cod[!check]  # muestra los que no cumplen
# Mensaje si todos cumplen
if(all(check)) print("Todos los vuelos cumplen la estructura")

#Comprobamos que el mismo aeropuerto no aparezca duplicado con dos nombres diferentes
unique(vuelos$origen)
# Extraer el código entre paréntesis

codigos <- gsub(".*\\((.*)\\).*", "\\1", vuelos$origen)

# Contar cuántos nombres únicos hay por código
nombres_por_codigo <- tapply(vuelos$origen, codigos, function(x) length(unique(x)))

# Mostrar solo los códigos con más de un nombre
nombres_por_codigo[nombres_por_codigo > 1]

#Comprobamos los estados, que deben ser "Aproximándose", "En vuelo", "Entrega equip.", "Finalizado" o "Informado"
unique(vuelos$estado)

# Eliminar los registros que están cancelados
vuelos <- vuelos[vuelos$estado != "Cancelado", ]

# Comprobar que se eliminaron
table(vuelos$estado)



