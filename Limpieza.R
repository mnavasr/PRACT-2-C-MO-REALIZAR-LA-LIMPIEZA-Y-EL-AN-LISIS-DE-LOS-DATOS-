
library("ggmosaic")
library("ggplot2")
library("fitdistrplus")
library("MASS")
library("survival")
library("ggstatsplot")
library("tidyverse")

vuelos=read.csv("D:\\MASTER CIENCIA DE DATOS\\SEGUNDO SEMESTRE\\TIPOLOGÍA Y CICLO DE VIDA DE LOS DATOS\\RETO 2\\llegadas_aeropuerto_vlc_20251109_20251122.csv", stringsAsFactors = FALSE, colClasses = "character")
dim(vuelos)
summary(vuelos)


#3. 1. ¿Los datos contienen ceros, elementos vacíos u otros valores numéricos
#que indiquen la pérdida de datos? Gestiona cada uno de estos casos
#utilizando el método de imputación que consideres más 

problemas <- data.frame(
  NAs    = colSums(is.na(vuelos)),
  Vacios = colSums(vuelos == "", na.rm = TRUE)
)

problemas

#3.2. Identifica y gestiona adecuadamente el tipo de dato de cada atributo 

sapply(vuelos, class)

# Convertimos cada columna al tipo adecuado, SIN convertir todo el data frame a character
vuelos$fecha  <- as.Date(vuelos$fecha, format = "%Y-%m-%d")  # formato correcto ISO
vuelos$hora   <- as.character(vuelos$hora)                   # dejamos como texto
vuelos$vuelo  <- as.character(vuelos$vuelo)                 # identificador
vuelos$origen <- as.character(vuelos$origen)                # texto (categoría)
vuelos$estado <- as.factor(vuelos$estado)                   # categoría
  

sapply(vuelos, class)

#3.3. Identifica y gestiona los valores extremos.

#Vamos a comprobar uno a uno los campos 
#Las fechas deben estar comprendidas en un período de 15 días, un valor extremo sería una fecha fuera de ese rango
table(vuelos$fecha)
vuelos_extremos_fecha <- vuelos[ vuelos$fecha < as.Date("2025-11-09") |
                                  vuelos$fecha > as.Date("2025-11-22"), ]
vuelos_extremos_fecha  # muestra los que no cumplen

#Comprobamos las horas, que deben estar comprendidas entre 00:00 y 23:59
horas_fuera_rango <- !(vuelos$hora >= "00:00" & vuelos$hora <= "23:59")
sum(horas_fuera_rango)


#Comprobamos los vuelos, que deben ser identificadores que los tres primeros valores
#sean letras y los cuatro últimos números

vuelos_cod <- vuelos$vuelo
check <- substr(vuelos_cod,1,3) == toupper(substr(vuelos_cod,1,3))  # primeras 3 letras mayúsculas
vuelos_cod[!check]  # muestra los que no cumplen
# Mensaje si todos cumplen
if(all(check)) print("Todos los vuelos cumplen la estructura")

#Comprobamos que el mismo aeropuerto no aparezca duplicado con dos nombres diferentes
unique(vuelos$origen)
# Extraer los códigos de aeropuerto entre paréntesis
codigos <- gsub(".*\\((.*)\\).*", "\\1", vuelos$origen)
# Contar cuántos nombres únicos hay por código
nombres_por_codigo <- tapply(vuelos$origen, codigos, function(x) length(unique(x)))
# Mostrar solo los códigos con más de un nombre
nombres_por_codigo[nombres_por_codigo > 1]

#Comprobamos los estados, que deben ser "Aproximándose", "En vuelo", "Entrega equip.", "Finalizado" o "Informado"
unique(vuelos$estado)


#3.4. Justifica la necesidad de otros métodos de limpieza para este dataset en
#particular y, de ser necesario, aplícalos.

# Eliminar los registros que están cancelados
vuelos <- vuelos[vuelos$estado != "Cancelado", ]
# Comprobar que se eliminaron
table(vuelos$estado)

#Sabemos que hay varios vuelos cuyo origen y hora son iguales, lo que indica que son duplicados
# Ver cuántos duplicados hay
duplicados<-duplicated(vuelos[, c("hora", "origen")])
sum(duplicados)
# Eliminar duplicados
vuelos <- vuelos[!duplicados, ]
# Verificar que se eliminaron
sum(duplicados <- duplicated(vuelos[, c("hora", "origen")]))


# Categorizar la hora en franjas horarias
hora_num <- as.numeric(substr(vuelos$hora, 1, 2))
vuelos$franja_horaria <- ifelse(hora_num >= 0  & hora_num < 6,  "Madrugada",
                                 ifelse(hora_num >= 6  & hora_num < 14, "Mañana",
                                 ifelse(hora_num >= 14 & hora_num < 20, "Tarde",
                                 "Noche")))

vuelos$franja_horaria <- factor(vuelos$franja_horaria,
                                       levels = c("Madrugada","Mañana","Tarde","Noche"))
table(vuelos$franja_horaria)

#Extraer el código IATA: primeras letras del código de vuelo
vuelos$IATA <- substr(vuelos$vuelo, 1, 3)
#Extraer el número de vuelo: eliminar las letras y quedarnos con los números
vuelos$numero_vuelo <- as.numeric(gsub("[A-Z]", "", vuelos$vuelo))
head(vuelos)
