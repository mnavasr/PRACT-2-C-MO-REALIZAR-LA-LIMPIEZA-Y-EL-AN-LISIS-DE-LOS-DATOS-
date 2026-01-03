install.packages("readxl")

library("ggmosaic")
library("ggplot2")
library("fitdistrplus")
library("MASS")
library("survival")
library("ggstatsplot")
library("tidyverse")
library("hms")
library("readxl")

vuelos<-read.csv("D:\\MASTER CIENCIA DE DATOS\\SEGUNDO SEMESTRE\\TIPOLOGÍA Y CICLO DE VIDA DE LOS DATOS\\RETO 2\\llegadas_aeropuerto_vlc_20251109_20251122.csv", stringsAsFactors = FALSE, colClasses = "character")
dim(vuelos)
summary(vuelos)

#Extraer el código ICAO: primeras letras del código de vuelo y el código IATA: letras entre paréntesis del campo origen
vuelos$icao_aerolinea <- substr(vuelos$vuelo, 1, 3)
vuelos$iata_aeropuerto <- substr(vuelos$origen, nchar(vuelos$origen) - 3, nchar(vuelos$origen) - 1)


#Extraer el número de vuelo: eliminar las letras y quedarnos con los números
vuelos$numero_vuelo <- as.numeric(gsub("[A-Z]", "", vuelos$vuelo))
head(vuelos)

#Cargamos el archivo de aeropuertos
airports<-read.csv("D:\\MASTER CIENCIA DE DATOS\\SEGUNDO SEMESTRE\\TIPOLOGÍA Y CICLO DE VIDA DE LOS DATOS\\RETO 2\\airports.csv", stringsAsFactors = FALSE, colClasses = "character")

#No nos interesan las columnas 1 (id) y 10 (timezone)
airports <- airports[, -c(1, 10)]

colnames(airports)
colnames(vuelos)

names(airports)[names(airports) == "icao"] <- "icao_aeropuerto"
names(airports)[names(airports) == "iata"] <- "iata_aeropuerto"

#Al hacer la limpieza de datos, hemos detectado que el aeropuerto de Sofia (SOF) aparece en el dataset de aeropuertos
#con el nombre "Sofia Airport", mientras que en el dataset de vuelos aparece simplemente como "Sofia".
#Por tanto, vamos a proceder a renombrarlo para que coincida en ambos datasets y evitar valores faltantes tras el merge.
airports$name <- gsub("Sofia Airport", "Sofia", airports$name, ignore.case = TRUE)


vuelos_completo <- merge(vuelos, airports, by = "iata_aeropuerto", all.x = TRUE)
head(vuelos_completo)
colnames(vuelos_completo)


#Para hacer el merge con las aerolíneas, hemos decidido quedarnos solamente con el código icao de la aerolínea y el nombre de la aerolínea
#ya que el resto de identificadores nos parece redundante.

airlines<-read.csv("D:\\MASTER CIENCIA DE DATOS\\SEGUNDO SEMESTRE\\TIPOLOGÍA Y CICLO DE VIDA DE LOS DATOS\\RETO 2\\airlines.csv", stringsAsFactors = FALSE, colClasses = "character")
airlines$name[airlines$name == "Ryanair"] <- "RYANAIR DAC"

airlines_sel <- airlines[, c("icao", "name", "country")]

names(airlines_sel) <- c("icao_aerolinea", "name_aerolinea", "country_aerolinea")

colnames(airlines_sel)
colnames(vuelos_completo)


vuelos_completos_final <- merge(vuelos_completo, airlines_sel, by= "icao_aerolinea", all.x = TRUE)
head(vuelos_completos_final)
colnames(vuelos_completos_final)

##TRABAJAMOS AHORA PARA INTEGRAR LOS HISTÓRICOS DE ORIGEN Y DESTINO Y COMPAÑÍA
# Cargar los datos históricos de operaciones y pasajeros por aeropuerto y compañía
historico_origen<-read.csv(
  "D:/MASTER CIENCIA DE DATOS/SEGUNDO SEMESTRE/TIPOLOGÍA Y CICLO DE VIDA DE LOS DATOS/RETO 2/PAX_Origen_Historico_Final.csv",
  sep = ",",
  stringsAsFactors = FALSE,
  check.names = FALSE,
  fileEncoding = "UTF-8"
)
historico_aerolinea <- read.csv(
  "D:/MASTER CIENCIA DE DATOS/SEGUNDO SEMESTRE/TIPOLOGÍA Y CICLO DE VIDA DE LOS DATOS/RETO 2/PAX_Aerolinea_Historico_Final.csv",
  sep = ",",
  stringsAsFactors = FALSE,
  check.names = FALSE,
  fileEncoding = "UTF-8"
)
##Ahora vamos a tratarlos para poder hacer los merges oportunos
colnames(historico_origen)
colnames(historico_aerolinea)
colnames(vuelos_completos_final)

# Renombrar columnas  para que sean iguales que nuestro dataset principal
colnames(historico_aerolinea)[colnames(historico_aerolinea) == "ICAO"] <- "icao_aerolinea"
colnames(historico_origen)[colnames(historico_origen) == "IATA"] <- "iata_aeropuerto"

#Antes de calcula la media, vamos a eliminar aquellas filas en las que el valor de vuelos es 0 y hay pasajeros, ya que
#debe de ser un error en los datos, no puede haber pasajeros si no hay vuelos.
historico_aerolinea <- historico_aerolinea %>%
  filter(!(Vuelos == 0 & Pasajeros > 0))
historico_origen <- historico_origen %>%
  filter(!(Vuelos == 0 & Pasajeros > 0))

##Queremos ahora obtener la media de PAX por aerolínea y aeropuerto sin distinción de año y mes
media_aeropuertos <- historico_origen %>%
  group_by(iata_aeropuerto) %>%                       # agrupamos solo por aeropuerto
  summarise(
    Ratio_Pax_Vuelo_medio = mean(Ratio_Pax_Vuelo, na.rm = TRUE), # media de todos los años y meses
    .groups = "drop"
  )

head(media_aeropuertos)

media_aerolineas <- historico_aerolinea %>%
  group_by(icao_aerolinea) %>%                       # agrupamos solo por aerolínea
  summarise(
    Ratio_Pax_Vuelo_medio = mean(Ratio_Pax_Vuelo, na.rm = TRUE), # media de todos los años y meses
    .groups = "drop"
  )
#Hacemos los merges oportunos para añadir la información de PAX medio al dataset final
vuelos_completos_final <- merge(
  vuelos_completos_final,
  media_aeropuertos,
  by = "iata_aeropuerto",
  all.x = TRUE
)
vuelos_completos_final <- merge(
  vuelos_completos_final,
  media_aerolineas,
  by = "icao_aerolinea",
  all.x = TRUE
)


names(vuelos_completos_final)[names(vuelos_completos_final) == "Ratio_Pax_Vuelo_medio.x"] <- "PAX_medio_aeropuerto"
names(vuelos_completos_final)[names(vuelos_completos_final) == "Ratio_Pax_Vuelo_medio.y"] <- "PAX_medio_compania"

head(vuelos_completos_final)
colnames(vuelos_completos_final)

#Finalmente, para saber el promedio de personas que viajan por cada vuelo, vamos a tener en cuenta la media de los pasajeros
#por aerolínea y aeropuerto.
vuelos_completos_final$PAX_medio_total <- rowMeans(vuelos_completos_final[, c("PAX_medio_aeropuerto", "PAX_medio_compania")], na.rm = TRUE)

# Guardar el dataset limpio y enriquecido en un nuevo archivo CSV
write_excel_csv2(vuelos_completos_final, 
                 "D:\\MASTER CIENCIA DE DATOS\\SEGUNDO SEMESTRE\\TIPOLOGÍA Y CICLO DE VIDA DE LOS DATOS\\RETO 2\\llegadas_aeropuerto_vlc_limpio_historico.csv")



#LIMPIEZA DE DATOS
#3. 1. ¿Los datos contienen ceros, elementos vacíos u otros valores numéricos
#que indiquen la pérdida de datos? Gestiona cada uno de estos casos
#utilizando el método de imputación que consideres más 

problemas <- data.frame(
  NAs    = colSums(is.na(vuelos_completos_final)),
  Vacios = colSums(vuelos_completos_final == "", na.rm = TRUE)
)

problemas

#Los valores vacíos que aparecen en name, city, country, icao_aeropuerto, latitude, longitude, altitude y tz pertencen
#al aeropuerto de BERLIN BRANDENBURG (BER) del cual no tenemos los datos en el dataset de aeropuertos pero sí la información de pasajeros medios.
vuelos_completos_final %>%
  filter(is.na(name) | is.na(city) | is.na(country) | is.na(icao_aeropuerto)) %>%
  select(vuelo, iata_aeropuerto, name, city, country, icao_aeropuerto)

#Los valores vacíos que aparecen en name_aerolinea y country_aerolinea pertenecen a aerolíneas que no están registradas en la base de datos
#de aerolíneas, por lo que no podemos imputar ningún valor. En este caso, habría que plantear cuál es la mejor solución.
vuelos_completos_final %>%
  filter(is.na(name_aerolinea) | is.na(country_aerolinea)) %>%
  select(vuelo, icao_aerolinea, name_aerolinea, country_aerolinea)

#Vamos a rellenar manualmente esos valores, ya que podemos obtener la información del nombre de la aerolínea y del país que podemos obtener
#de la página de AENA.
aerolineas_manual <- data.frame(
  icao_aerolinea = c("ANE", "AVA", "BEL", "EJU", "EZS", "ITY", "KLJ", "LHX", "WMT"),
  name_aerolinea = c(" Air Nostrum", "Avianca", "Brussels Airlines", "EasyJet", "EasyJet Switzerland", "ITA Airways", "JSC Klasjet", "Lufthansa", "Wizz Air Malta"),
  country_aerolinea = c("Spain", "Colombia", "Belgium", "United Kingdom", "Switzerland", "Italy", "Lithuania", "Germany", "Malta"),
  stringsAsFactors = FALSE
)

for(i in 1:nrow(aerolineas_manual)) {

  filas <- vuelos_completos_final$icao_aerolinea == aerolineas_manual$icao_aerolinea[i] &
           (is.na(vuelos_completos_final$name_aerolinea) |
            is.na(vuelos_completos_final$country_aerolinea))

  vuelos_completos_final$name_aerolinea[filas]   <- aerolineas_manual$name_aerolinea[i]
  vuelos_completos_final$country_aerolinea[filas] <- aerolineas_manual$country_aerolinea[i]
}

#En PAX_medio_aeropuerto
vuelos_completos_final[is.na(vuelos_completos_final$PAX_medio_aeropuerto),
                       c("origen","icao_aerolinea", "iata_aeropuerto", "PAX_medio_aeropuerto", "PAX_medio_compania")]


#Me devuelve otro fallo en un aeropuerto cuyo origen en GRX pero no aparece el nombre del aeropuerto, esto se debe a que no
#hay coincidencia en el dataset de históricos de origen, es decir, no hay registro de pasajeros y operaciones en ese aeropuerto.

historico_origen %>% 
  filter(iata_aeropuerto == "GRX")

#En PAX_medio_compañia tenemos muchos valores NA, eso sucede porque si hay 0 operaciones, no podemos tener pasajeros,
#en esos casos eliminamos esos vuelos antes de calcular la media.
vuelos_completos_final %>% 
  filter(is.na(PAX_medio_compania))

aerolineas_problematicas <- vuelos_completos_final %>%
  filter(is.na(PAX_medio_compania)) %>%
  pull(icao_aerolinea) %>%
  unique()


historico_aerolinea %>%
  filter(icao_aerolinea %in% aerolineas_problematicas) %>%
  arrange(icao_aerolinea, Año, Mes)
#COMO VEMOS NO HAY OPERACIONES ASOCIADAS A ESAS AEROLÍNEAS POR LO QUE MANTENEMOS NA EN ESE CAMPO

#Volvemos a comprobar que ya no hay valores NA ni vacíos que no deseamos
problemas_final <- data.frame(
  NAs    = colSums(is.na(vuelos_completos_final)),
  Vacios = colSums(vuelos_completos_final == "", na.rm = TRUE)
)
problemas_final

#3.2. Identifica y gestiona adecuadamente el tipo de dato de cada atributo 

sapply(vuelos_completos_final, class)

# Convertimos cada columna al tipo adecuado, sin convertir aquellas que ya se encuentran en el formato correcto
vuelos_completos_final$fecha  <- as.Date(vuelos_completos_final$fecha, format = "%Y-%m-%d")  # formato correcto ISO
vuelos_completos_final$estado <- as.factor(vuelos_completos_final$estado) 
vuelos_completos_final$icao_aerolinea <- as.factor(vuelos_completos_final$icao_aerolinea)  
vuelos_completos_final$iata_aeropuerto <- as.factor(vuelos_completos_final$iata_aeropuerto)   
vuelos_completos_final$icao_aeropuerto <- as.factor(vuelos_completos_final$icao_aeropuerto)
vuelos_completos_final$latitude  <- as.numeric(vuelos_completos_final$latitude)
vuelos_completos_final$longitude <- as.numeric(vuelos_completos_final$longitude)
vuelos_completos_final$altitude  <- as.numeric(vuelos_completos_final$altitude)
vuelos_completos_final$PAX_medio_aeropuerto <- as.numeric(vuelos_completos_final$PAX_medio_aeropuerto)
vuelos_completos_final$PAX_medio_compania <- as.numeric(vuelos_completos_final$PAX_medio_compania)
vuelos_completos_final$PAX_medio_total <- as.numeric(vuelos_completos_final$PAX_medio_total)

sapply(vuelos_completos_final, class)
#Ahora, como podemos observar, debido a las distintas procedencias de los datos, las columnas no presentan un formato homogéneo en el título
#por lo que vamos a renombrarlas para que tengan un formato uniforme y claro.

colnames(vuelos_completos_final) <- c(
  "Icao_Aerolinea",
  "Iata_Aeropuerto",
  "Fecha",
  "Hora",
  "Vuelo",
  "Origen",
  "Estado",
  "Numero_Vuelo",
  "Nombre_Aeropuerto",
  "Ciudad_Aeropuerto",
  "País_Aeropuerto",
  "Icao_Aeropuerto",
  "Latitud",
  "Longitud",
  "Altitud",
  "Zona_Horaria",
  "Nombre_Aerolinea",
  "País_Aerolinea",
  "PAX_Medio_Aeropuerto",
  "PAX_Medio_Compania",
  "PAX_Medio_Total"
)

#3.3. Identifica y gestiona los valores extremos.

#Vamos a comprobar uno a uno los campos donde es posible observar valores extremos o erróneos. 
#Las fechas deben estar comprendidas en un período de 15 días, un valor extremo sería una fecha fuera de ese rango
table(vuelos_completos_final$Fecha)
vuelos_extremos_fecha <- vuelos_completos_final[ vuelos_completos_final$Fecha < as.Date("2025-11-09") |
                                  vuelos_completos_final$Fecha > as.Date("2025-11-22"), ]
vuelos_extremos_fecha  # muestra los que no cumplen

#Comprobamos las horas, que deben estar comprendidas entre 00:00 y 23:59
horas_fuera_rango <- !(vuelos_completos_final$Hora >= "00:00" & vuelos_completos_final$Hora <= "23:59")
sum(horas_fuera_rango)

#Comprobamos los vuelos, que deben ser identificadores que los tres primeros valores
#sean letras y los cuatro últimos números

vuelos_cod <- vuelos_completos_final$Vuelo
check <- substr(vuelos_cod,1,3) == toupper(substr(vuelos_cod,1,3))  # primeras 3 letras mayúsculas
vuelos_cod[!check]  # muestra los que no cumplen
# Mensaje si todos cumplen
if(all(check)) print("Todos los vuelos cumplen la estructura")

#Comprobamos los estados, que deben ser "Aproximándose", "En vuelo", "Entrega equip.", "Finalizado" o "Informado"
unique(vuelos_completos_final$Estado)

#Comprobamos las latitudes, que deben estar comprendidas entre -90 y 90
latitudes_fuera_rango <- vuelos_completos_final$Latitud < -90 | vuelos_completos_final$Latitud > 90
sum(latitudes_fuera_rango)

#Comprobamos las longitudes, que deben estar comprendidas entre -180 y 180
longitudes_fuera_rango <- vuelos_completos_final$Longitud < -180 |
  vuelos_completos_final$Longitud > 180
sum(longitudes_fuera_rango)

#Comprobamos las altitudes
altitudes_fuera_rango <- vuelos_completos_final$Altitud < 0 |
  vuelos_completos_final$Altitud > 8850 
sum(altitudes_fuera_rango)

#Comprobamos los promedios de pasajeros por vuelo, que deben ser valores positivos
pax_negativos_aeropuerto <- vuelos_completos_final$PAX_Medio_Aeropuerto < 0
sum(pax_negativos_aeropuerto)
pax_negativos_compania <- vuelos_completos_final$PAX_Medio_Compania < 0
sum(pax_negativos_compania)

#Comprobar que icao_compañia, iata_aeropuerto e icao_aeropuerto tienen la longitud correcta
longitud_icao_compania <- nchar(as.character(vuelos_completos_final$Icao_Aerolinea)) != 3
sum(longitud_icao_compania)   
longitud_iata_aeropuerto <- nchar(as.character(vuelos_completos_final$Iata_Aeropuerto)) != 3
sum(longitud_iata_aeropuerto) 
longitud_icao_aeropuerto <- nchar(as.character(vuelos_completos_final$Icao_Aeropuerto)) != 4
sum(longitud_icao_aeropuerto)

vuelos_num <- vuelos_completos_final %>%
  select(where(is.numeric))

vuelos_long <- vuelos_num %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Valor")

ggplot(vuelos_long, aes(x = Variable, y = Valor, fill = Variable)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
  labs(title = "Boxplot de variables numéricas",
       y = "Valor", x = "") +
  theme_minimal() +
  theme(legend.position = "none")

#Se observa como no hay valores outliers extremos en las variables numéricas que nos preocupen, ya que la altitud puede deberse a un 
#aeropuerto en una zona montañosa y las latitudes y longitudes son correctas.

#3.4. Justifica la necesidad de otros métodos de limpieza para este dataset en
#particular y, de ser necesario, aplícalos.

# Eliminar los registros que están cancelados
vuelos <- vuelos_completos_final[vuelos_completos_final$Estado != "Cancelado", ]
# Comprobar que se eliminaron
table(vuelos_completos_final$Estado)

#Sabemos que hay varios vuelos cuyo origen y hora son iguales, lo que indica que son duplicados
# Ver cuántos duplicados hay
duplicados<-duplicated(vuelos_completos_final[, c("Hora", "Origen")])
sum(duplicados)
# Eliminar duplicados
vuelos_completos_final <- vuelos_completos_final[!duplicados, ]
# Verificar que se eliminaron
sum(duplicados <- duplicated(vuelos_completos_final[, c("Hora", "Origen")]))


# Categorizar la hora en franjas horarias
hora_num <- as.numeric(substr(vuelos_completos_final$Hora, 1, 2))
vuelos_completos_final$franja_horaria <- ifelse(hora_num >= 0  & hora_num < 6,  "Madrugada",
                                 ifelse(hora_num >= 6  & hora_num < 12, "Mañana",
                                 ifelse(hora_num >= 12 & hora_num < 18, "Tarde",
                                 "Noche")))

vuelos_completos_final$franja_horaria <- factor(vuelos_completos_final$franja_horaria,
                                       levels = c("Madrugada","Mañana","Tarde","Noche"))
table(vuelos_completos_final$franja_horaria)

# Guardar el dataset limpio y enriquecido en un nuevo archivo CSV
write_excel_csv2(vuelos_completos_final, 
                 "D:\\MASTER CIENCIA DE DATOS\\SEGUNDO SEMESTRE\\TIPOLOGÍA Y CICLO DE VIDA DE LOS DATOS\\RETO 2\\llegadas_aeropuerto_vlc_limpio_historico_final.csv")


##Comenzamos con la parte de análisis
#4.1. Aplica un modelo supervisado y uno no supervisado a los datos y comenta los resultados obtenidos.
#Para el modelo supervisado:
#