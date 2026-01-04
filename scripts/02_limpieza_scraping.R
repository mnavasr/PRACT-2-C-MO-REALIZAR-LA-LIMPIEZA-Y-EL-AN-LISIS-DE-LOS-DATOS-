# M2.851_PRACT2 - 02 - LIMPIEZA E INTEGRACIÓN


# 1. LIBRERÍAS
if (!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)
library(readr)
library(lubridate)


# 2. CONFIGURACIÓN DE RUTAS

ruta_data      <- "data"
ruta_raw_scrap <- file.path(ruta_data, "01_raw", "scraping")
ruta_mapping   <- file.path(ruta_data, "00_mapping")
ruta_processed <- file.path(ruta_data, "02_processed")

message("Configuración:")
message(paste("- Input Scraping:", ruta_raw_scrap))
message(paste("- Output Final:  ", ruta_processed))


# 3. CARGA DE DATOS

message("\n1) Carga de datos")

# A. Datos del Scraping
archivo_scraping <- list.files(ruta_raw_scrap, pattern = "llegadas_aeropuerto_vlc.*\\.csv$", full.names = TRUE)[1]
if (is.na(archivo_scraping)) stop("No se han encontrado archivos obtenidos mediante web scraping")

# Leemos forzando caracteres para no perder ceros iniciales
vuelos <- read_csv(archivo_scraping, col_types = cols(.default = "c"))

# B. Maestros
airports <- read_csv(file.path(ruta_mapping, "airports.csv"), show_col_types = FALSE)
airlines <- read_csv(file.path(ruta_mapping, "airlines.csv"), show_col_types = FALSE)

# C. Históricos
historico_origen    <- read_csv(file.path(ruta_processed, "PAX_Origen_Historico_Final.csv"), show_col_types = FALSE)
historico_aerolinea <- read_csv(file.path(ruta_processed, "PAX_Aerolinea_Historico_Final.csv"), show_col_types = FALSE)


# 4. PRE-LIMPIEZA

message("\n2) Pre-limpieza del dataset scrapeado")

filas_iniciales <- nrow(vuelos)

vuelos <- vuelos %>%
  # A. Convertir fecha para poder distinguir los días distintos
  mutate(fecha = ymd(fecha)) %>% 
  
  # B. Eliminar vuelos cancelados
  filter(estado != "Cancelado") %>%
  
  # C. Eliminar vuelos duplicados
  # Un vuelo es único por su fecha + hora + origen
  # Usamos distinct para quedarnos con una sola fila por día/hora/origen
  distinct(fecha, hora, origen, .keep_all = TRUE)

filas_finales <- nrow(vuelos)
message(paste("Vuelos iniciales:", filas_iniciales))
message(paste("Vuelos tras la limpieza:", filas_finales))
message(paste("Registros eliminados:", filas_iniciales - filas_finales))


# 5. EXTRACCIÓN DE CLAVES Y LIMPIEZA DE MAESTROS

message("\n3) Extracción de claves y normalización")

# 5.1. Extracción de códigos
vuelos <- vuelos %>%
  mutate(
    icao_aerolinea = str_sub(vuelo, 1, 3),
    iata_aeropuerto = str_extract(origen, "(?<=\\()[A-Z]{3}(?=\\))"),
    numero_vuelo = as.numeric(str_extract(vuelo, "\\d+")),
    mes = month(fecha)
  )

# 5.2. Limpieza de datasets maestros
airports_clean <- airports %>%
  select(-id, -timezone) %>%
  rename(icao_aeropuerto = icao, iata_aeropuerto = iata) %>%
  mutate(name = str_replace(name, regex("Sofia Airport", ignore_case = TRUE), "Sofia"))

airlines_clean <- airlines %>%
  rename(icao_aerolinea = icao, name_aerolinea = name, country_aerolinea = country) %>%
  select(icao_aerolinea, name_aerolinea, country_aerolinea) %>%
  mutate(name_aerolinea = ifelse(name_aerolinea == "Ryanair", "RYANAIR DAC", name_aerolinea))


# 6. ENRIQUECIMIENTO (JOIN MAESTROS)

message("\n-4) Integración de datos")

# Merge con datasets maestros
vuelos_enrich <- left_join(vuelos, airports_clean, by = "iata_aeropuerto")
vuelos_enrich <- left_join(vuelos_enrich, airlines_clean, by = "icao_aerolinea")

# Imputación manual (correcciones faltantes en maestros)
correcciones <- tibble(
  icao_aerolinea = c("ANE", "AVA", "BEL", "EJU", "EZS", "ITY", "KLJ", "LHX", "WMT"),
  nuevo_nombre   = c("Air Nostrum", "Avianca", "Brussels Airlines", "EasyJet", "EasyJet Switzerland", 
                     "ITA Airways", "JSC Klasjet", "Lufthansa", "Wizz Air Malta"),
  nuevo_pais     = c("Spain", "Colombia", "Belgium", "United Kingdom", "Switzerland", 
                     "Italy", "Lithuania", "Germany", "Malta")
)

vuelos_enrich <- vuelos_enrich %>%
  left_join(correcciones, by = "icao_aerolinea") %>%
  mutate(
    name_aerolinea = coalesce(name_aerolinea, nuevo_nombre),
    country_aerolinea = coalesce(country_aerolinea, nuevo_pais)
  ) %>%
  select(-nuevo_nombre, -nuevo_pais)


# 7. INTEGRACIÓN HISTÓRICA Y ESTIMACIÓN

message("\n5) Estimación de pasajeros")

# Preparar Históricos (Evitar ceros)
hist_origen_clean <- historico_origen %>% 
  filter(!(Vuelos == 0 & Pasajeros > 0)) %>%
  group_by(Aeropuerto) %>% 
  summarise(Ratio_Pax_Origen = mean(Ratio_Pax_Vuelo, na.rm = TRUE)) %>%
  rename(iata_aeropuerto = Aeropuerto)

hist_aero_clean <- historico_aerolinea %>% 
  filter(!(Vuelos == 0 & Pasajeros > 0)) %>%
  group_by(ICAO) %>% # OJO: Asegúrate que la columna del CSV histórico se llama ICAO
  summarise(Ratio_Pax_Aerolinea = mean(Ratio_Pax_Vuelo, na.rm = TRUE)) %>%
  rename(icao_aerolinea = ICAO)

# Join y Cálculo
vuelos_final <- vuelos_enrich %>%
  left_join(hist_origen_clean, by = "iata_aeropuerto") %>%
  left_join(hist_aero_clean, by = "icao_aerolinea") %>%
  rowwise() %>%
  mutate(
    PAX_Estimado = mean(c(Ratio_Pax_Origen, Ratio_Pax_Aerolinea), na.rm = TRUE),
    PAX_Estimado = round(PAX_Estimado)
  ) %>%
  ungroup()


# 8. FORMATO FINAL Y EXPORTACIÓN

message("\n--- 6. Formato Final ---")

vuelos_final <- vuelos_final %>%
  mutate(
    hora_num = as.numeric(substr(hora, 1, 2)),
    franja_horaria = case_when(
      hora_num >= 0 & hora_num < 6 ~ "Madrugada",
      hora_num >= 6 & hora_num < 12 ~ "Mañana",
      hora_num >= 12 & hora_num < 18 ~ "Tarde",
      TRUE ~ "Noche"
    )
  ) %>%
  select(
    Fecha = fecha,
    Hora = hora,
    Vuelo = vuelo,
    Origen = origen,
    Estado = estado,
    Aerolinea = name_aerolinea,
    Pais_Aerolinea = country_aerolinea,
    Aeropuerto = name,
    Ciudad = city,
    Pais_Origen = country,
    IATA = iata_aeropuerto,
    ICAO_Cia = icao_aerolinea,
    PAX_Estimado,
    Franja_Horaria = franja_horaria
  )

archivo_salida <- file.path(ruta_processed, "Dataset_Final_Integrado.csv")
write_csv(vuelos_final, archivo_salida)

message(paste("¡ÉXITO! Dataset limpio guardado en:", archivo_salida))
glimpse(vuelos_final)