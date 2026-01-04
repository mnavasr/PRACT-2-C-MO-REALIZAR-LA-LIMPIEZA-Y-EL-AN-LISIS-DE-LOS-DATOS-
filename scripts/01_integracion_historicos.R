# M2.851_PRACT2 - 01 - GENERACIÓN DE DATASETS HISTÓRICOS PARA ENRIQUECIMIENTO

# 1. CARGA DE LIBRERÍAS
if (!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)
library(readr)
library(stringr)


# 2. CONFIGURACIÓN DE RUTAS

# 2.0. Definimos la raíz de datos
ruta_data <- "data"

# 2.1. Ruta de datos históricos AENA
ruta_raw_aena <- file.path(ruta_data, "01_raw", "aena_historico")
ruta_comp     <- file.path(ruta_raw_aena, "COMP") # Subcarpeta aerolíneas
ruta_orgn     <- file.path(ruta_raw_aena, "ORGN") # Subcarpeta orígenes

# 2.2. Ruta de datos de mapeo
ruta_mapping  <- file.path(ruta_data, "00_mapping")

# 2.3. Ruta de datos procesados (Output donde guardaremos los resultados)
ruta_output   <- file.path(ruta_data, "02_processed")

# Informamos la composición de los archivos históricos:
message("Configuración:")
message(paste("- Datos Aerolíneas:", ruta_comp))
message(paste("- Datos Aeropuerto de destino:", ruta_orgn))
message(paste("- Datos Mapping:", ruta_mapping))


# 3. FUNCIONES AUXILIARES

# 3.1. Limpieza de Mes (Texto -> Número)
limpiar_mes <- function(texto) {
  
  # Vector con los meses
  meses <- c("enero", "febrero", "marzo", "abril", "mayo", "junio",
             "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre")
  
  # Pasar el texto a minúsculas
  texto <- tolower(texto)
  
  # Separar el texto por espacios y coger la primera palabra
  mes_texto <- sapply(strsplit(texto, " "), `[`, 1)
  
  # Buscar la posición del mes
  mes_numero <- match(mes_texto, meses)
  
  return(mes_numero)
}

# 3.2 Lectura archivos históricos (UTF-16LE)
leer_utf16 <- function(ruta, patron) {
  
  archivos <- list.files(ruta, pattern = patron, full.names = TRUE)
  if (length(archivos) == 0) stop("No se encontraron archivos")
  
  locale_es <- readr::locale(
    encoding = "UTF-16LE",
    decimal_mark = ",",
    grouping_mark = "."
  )
  
  lapply(archivos, function(archivo) {
    readr::read_csv(
      archivo,
      locale = locale_es,
      show_col_types = FALSE
    )
  }) |>
    dplyr::bind_rows()
}


# 4. GENERACIÓN DE DATASETS HISTÓRICOS

message("\n1) PROCESAR HISTÓRICOS")

# 4.1. DATASET AEROLÍNEAS:
# Cargar pasajeros:
df_comp_pax <- leer_utf16(ruta_comp, "PAX_COMP") %>%
  rename(Pasajeros = `Pasajeros Totales`) %>% 
  filter(Compañía != "Total") %>% 
  mutate(Mes = limpiar_mes(Mes))

# Cargar operaciones (vuelos):
df_comp_ope <- leer_utf16(ruta_comp, "OPE_COMP") %>%
  rename(Vuelos = `Operaciones Totales`) %>% 
  filter(Compañía != "Total") %>% 
  mutate(Mes = limpiar_mes(Mes))

# Integrar pasajeros y vuelos:
PAX_Aerolinea_Historico <- full_join(df_comp_pax, df_comp_ope, by = c("Año", "Mes", "País", "Compañía")) %>%
  mutate(
    Pasajeros = replace_na(Pasajeros, 0), 
    Vuelos = replace_na(Vuelos, 0),
    Ratio_Pax_Vuelo = ifelse(Vuelos > 0, round(Pasajeros / Vuelos, 2), 0)
  )

# 4.2. DATASET ORÍGENES:
# Carga pasajeros:
df_orgn_pax <- leer_utf16(ruta_orgn, "PAX_ORGN") %>%
  rename(Pasajeros = `Pasajeros Totales`, Aeropuerto = `Aeropuerto Escala`) %>% 
  filter(Aeropuerto != "Total") %>% 
  mutate(Mes = limpiar_mes(Mes))

# Carga operaciones (vuelos):
df_orgn_ope <- leer_utf16(ruta_orgn, "OPE_ORGN") %>%
  rename(Vuelos = `Operaciones Totales`, Aeropuerto = `Aeropuerto ORI/DES`) %>% 
  filter(Aeropuerto != "Total") %>% 
  mutate(Mes = limpiar_mes(Mes))

# Integrar pasajeros y vuelos:
PAX_Origen_Historico <- full_join(df_orgn_pax, df_orgn_ope, by = c("Año", "Mes", "País", "Aeropuerto")) %>%
  mutate(
    Pasajeros = replace_na(Pasajeros, 0), 
    Vuelos = replace_na(Vuelos, 0),
    Ratio_Pax_Vuelo = ifelse(Vuelos > 0, round(Pasajeros / Vuelos, 2), 0)
  )



# 5. ENRIQUECIMIENTO CON MAPPINGS (ICAO / IATA)
# Añadiremos los códigos ICAO e IATA a partir de los datsets generados manualmente
message("\n2) AÑADIR CÓDIGOS ICAO e IATA")

# 5.1 Mapping ICAO (Aerolíneas)
archivo_icao <- file.path(ruta_mapping, "mapping_COMP_ICAO.csv")

if(file.exists(archivo_icao)) {
  # Usamos read_delim con delim = ";" porque los archivos han sido generados manualmente
  map_icao <- read_delim(archivo_icao, delim = ";", show_col_types = FALSE)
  
  PAX_Aerolinea_Historico <- PAX_Aerolinea_Historico %>%
    left_join(map_icao, by = "Compañía") %>%
    relocate(ICAO, .after = Compañía)
    
  message(paste("ICAO integrado. Filas mapeadas:", nrow(PAX_Aerolinea_Historico)))
} else {
  warning(paste("No encontrado:", archivo_icao))
}

# 5.2 Mapping IATA (Aeropuertos)
archivo_iata <- file.path(ruta_mapping, "mapping_ORGN_IATA.csv")

if(file.exists(archivo_iata)) {
  # Usamos read_delim con delim = ";" porque los archivos han sido generados manualmente
  map_iata <- read_delim(archivo_iata, delim = ";", show_col_types = FALSE)
  
  PAX_Origen_Historico <- PAX_Origen_Historico %>%
    left_join(map_iata, by = "Aeropuerto") %>%
    relocate(IATA, .after = Aeropuerto)
    
  message(paste("IATA. Filas mapeadas:", nrow(PAX_Origen_Historico)))
} else {
  warning(paste("No encontrado:", archivo_iata))
}


# 6. EXPORTACIÓN FINAL
# Generamos los archivos históricos definitivos para enriquecer nuestro dataset scrapeado:
message("\n3) GUARDAR DATASETS FINALES")

write_csv(PAX_Aerolinea_Historico, file.path(ruta_output, "PAX_Aerolinea_Historico_Final.csv"))
write_csv(PAX_Origen_Historico, file.path(ruta_output, "PAX_Origen_Historico_Final.csv"))

message("EL PROCESO SE HA REALOZADO CON ÉXITO.")