
# CONFIGURACIÓN GLOBAL DEL PROYECTO

# Opciones generales
options(stringsAsFactors = FALSE)
options(scipen = 999)

# Paquetes del proyecto
library(WDI)
library(readr)
library(here) 

# Directorio raíz del proyecto (detectado por here)
proyecto_dir <- here::here()

# Directorios de datos (según clase de organización)
dir_data_raw       <- here::here("data", "raw")
dir_data_clean     <- here::here("data", "clean")
dir_data_processed <- here::here("data", "processed")

dir_outputs_figures <- here::here("outputs", "figures")
dir_outputs_tables  <- here::here("outputs", "tables")

# Crear carpetas si no existen
dirs <- c(
  dir_data_raw,
  dir_data_clean,
  dir_data_processed,
  dir_outputs_figures,
  dir_outputs_tables
)

for (d in dirs) {
  if (!dir.exists(d)) {
    dir.create(d, recursive = TRUE)
  }
}

cat("Configuración cargada correctamente\n")
cat("   Proyecto en: ", proyecto_dir, "\n")