
# - NY.GDP.PCAP.PP.CD (PIB per cápita PPP, WDI)
# - AG.YLD.CREL.KG (rendimiento de cereales, WDI)
# - kcal/cap/día (OWID-FAO)

library(WDI)
library(readr)
library(dplyr)
library(here) 

cat("Iniciando descarga de datos\n")

# Ruta relativa dentro del proyecto
path_raw <- here("data", "raw")

# Crear carpeta data/raw si no existe
if (!dir.exists(path_raw)) {
  dir.create(path_raw, recursive = TRUE, showWarnings = FALSE)
  cat("Carpeta creada:", path_raw, "\n")
} else {
  cat("Carpeta ya existe:", path_raw, "\n")
}

## 1) PIB per cápita (NY.GDP.PCAP.PP.CD)

cat("\nDescargando PIB per cápita PPP (WDI)...\n")

gdp_pc <- WDI(
  country   = "all",
  indicator = "NY.GDP.PCAP.PP.CD",
  start     = 1980,
  end       = 2022,
  extra     = TRUE
)

cat("   Filas descargadas PIB:", nrow(gdp_pc), "\n")

write_csv(gdp_pc, file.path(path_raw, "gdp_pc_ppp_wdi.csv"))
cat("Guardado: ", file.path(path_raw, "gdp_pc_ppp_wdi.csv"), "\n")

## 2) Rendimiento de cereales (AG.YLD.CREL.KG)

cat("\nDescargando rendimiento de cereales (WDI)...\n")

yield_cereals <- WDI(
  country   = "all",
  indicator = "AG.YLD.CREL.KG",
  start     = 1980,
  end       = 2022,
  extra     = TRUE
)

cat("   Filas descargadas Yield:", nrow(yield_cereals), "\n")

write_csv(yield_cereals, file.path(path_raw, "yield_cereals_wdi.csv"))
cat("Guardado: ", file.path(path_raw, "yield_cereals_wdi.csv"), "\n")

## 3) Disponibilidad calórica (kcal/cap/día, OWID-FAO)

cat("\nDescargando kcal per cápita (OWID-FAO)...\n")

kcal_pc <- read_csv(
  "https://ourworldindata.org/grapher/daily-per-capita-caloric-supply.csv",
  show_col_types = FALSE
)

cat("   Filas descargadas Kcal:", nrow(kcal_pc), "\n")

write_csv(kcal_pc, file.path(path_raw, "kcal_pc_owid.csv"))
cat("Guardado: ", file.path(path_raw, "kcal_pc_owid.csv"), "\n")


cat("\nArchivos en data/raw ahora:\n")
print(list.files(path_raw))

cat("\nDescarga de datos crudos finalizada.\n")
