library(readr)
library(dplyr)
library(tidyr)

path_raw <- "data/raw"
path_processed <- "data/processed"

# Crear carpeta si no existe
if (!dir.exists(path_processed)) {
  dir.create(path_processed, recursive = TRUE)
}

# 1. Función de EDA

eda <- function(df, nombre_dataset = "Dataset") {
  
  cat("EDA:", nombre_dataset, "\n")
  
  # 1) Estructura general
  cat("1) Estructura general del dataset\n")
  cat("   - Filas:", nrow(df), "\n")
  cat("   - Columnas:", ncol(df), "\n\n")
  
  # 2) Identificación de columnas
  cat("2) Identificación de columnas\n")
  print(names(df))
  cat("\n")
  
  # 3) Tipo de datos por columna
  cat("3) Tipo de datos por columna\n")
  tipos <- sapply(df, class)
  print(tipos)
  cat("\n")
  
  # 4) Datos vacíos
  cat("4) Cantidad de valores faltantes por columna\n")
  na_tbl <- df %>%
    summarise(across(everything(), ~ sum(is.na(.)))) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "n_na")
  
  print(na_tbl)
  cat("\n")
  
  # 5) Primeras observaciones (patrones o anomalías)
  cat("5) Primeras observaciones (detección inicial de patrones / anomalías)\n")
  
  numeric_cols <- names(df)[sapply(df, is.numeric)]
  
  if (length(numeric_cols) == 0) {
    cat("   - No hay variables numéricas para analizar anomalías.\n\n")
  } else {
    for (col in numeric_cols) {
      x <- df[[col]]
      x_no_na <- x[!is.na(x)]
      
      cat("*", col, ":\n")
      
      # Proporción de valores faltantes
      prop_na <- mean(is.na(x))
      if (prop_na > 0.05) {
        cat("   - Elevada proporción de faltantes: ",
            round(prop_na * 100, 1), "%\n")
      } else if (prop_na > 0) {
        cat("   - Faltantes presentes pero no significativos: ",
            round(prop_na * 100, 1), "%\n")
      } else {
        cat("   - Sin datos faltantes.\n")
      }
      
      # Posibles anomalías:
      if (length(x_no_na) > 0) {
        p99 <- quantile(x_no_na, 0.99)
        p01 <- quantile(x_no_na, 0.01)
        
        cat("   - Rango aproximado: [",
            round(p01, 2), ", ", round(p99, 2), "] (percentiles 1% y 99%)\n")
        
        n_extremos <- sum(x_no_na < p01 | x_no_na > p99)
        
        if (n_extremos > 0) {
          cat("   - Valores potencialmente extremos (fuera del 1%-99%): ",
              n_extremos, "\n")
        } else {
          cat("   - No se detectan valores extremos relevantes.\n")
        }
      }
      
      cat("\n")
    }
  }
}

# 2. Cargar datos crudos

gdp_raw   <- read_csv(file.path(path_raw, "gdp_pc_ppp_wdi.csv"),  show_col_types = FALSE)
yield_raw <- read_csv(file.path(path_raw, "yield_cereals_wdi.csv"), show_col_types = FALSE)
kcal_raw  <- read_csv(file.path(path_raw, "kcal_pc_owid.csv"),    show_col_types = FALSE)

cat("Datos cargados desde data/raw/\n\n")


# 3. Seleccionar variables relevantes y aplicar EDA

# PIB per cápita (WDI)
gdp_sel <- gdp_raw %>%
  select(country, iso2c, iso3c, year, 
         gdp_pc_ppp = NY.GDP.PCAP.PP.CD, region, income)

eda(gdp_sel, "PIB per cápita (WDI)")

# Rendimiento de cereales (WDI)
yield_sel <- yield_raw %>%
  select(country, iso2c, iso3c, year, 
         yield_cereals = AG.YLD.CREL.KG, region, income)

eda(yield_sel, "Rendimiento de cereales (WDI)")

# Kcal per cápita (OWID-FAO)
kcal_sel <- kcal_raw %>%
  rename(
    country = Entity,
    iso3c   = Code,
    year    = Year,
    kcal_pc = `Daily calorie supply per person`
  )

eda(kcal_sel, "Kcal per cápita (OWID-FAO)")

write_csv(gdp_sel,   file.path(path_processed, "gdp_processed.csv"))
write_csv(yield_sel, file.path(path_processed, "yield_processed.csv"))
write_csv(kcal_sel,  file.path(path_processed, "kcal_processed.csv"))

cat("\nBases procesadas guardadas en data/processed/:\n")
cat(" - gdp_processed.csv\n")
cat(" - yield_processed.csv\n")
cat(" - kcal_processed.csv\n")
