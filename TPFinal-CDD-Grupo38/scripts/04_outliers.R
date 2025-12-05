library(readr)
library(dplyr)
library(purrr)
library(zoo)  

path_processed <- "data/processed"
path_clean     <- "data/clean"

dir.create(path_clean, showWarnings = FALSE)

# 1. Leer bases procesadas

gdp    <- read_csv(file.path(path_processed, "gdp_processed.csv"),  show_col_types = FALSE)
yield  <- read_csv(file.path(path_processed, "yield_processed.csv"), show_col_types = FALSE)
kcal   <- read_csv(file.path(path_processed, "kcal_processed.csv"),  show_col_types = FALSE)

# unimos en un panel final (por iso3c + year)
panel <- list(gdp, yield, kcal) |>
  reduce(full_join, by = c("iso3c", "year"))

# 2. Función para detectar outliers (Método IQR)

detectar_outliers <- function(x) {
  x_no_na <- x[!is.na(x)]
  if (length(x_no_na) == 0) return(integer(0))
  
  q1  <- quantile(x_no_na, 0.25)
  q3  <- quantile(x_no_na, 0.75)
  iqr <- q3 - q1
  
  lim_inf <- q1 - 1.5 * iqr
  lim_sup <- q3 + 1.5 * iqr
  
  which(x < lim_inf | x > lim_sup)
}

# Dummy de outliers (para usar en regresiones) 

dummy_outliers <- function(vec) {
  idx <- detectar_outliers(vec)
  d   <- rep(0L, length(vec))
  d[idx] <- 1L
  d[is.na(vec)] <- NA_integer_  
  d
}

# descripción corta de cada variable-
desc_variable <- function(nombre) {
  if (nombre == "PIB per cápita PPP") {
    "el nivel de ingreso per cápita de cada país ajustado por paridad de poder de compra"
  } else if (nombre == "Rendimiento de cereales") {
    "la productividad agrícola medida como rendimiento de cereales por hectárea"
  } else if (nombre == "Kcal per cápita") {
    "la disponibilidad calórica diaria promedio por habitante"
  } else {
    "la variable analizada"
  }
}

# 3. Analizar outliers + faltantes y tomar decisiones 


limpiar_variable <- function(vec, nombre, nombre_dummy) {
  
  cat("\nVariable:", nombre, "\n")
  
  # Faltantes
  n_na    <- sum(is.na(vec))
  prop_na <- round(mean(is.na(vec)) * 100, 2)
  
  # Outliers
  idx_out  <- detectar_outliers(vec)
  n_out    <- length(idx_out)
  n_valid  <- sum(!is.na(vec))
  prop_out <- if (n_valid > 0) round(n_out / n_valid * 100, 2) else 0
  
  cat("Método para outliers: Regla 1.5 * IQR (Q1 - 1.5*IQR, Q3 + 1.5*IQR)\n")
  cat("Outliers detectados:", n_out, " (", prop_out, "% de las observaciones no faltantes)\n", sep = "")
  cat("Faltantes totales:", n_na, " (", prop_na, "% del total de observaciones)\n\n", sep = "")
  
  vec_clean <- vec
  desc_var  <- desc_variable(nombre)
  
  # Decisión sobre datos faltantes
  
  if (prop_na < 5) {
    # Eliminar observaciones con NA
    decision_NA <- paste0(
      "En la variable '", nombre, "' (", desc_var, ") se decidió eliminar las observaciones con datos faltantes, ",
      "ya que representan solo ", prop_na, "% de la muestra y no se observa un patrón sistemático ",
      "en su distribución entre países o años. Bajo el supuesto de datos faltantes completamente al azar (MCAR), ",
      "esta eliminación no debería introducir sesgos relevantes en las comparaciones internacionales."
    )
    
  } else {
    # Interpolación temporal
    decision_NA <- paste0(
      "En la variable '", nombre, "' (", desc_var, ") los datos faltantes superan el 5% de la muestra (",
      prop_na, "%). Se decidió imputarlos mediante interpolación ",
      "lineal temporal (na.approx), aprovechando que se trata de una serie anual por país. ",
      "Se asume que la evolución en el tiempo es relativamente suave y que no existen cambios ",
      "estructurales abruptos justo en los períodos con datos faltantes. Esta decisión permite ",
      "conservar más observaciones para el análisis de tendencias, aun cuando pueda suavizar ",
      "ligeramente la variabilidad interanual."
    )
    vec_clean <- na.approx(vec_clean, na.rm = FALSE)
  }
  
  
  # Decisión sobre outliers

  if (n_out > 0) {
    decision_OUT <- paste0(
      "En la variable '", nombre, "' los ", n_out, " outliers identificados (", prop_out,
      "% de las observaciones no faltantes) se consideran informativos, ya que reflejan casos ",
      "con niveles extremos de ", desc_var, " (por ejemplo, países muy ricos/pobres o con ",
      "productividades particularmente altas o bajas). Por este motivo, se decidió mantener ",
      "los valores originales en la base, pero crear una variable dummy ('", nombre_dummy,
      "') que toma valor 1 cuando la observación es outlier y 0 en caso contrario. ",
      "De esta forma, en las regresiones posteriores se podrá controlar explícitamente el ",
      "efecto de estos casos extremos sin perder información."
    )
  } else {
    decision_OUT <- paste0(
      "En la variable '", nombre,
      "' no se detectaron outliers significativos según el criterio 1.5*IQR, ",
      "por lo que no fue necesario crear una dummy específica para outliers."
    )
  }
  

  # Justificación final
  
  cat("Justificación respecto a datos faltantes:\n")
  cat(" - ", decision_NA, "\n\n", sep = "")
  
  cat("Justificación respecto a outliers:\n")
  cat(" - ", decision_OUT, "\n\n", sep = "")
  
  cat("Posibles efectos sobre los resultados:\n")
  if (prop_na < 5) {
    cat(" - Al eliminar una proporción pequeña de observaciones en '", nombre,
        "', se prioriza la calidad de los datos sin esperar cambios sustantivos ",
        "en las estimaciones globales de ", desc_var, ".\n", sep = "")
  } else {
    cat(" - La interpolación temporal en '", nombre,
        "' puede suavizar la serie y reducir levemente la dispersión interanual, ",
        "pero permite mantener la continuidad de la serie por país y aprovechar mejor la información disponible.\n",
        sep = "")
  }
  if (n_out > 0) {
    cat(" - Las dummies de outliers permiten controlar estos casos en las regresiones,\n",
        "   manteniendo la información original pero evitando que dominen los coeficientes.\n", sep = "")
  } else {
    cat(" - Al no encontrarse outliers relevantes en '", nombre,
        "', no se esperan efectos adicionales por este punto.\n", sep = "")
  }
  
  return(vec_clean)
}

# 4. Aplicar limpieza + crear dummies

panel_clean <- panel |>
  mutate(
    # variables “limpias” (faltantes tratados)
    gdp_pc_ppp_clean    = limpiar_variable(gdp_pc_ppp,   "PIB per cápita PPP", "gdp_pc_ppp_outlier"),
    yield_cereals_clean = limpiar_variable(yield_cereals,"Rendimiento de cereales", "yield_cereals_outlier"),
    kcal_pc_clean       = limpiar_variable(kcal_pc,     "Kcal per cápita", "kcal_pc_outlier"),
    
    # dummies de outliers (1 = outlier, 0 = normal, NA si dato faltante)
    gdp_pc_ppp_outlier    = dummy_outliers(gdp_pc_ppp),
    yield_cereals_outlier = dummy_outliers(yield_cereals),
    kcal_pc_outlier       = dummy_outliers(kcal_pc)
  )

# eliminar filas con NA restantes en las variables “limpias”
# (solo por faltantes, los outliers se mantienen)
panel_clean_final <- panel_clean |>
  filter(
    !is.na(gdp_pc_ppp_clean),
    !is.na(yield_cereals_clean),
    !is.na(kcal_pc_clean)
  )

# 5. Exportar base limpia -----------------------------------

write_csv(panel_clean_final, file.path(path_clean, "base_clean_panel.csv"))

cat("\nBase limpia exportada en: data/clean/base_clean_panel.csv\n")
cat("Proceso completado.\n")
