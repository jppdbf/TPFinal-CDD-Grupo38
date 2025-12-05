
library(readr)
library(dplyr)

path_processed <- "data/processed"

# 1) Función para la moda numérica 

get_mode <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# 2) Función para imprimir estadísticas descriptivas

imprimir_stats <- function(datos, nombre_dataset = "Dataset") {
  
  cat("ESTADÍSTICAS DESCRIPTIVAS -", nombre_dataset, "\n")
  
  
  numeric_cols <- names(datos)[sapply(datos, is.numeric)]
  
  if (length(numeric_cols) == 0) {
    cat("No hay variables numéricas en este dataset.\n\n")
    return(invisible(NULL))
  }
  
  for (col in numeric_cols) {
    x <- datos[[col]]
    
    media   <- mean(x, na.rm = TRUE)
    mediana <- median(x, na.rm = TRUE)
    moda    <- get_mode(x)
    sd_x    <- sd(x, na.rm = TRUE)
    iqr_x   <- IQR(x, na.rm = TRUE)
    
    cat("Variable:", col, "\n")
    cat("  Media:               ", media,   "\n")
    cat("  Mediana:             ", mediana, "\n")
    cat("  Moda:                ", moda,    "\n")
    cat("  Desvío estándar:     ", sd_x,    "\n")
    cat("  Rango intercuartílico:", iqr_x,  "\n\n")
  }
}

# 3) Leer las bases

gdp_proc   <- read_csv(file.path(path_processed, "gdp_processed.csv"),
                       show_col_types = FALSE)
yield_proc <- read_csv(file.path(path_processed, "yield_processed.csv"),
                       show_col_types = FALSE)
kcal_proc  <- read_csv(file.path(path_processed, "kcal_processed.csv"),
                       show_col_types = FALSE)

# 4) Aplicar la función a cada base

imprimir_stats(gdp_proc,   "PIB per cápita (gdp_processed)")
imprimir_stats(yield_proc, "Rendimiento de cereales (yield_processed)")
imprimir_stats(kcal_proc,  "Kcal per cápita (kcal_processed)")
