library(readr)
library(dplyr)
library(purrr)
library(tidyr)
library(zoo)

path_raw   <- "data/raw"
path_clean <- "data/clean"

# 1. LEER BASES RAW 

gdp_raw <- read_csv(file.path(path_raw, "gdp_pc_ppp_wdi.csv"), show_col_types = FALSE) %>%
  rename(
    gdp_pc_ppp = `NY.GDP.PCAP.PP.CD`
  )

kcal_raw <- read_csv(file.path(path_raw, "kcal_pc_owid.csv"), show_col_types = FALSE) %>%
  rename(
    iso3c = Code,
    year  = Year,
    kcal_pc = `Daily calorie supply per person`
  )

yield_raw <- read_csv(file.path(path_raw, "yield_cereals_wdi.csv"), show_col_types = FALSE) %>%
  rename(
    yield_cereals = `AG.YLD.CREL.KG`
  )

# panel RAW con las 3 variables
panel_raw <- list(gdp_raw, kcal_raw, yield_raw) %>%
  reduce(full_join, by = c("iso3c", "year"))

# 2. LEER BASE CLEAN DE data/clean

panel_clean <- read_csv(
  file.path(path_clean, "base_clean_panel.csv"),
  show_col_types = FALSE
)

# 3. FUNCIÓN DE ESTADÍSTICAS

moda_num <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  tab <- table(x)
  as.numeric(names(tab)[which.max(tab)])
}

vars <- c("gdp_pc_ppp", "kcal_pc", "yield_cereals")

calc_stats <- function(df, nombre_base) {
  df %>%
    select(any_of(vars)) %>%
    pivot_longer(cols = everything(),
                 names_to = "variable",
                 values_to = "valor") %>%
    group_by(variable) %>%
    summarise(
      n       = sum(!is.na(valor)),
      media   = mean(valor, na.rm = TRUE),
      mediana = median(valor, na.rm = TRUE),
      moda    = moda_num(valor),
      sd      = sd(valor, na.rm = TRUE),
      iqr     = IQR(valor, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(base = nombre_base, .before = 1)
}

# 4. CALCULAR ESTADÍSTICAS RAW Y CLEAN

stats_raw   <- calc_stats(panel_raw, "raw")
stats_clean <- calc_stats(panel_clean, "clean")

stats_largo <- bind_rows(stats_raw, stats_clean)

stats_comparacion <- stats_largo %>%
  pivot_wider(
    id_cols = variable,
    names_from = base,
    values_from = c(n, media, mediana, moda, sd, iqr),
    names_sep = "_"
  ) %>%
  mutate(
    delta_media   = media_clean   - media_raw,
    delta_mediana = mediana_clean - mediana_raw,
    delta_moda    = moda_clean    - moda_raw,
    delta_sd      = sd_clean      - sd_raw,
    delta_iqr     = iqr_clean     - iqr_raw,
    
    rel_media   = 100 * delta_media   / media_raw,
    rel_mediana = 100 * delta_mediana / mediana_raw,
    rel_sd      = 100 * delta_sd      / sd_raw,
    rel_iqr     = 100 * delta_iqr     / iqr_raw
  )

# 5. IMPRIMIR RESULTADOS

cat("\nESTADÍSTICAS RAW:\n")
print(stats_raw)

cat("\nESTADÍSTICAS CLEAN:\n")
print(stats_clean)

cat("\nCOMPARACIÓN RAW vs CLEAN:\n")
print(stats_comparacion)

cat("\nCAMBIOS RELATIVOS (%):\n")
print(stats_comparacion %>% select(variable, starts_with("rel_")))
