
library(readr)
library(dplyr)
library(lmtest)
library(sandwich)
library(car)
library(modelsummary)

path_clean <- "data/clean"

panel_clean <- read_csv(
  file.path(path_clean, "base_clean_panel.csv"),
  show_col_types = FALSE
)

cat("Columnas en la base:\n")
print(names(panel_clean))

# Filtrar para evitar logs de ceros
panel_model <- panel_clean %>%
  filter(kcal_pc > 0,
         gdp_pc_ppp > 0,
         yield_cereals > 0)

cat("\nObservaciones después de filtrar ceros:\n")
print(nrow(panel_model))

## 2. Identificar dummies automáticamente

cat("\n=== 2. Identificando variables dummy ===\n")

is_dummy <- function(x) {
  ux <- unique(na.omit(x))
  length(ux) > 1 && all(ux %in% c(0, 1))
}

dummy_vars <- panel_model %>%
  select(where(is_dummy)) %>%
  select(-any_of(c("kcal_pc", "gdp_pc_ppp", "yield_cereals"))) %>%
  names()

cat("Dummies detectadas:\n")
print(dummy_vars)

## 3. Especificación de modelos

cat("\n=== 3. Especificando modelos (log-log) ===\n")

form_ingreso <- as.formula(
  paste(
    "log(kcal_pc) ~ log(gdp_pc_ppp)",
    if (length(dummy_vars) > 0) paste("+", paste(dummy_vars, collapse = " + ")) else ""
  )
)

form_rend <- as.formula(
  paste(
    "log(kcal_pc) ~ log(yield_cereals)",
    if (length(dummy_vars) > 0) paste("+", paste(dummy_vars, collapse = " + ")) else ""
  )
)

form_ambos <- as.formula(
  paste(
    "log(kcal_pc) ~ log(gdp_pc_ppp) + log(yield_cereals)",
    if (length(dummy_vars) > 0) paste("+", paste(dummy_vars, collapse = " + ")) else ""
  )
)

cat("Fórmula modelo ingreso:\n"); print(form_ingreso)
cat("Fórmula modelo rendimiento:\n"); print(form_rend)
cat("Fórmula modelo ambos:\n"); print(form_ambos)

## 4. Estimar modelos

cat("\n=== 4. Estimando modelos ===\n")

mod_ingreso <- lm(form_ingreso, data = panel_model)
mod_rend    <- lm(form_rend,    data = panel_model)
mod_ambos   <- lm(form_ambos,   data = panel_model)

cat("\nResumen modelo ingreso:\n")
print(summary(mod_ingreso))

cat("\nResumen modelo rendimiento:\n")
print(summary(mod_rend))

cat("\nResumen modelo ambos:\n")
print(summary(mod_ambos))

## 5. Tests de hipótesis con errores robustos

cat("\n=== 5. Tests t con errores robustos (HC1) ===\n")

cat("\nModelo ingreso (robusto):\n")
print(coeftest(mod_ingreso, vcov = vcovHC(mod_ingreso, type = "HC1")))

cat("\nModelo rendimiento (robusto):\n")
print(coeftest(mod_rend, vcov = vcovHC(mod_rend, type = "HC1")))

cat("\nModelo ambos (robusto):\n")
print(coeftest(mod_ambos, vcov = vcovHC(mod_ambos, type = "HC1")))

## 6. Diagnóstico de modelos

cat("\n=== 6. Diagnósticos ===\n")

cat("\nVIF (multicolinealidad) modelo ambos:\n")
print(vif(mod_ambos))

cat("\nTest Breusch-Pagan (heteroscedasticidad):\n")
print(bptest(mod_ambos))

## 7. Comparación del poder predictivo

cat("\n=== 7. Comparación de modelos: ingreso vs rendimiento ===\n")

comparacion_modelos <- data.frame(
  modelo   = c("Ingreso", "Rendimiento", "Ambos"),
  R2       = c(summary(mod_ingreso)$r.squared,
               summary(mod_rend)$r.squared,
               summary(mod_ambos)$r.squared),
  R2_adj   = c(summary(mod_ingreso)$adj.r.squared,
               summary(mod_rend)$adj.r.squared,
               summary(mod_ambos)$adj.r.squared),
  AIC      = c(AIC(mod_ingreso),
               AIC(mod_rend),
               AIC(mod_ambos)),
  BIC      = c(BIC(mod_ingreso),
               BIC(mod_rend),
               BIC(mod_ambos))
)

print(comparacion_modelos)

cat("\nInterpretación:\n")

if (comparacion_modelos$R2_adj[1] > comparacion_modelos$R2_adj[2]) {
  cat("→ El ingreso per cápita predice mejor la seguridad alimentaria que el rendimiento agrícola.\n")
} else {
  cat("→ El rendimiento agrícola tiene mayor poder predictivo que el ingreso.\n")
}

path_tables <- "outputs/tables"
if (!dir.exists(path_tables)) dir.create(path_tables, recursive = TRUE)


modelos <- list(
  "Ingreso per cápita"      = mod_ingreso,
  "Rendimiento agrícola"    = mod_rend,
  "Ambos predictores"       = mod_ambos
)

modelsummary(
  modelos,
  vcov = "HC1",
  stars = TRUE,
  statistic = "({std.error})",
  gof_map = c("nobs", "r.squared", "adj.r.squared", "AIC", "BIC"),
  output = file.path(path_tables, "tabla_regresiones_final.html"),
  title = "Tabla comparativa de modelos de regresión (errores robustos)"
)
