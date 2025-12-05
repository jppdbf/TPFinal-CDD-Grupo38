library(readr)
library(dplyr)
library(ggplot2)

path_clean   <- "data/clean"
path_figures <- "outputs/figures"

if (!dir.exists(path_figures)) dir.create(path_figures, recursive = TRUE)

# 1. Leer base clean y preparar datos

panel_clean <- read_csv(
  file.path(path_clean, "base_clean_panel.csv"),
  show_col_types = FALSE
)

data_plot <- panel_clean %>%
  filter(
    !is.na(kcal_pc),
    !is.na(gdp_pc_ppp),
    !is.na(yield_cereals),
    kcal_pc > 0,
    gdp_pc_ppp > 0,
    yield_cereals > 0
  )


# 2. Gráfico 1: log-log ingreso per cápita vs kcal


p1 <- ggplot(data_plot, aes(x = gdp_pc_ppp, y = kcal_pc)) +
  geom_point(alpha = 0.6, colour = "#77B1D4") +        # puntos azules
  geom_smooth(method = "lm", se = TRUE,
              linewidth = 1.1, colour = "red") +    # línea roja
  scale_x_log10() +
  scale_y_log10() +
  theme_minimal(base_size = 14) +
  labs(
    title = "Ingreso per cápita y seguridad alimentaria",
    subtitle = "Relación log-log entre ingreso per cápita (PPP) y calorías disponibles",
    x = "log10(Ingreso per cápita, PPP)",
    y = "log10( Calorías diarias por persona )",
    caption = "Fuente de datos: WDI (Banco Mundial) y OWID. Elaboración propia."
  )

print(p1)

file_p1 <- file.path(path_figures, "grafico1_loglog_ingreso_kcal.png")
ggsave(file_p1, p1, width = 9, height = 6, dpi = 300)
cat("Gráfico 1 guardado en: ", file_p1, "\n")

# 3. Gráfico 2: log-log rendimiento agrícola vs kcal

p2 <- ggplot(data_plot, aes(x = yield_cereals, y = kcal_pc)) +
  geom_point(alpha = 0.6, colour = "#77B1D4") +        # puntos azules
  geom_smooth(method = "lm", se = TRUE,
              linewidth = 1.1, colour = "red") +    # línea roja
  scale_x_log10() +
  scale_y_log10() +
  theme_minimal(base_size = 14) +
  labs(
    title = "Rendimiento agrícola y seguridad alimentaria",
    subtitle = "Relación log-log entre rendimiento de cereales y calorías disponibles",
    x = "log10(Rendimiento de cereales, kg/ha)",
    y = "log10( Calorías diarias por persona )",
    caption = "Fuente de datos: WDI (Banco Mundial) y OWID. Elaboración propia."
  )

print(p2)

file_p2 <- file.path(path_figures, "grafico2_loglog_yield_kcal.png")
ggsave(file_p2, p2, width = 9, height = 6, dpi = 300)
cat("Gráfico 2 guardado en: ", file_p2, "\n")
