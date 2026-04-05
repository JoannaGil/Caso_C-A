
source("scripts/Exploracion_Datos.R")

# -----------------------------------------------------------------------------
# 3. REGRESION LINEAL 
# -----------------------------------------------------------------------------


# -----------------------------------------------------------------------------
# 3.1. LIBRERÍAS REQUERIDAS
# -----------------------------------------------------------------------------
library(broom)
library(dplyr)
library(knitr)
library(kableExtra)


# -----------------------------------------------------------------------------
# 3.2. LIBRERÍAS REQUERIDAS
# -----------------------------------------------------------------------------


modelo <- lm(
  log10(Precio) ~ Area_construida + Estrato + Habitaciones + Parqueaderos + Banios,
  data = ZonaNorte_limpio
)

tabla_modelo <- broom::tidy(modelo) |>
  mutate(
    Nombre = c("Intercepto", "Área construida", "Estrato", "Habitaciones", "Parqueaderos", "Baños"),
    align = "l",
    Coeficiente = round(estimate, 5),
    `Error estándar` = round(std.error, 5),
    `t value` = round(statistic, 2),
    p_num = p.value,  # para ordenar
    `P-value` = formatC(p.value, format = "e", digits = 2),
    Significancia = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ ""
    ),
    Impacto = case_when(
      abs(estimate) > 0.10 ~ "Alto",
      abs(estimate) > 0.03 ~ "Medio",
      TRUE ~ "Bajo"
    )
  )

# separar intercepto
tabla_intercepto <- tabla_modelo |>
  filter(Nombre == "Intercepto")

# ordenar variables por significancia (p-value)
tabla_variables <- tabla_modelo |>
  filter(Nombre != "Intercepto") |>
  arrange(p_num)

# unir nuevamente
tabla_modelo_final <- bind_rows(tabla_intercepto, tabla_variables) |>
  
  # aplicar colores
  mutate(
    Significancia = case_when(
      Significancia == "***" ~ cell_spec("***", format = "html", color = "white", background = "#1a9641"),
      Significancia == "**"  ~ cell_spec("**",  format = "html", color = "white", background = "#66bd63"),
      Significancia == "*"   ~ cell_spec("*",   format = "html", color = "black", background = "#fff176"),
      TRUE ~ ""
    ),
    Impacto = case_when(
      Impacto == "Alto"  ~ cell_spec("Alto",  format = "html", color = "white", background = "#2166ac"),
      Impacto == "Medio" ~ cell_spec("Medio", format = "html", color = "white", background = "#67a9cf"),
      Impacto == "Bajo"  ~ cell_spec("Bajo",  format = "html", color = "black", background = "#d1e5f0")
    )
  ) |>
  
  select(
    Nombre,
    Coeficiente,
    `Error estándar`,
    `t value`,
    `P-value`,
    Significancia,
    Impacto
  )
