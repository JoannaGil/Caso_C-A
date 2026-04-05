source("scripts/Exploracion_Datos.R")

# -----------------------------------------------------------------------------
# 2. Análisis exploratorio y correlacional de las variables - ZonaNorte
# -----------------------------------------------------------------------------


#Filtrar viviendas tipo casa con Zona Norte (base original)
ZonaNorte <- vivienda_clean %>%
  filter(Tipo == "Casa", Zona == "Zona Norte")

##### NOTA ####

#Al realizar la distrobucion de la variable "habitaciones" se encontraron valores cero por que se decidio pasar estos a NA 
# para posteriormente eliminarlos 

# 1. Limpiar espacios
ZonaNorte$Habitaciones <- trimws(ZonaNorte$Habitaciones)

# 2. Convertir a numérico
ZonaNorte$Habitaciones  <- as.numeric(ZonaNorte$Habitaciones)

# 3. Ver cuántos ceros hay
sum(ZonaNorte$Habitaciones  == 0, na.rm = TRUE)

# 4. Reemplazar ceros por NA
ZonaNorte$Habitaciones [ZonaNorte$Habitaciones  == 0] <- NA

# 5. Ver cuántos NA quedaron
sum(is.na(ZonaNorte$Habitaciones ))

# 6. Eliminar filas con NA en Banios
ZonaNorte <- ZonaNorte[!is.na(ZonaNorte$Habitaciones ), ]

# 7. Verificación final
nrow(ZonaNorte)
sum(ZonaNorte$Habitaciones  == 0, na.rm = TRUE)
sum(is.na(ZonaNorte$Habitaciones ))
unique(ZonaNorte$Habitaciones )


#Limpieza de la variable Banios

# 1. Limpiar espacios
ZonaNorte$Banios <- trimws(ZonaNorte$Banios)

# 2. Convertir a numérico
ZonaNorte$Banios <- as.numeric(ZonaNorte$Banios)

# 3. Ver cuántos ceros hay
sum(ZonaNorte$Banios == 0, na.rm = TRUE)

# 4. Reemplazar ceros por NA
ZonaNorte$Banios[ZonaNorte$Banios == 0] <- NA

# 5. Ver cuántos NA quedaron
sum(is.na(ZonaNorte$Banios))

# 6. Eliminar filas con NA en Banios
ZonaNorte <- ZonaNorte[!is.na(ZonaNorte$Banios), ]

# 7. Verificación final
nrow(ZonaNorte)
sum(ZonaNorte$Banios == 0, na.rm = TRUE)
sum(is.na(ZonaNorte$Banios))
unique(ZonaNorte$Banios)

# -----------------------------------------------------------------------------
# 0. LIBRERÍAS REQUERIDAS
# -----------------------------------------------------------------------------

library(paqueteMODELOS)
library(ggplot2)
library(highcharter)
library(purrr)
library(scales)
library(corrplot)
library(dplyr)
library(knitr)
library(kableExtra)


# -----------------------------------------------------------------------------
# 1. INSPECCIÓN GENERAL DEL DATASET
# -----------------------------------------------------------------------------

## Dimensiones del dataset
cat("Dimensiones del dataset:", dim(ZonaNorte), "\n")
# dim() devuelve [filas, columnas] — útil para verificar el tamaño esperado.

## Estructura y tipos de variables
str(ZonaNorte)
# str() muestra el tipo de cada columna (num, int, chr, factor, etc.)

## Estadísticas descriptivas
summary(ZonaNorte)
# summary() entrega: mínimo, Q1, mediana, media, Q3, máximo y conteo de NA.

## Vista de las primeras y últimas filas
head(ZonaNorte, 10)
tail(ZonaNorte, 10)
# Permite detectar errores de importación al inicio/fin del archivo.

## Valores Faltantes
colSums(is.na(ZonaNorte))


# =============================================================================
# 2. ANÁLISIS UNIVARIADO — DISTRIBUCIONES
# =============================================================================

# La escala log10 se aplica porque Precio suele tener distribución
# log-normal en mercados inmobiliarios: comprime la cola derecha y permite
# visualizar mejor la concentración central de los datos.


# =============================================================================
# 2.1. GRAFICA DISTRIBUCIONES PRECIO 
# =============================================================================

#Se trabaja con highcharte para hacerlo mas dinamico al momento de querer indagar o
#verificar valores

# 1. Filtrar precios válidos
precios <- ZonaNorte$Precio
precios <- precios[!is.na(precios) & precios > 0]

# 2. Histograma en log10
h <- hist(log10(precios), breaks = 30, plot = FALSE)

# 3. Límites de cada bin
bin_inicio_log <- h$breaks[-length(h$breaks)]
bin_fin_log    <- h$breaks[-1]

# 4. Convertir a pesos reales
bin_inicio_cop <- 10^bin_inicio_log
bin_fin_cop    <- 10^bin_fin_log
bin_mid_cop    <- 10^h$mids

# 5. Formatear a COP
fmt <- label_number(big.mark = ".", decimal.mark = ",")

df_hist <- data.frame(
  x = h$mids,
  y = h$counts,
  width = diff(h$breaks),
  inicio = fmt(bin_inicio_cop),
  fin    = fmt(bin_fin_cop),
  medio  = fmt(bin_mid_cop)
)

# 6. Ticks eje X
ticks_vals <- log10(c(1e8, 2e8, 5e8, 1e9, 2e9))
ticks_text <- c("$100 M", "$200 M", "$500 M", "$1.000 M", "$2.000 M")


# =============================================================================
# 2.2. GRAFICA DISTRIBUCIONES AREA CONSTRUIDA
# =============================================================================

# 1. Filtrar valores válidos de área construida
area <- ZonaNorte$Area_construida
area <- area[!is.na(area) & area > 0]

# 2. Crear histograma
h_area <- hist(area, breaks = 30, plot = FALSE)

# 3. Límites de bins
bin_inicio_area <- h_area$breaks[-length(h_area$breaks)]
bin_fin_area    <- h_area$breaks[-1]

# 4. Calcular porcentaje
porcentaje_area <- h_area$counts / sum(h_area$counts)

# 5. Formatear etiquetas
fmt_num <- label_number(big.mark = ".", decimal.mark = ",", accuracy = 1)
fmt_pct <- label_percent(accuracy = 0.1, decimal.mark = ",")

# 6. Construir data frame para el gráfico
df_area_hist <- data.frame(
  x = h_area$mids,
  y = porcentaje_area,
  width = diff(h_area$breaks),
  inicio = fmt_num(bin_inicio_area),
  fin = fmt_num(bin_fin_area),
  medio = fmt_num(h_area$mids),
  pct = fmt_pct(porcentaje_area)
)

# =============================================================================
# 2.3. GRAFICA DISTRIBUCIONES BAÑOS
# =============================================================================


df_banios <- ZonaNorte |>
  dplyr::filter(!is.na(Banios)) |>
  dplyr::count(Banios) |>
  dplyr::arrange(Banios)


# =============================================================================
# 2.4. GRAFICA DISTRIBUCIONES HABITACIONES
# =============================================================================

df_habitaciones <- ZonaNorte |>
  dplyr::filter(!is.na(Habitaciones)) |>
  dplyr::count(Habitaciones) |>
  dplyr::arrange(Habitaciones)

# =============================================================================
# 2.4. GRAFICA DISTRIBUCIONES ESTRATO
# =============================================================================

df_estrato <- ZonaNorte |>
  dplyr::filter(!is.na(Estrato)) |>
  dplyr::mutate(Estrato = as.numeric(Estrato)) |>
  dplyr::count(Estrato) |>
  dplyr::arrange(Estrato)

# =============================================================================
# 3 ANÁLISIS BIVARIADO — DISTRIBUCIONES
# =============================================================================

# En este paso buscamos verificar algunos elementos estructurales que no tenian mucha coherencia. 
# Por ejemplo, Propiedades con pocos metros cuadrados y muchos baños. validaremos los datos 

# =============================================================================
# 3.1. DISTRIBUCIONES PRECIO / AREA CONSTRUIDA
# =============================================================================

df_scatter <- ZonaNorte |>
  filter(
    !is.na(Area_construida),
    !is.na(Precio),
    Area_construida > 0,
    Precio > 0
  )

# =============================================================================
# 3.2. DISTRIBUCIONES PRECIO / HABITACIONES
# =============================================================================

df_box <- ZonaNorte |>
  filter(
    !is.na(Habitaciones),
    !is.na(Precio),
    Precio > 0
  ) |>
  mutate(Precio_log = log10(Precio))

# =============================================================================
# 3.3. DISTRIBUCIONES PRECIO / ESTRATO
# =============================================================================

# 1. Preparar datos
df_estrato <- ZonaNorte |>
  filter(
    !is.na(Estrato),
    !is.na(Precio),
    Precio > 0
  ) |>
  mutate(
    Estrato = as.character(Estrato),
    Precio_log = log10(Precio)
  )

# 2. Ordenar estratos
estratos <- sort(unique(df_estrato$Estrato))

# 3. Función para formato COP
fmt_cop <- function(x) {
  scales::label_number(
    big.mark = ".",
    decimal.mark = ",",
    accuracy = 1
  )(x)
}
# 4. Construir datos del boxplot
box_data <- lapply(estratos, function(e) {
  x <- df_estrato$Precio_log[df_estrato$Estrato == e]
  q <- as.numeric(quantile(x, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE))
  
  low_cop    <- fmt_cop(10^q[1])
  q1_cop     <- fmt_cop(10^q[2])
  median_cop <- fmt_cop(10^q[3])
  q3_cop     <- fmt_cop(10^q[4])
  high_cop   <- fmt_cop(10^q[5])
  
  list(
    low = as.numeric(q[1]),
    q1 = as.numeric(q[2]),
    median = as.numeric(q[3]),
    q3 = as.numeric(q[4]),
    high = as.numeric(q[5]),
    low_cop = low_cop,
    q1_cop = q1_cop,
    median_cop = median_cop,
    q3_cop = q3_cop,
    high_cop = high_cop
  )
})


# =============================================================================
# 4. LIMPIEZA DATOS
# =============================================================================

#VERIFICAMOS Y ELIMINAMOS VALORES QUE NO TENIAN COHERENCIA ESTRUCTURAL 

# 1. Copia de trabajo
ZonaNorte_base <- ZonaNorte

# 2. Crear columnas de validación estructural mínima
ZonaNorte_base <- ZonaNorte_base |>
  mutate(
    area_min_banios = Banios * 8,
    area_min_habitaciones = Habitaciones * 8,
    area_min_total_requerida = area_min_banios + area_min_habitaciones,
    diferencia_area = Area_construida - area_min_total_requerida,
    estado_coherencia = case_when(
      Area_construida < area_min_total_requerida ~ "No cumple mínimo estructural",
      TRUE ~ "Cumple mínimo estructural"
    ),
    deficit_area = case_when(
      Area_construida < area_min_total_requerida ~ area_min_total_requerida - Area_construida,
      TRUE ~ 0
    )
  )

# 3. Filtrar casos sospechosos
casos_sospechosos <- ZonaNorte_base |>
  filter(estado_coherencia == "No cumple mínimo estructural")

# 4. Ver cantidad de casos sospechosos
nrow(casos_sospechosos)

# 5. Revisar casos sospechosos
casos_sospechosos |>
  select(
    Precio,
    Area_construida,
    Habitaciones,
    Banios,
    area_min_banios,
    area_min_habitaciones,
    area_min_total_requerida,
    diferencia_area,
    deficit_area,
    estado_coherencia
  ) |>
  head(20)

# 6. Ver la base en RStudio
#print(casos_sospechosos, n = 20)

# 7. Crear base limpia
ZonaNorte_limpio <- ZonaNorte_base |>
  filter(estado_coherencia == "Cumple mínimo estructural")

# 8. Comparar tamaños
nrow(ZonaNorte_base)
nrow(ZonaNorte_limpio)
nrow(casos_sospechosos)
#summary(ZonaNorte_base$deficit_area)


# =============================================================================
# 5. TEST SHAPIRO
# =============================================================================

resultado_shapiro <- sapply(
  ZonaNorte_limpio |>
    dplyr::mutate(Precio_log = log10(Precio)) |>
    dplyr::select(Precio_log, Area_construida, Habitaciones, Banios, Estrato, Parqueaderos),
  function(x) shapiro.test(x)$p.value
)

tabla_shapiro <- data.frame(
  Variable = c(
    "Precio (log)",
    "Área construida",
    "Habitaciones",
    "Baños",
    "Estrato",
    "Parqueaderos"
  ),
  `P-value` = formatC(resultado_shapiro, format = "e", digits = 2),
  Normalidad = ifelse(resultado_shapiro > 0.05, "Normal", "No normal")
)

# =============================================================================
# 5. MATRIZ CORRELACION
# =============================================================================

df_corr <- ZonaNorte_limpio |>
  dplyr::select(
    Precio,
    Area_construida,
    Habitaciones,
    Banios,
    Estrato,
    Parqueaderos
  ) |>
  dplyr::mutate(
    Precio_log = log10(Precio),
    Estrato = as.numeric(Estrato)
  ) |>
  dplyr::select(
    Precio_log,
    Area_construida,
    Habitaciones,
    Banios,
    Estrato,
    Parqueaderos
  )

cor_matrix <- cor(df_corr, method = "spearman", use = "complete.obs")

cor_matrix

# =============================================================================
# 5. MATRIZ CORRELACION GRAFICO
# =============================================================================


# Matriz de correlación Spearman
cor_matriz <- cor(df_corr, method = "spearman", use = "complete.obs")
print(round(cor_matriz, 3))

