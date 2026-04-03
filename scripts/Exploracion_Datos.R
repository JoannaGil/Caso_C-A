# =============================================================
# Exploracion_Datos.R
# Convertido desde Exploracion_Datos.Rmd
# Incluye correcciones de tipos identificadas en auditoría
# =============================================================


# -------------------------------------------------------------
# SETUP — Carga de datos
# -------------------------------------------------------------

# install.packages("devtools")  # solo la primera vez
# devtools::install_github("centromagis/paqueteMODELOS", force = TRUE)

library(paqueteMODELOS)
data("vivienda")


# =============================================================
# STAGE 1: Inspección inicial
# =============================================================

# Estructura del dataset: tipos y ejemplos de valores
str(vivienda)

# Clase de cada variable
sapply(vivienda, class)

# Resumen estadístico general
summary(vivienda)


# -------------------------------------------------------------
# Preparación — copia de trabajo
# -------------------------------------------------------------

# Se crea una copia para preservar el dataset original
vivienda_clean <- vivienda


# -------------------------------------------------------------
# Renombramiento de variables
# -------------------------------------------------------------

etiquetas <- c(
  zona         = "Zona",
  piso         = "Piso",
  estrato      = "Estrato",
  preciom      = "Precio",
  areaconst    = "Area_construida",   # corregido: guión bajo en lugar de espacio
  parqueaderos = "Parqueaderos",
  banios       = "Banios",            # corregido: sin tilde para evitar problemas de encoding
  habitaciones = "Habitaciones",
  tipo         = "Tipo",
  barrio       = "Barrio",
  longitud     = "Longitud",
  latitud      = "Latitud"
)

names(vivienda_clean) <- ifelse(
  names(vivienda_clean) %in% names(etiquetas),
  etiquetas[names(vivienda_clean)],
  names(vivienda_clean)
)


# =============================================================
# STAGE 2: Calidad de datos — valores faltantes
# =============================================================

# Vector de representaciones no estándar de valores faltantes
missing_values <- c("", " ", "NA", "NaN", "null", "NULL")

# Conteo de faltantes en el dataset original
faltantes <- sapply(vivienda, function(x) {
  x <- as.character(x)
  sum(is.na(x) | trimws(x) %in% missing_values)
})
faltantes

# Conteo de faltantes en vivienda_clean (tras renombramiento)
faltantes_totales <- sapply(vivienda_clean, function(x) {
  x <- as.character(x)
  sum(is.na(x) | trimws(x) %in% missing_values)
})
faltantes_totales

# Estandarización: convertir representaciones no estándar a NA
# Nota: este paso convierte TODAS las columnas a character temporalmente
vivienda_clean <- data.frame(
  lapply(vivienda_clean, function(x) {
    x <- as.character(x)
    x[trimws(x) %in% missing_values] <- NA
    x
  }),
  stringsAsFactors = FALSE
)


# =============================================================
# STAGE 3: Limpieza y corrección de tipos
# =============================================================

# --- 3.1 Parqueaderos: NA = sin parqueadero → reemplazar con 0 ---
vivienda_clean$Parqueaderos <- as.numeric(vivienda_clean$Parqueaderos)
vivienda_clean$Parqueaderos[is.na(vivienda_clean$Parqueaderos)] <- 0


# --- 3.2 Piso: imputación por mediana dentro de cada zona ---
vivienda_clean$Piso <- as.numeric(vivienda_clean$Piso)

vivienda_clean$Piso <- ave(
  vivienda_clean$Piso,
  vivienda_clean$Zona,
  FUN = function(x) {
    if (all(is.na(x))) {
      x
    } else {
      x[is.na(x)] <- median(x, na.rm = TRUE)
      x
    }
  }
)


# --- 3.3 Eliminación de filas con NA en variables críticas ---
cols_criticas <- c(
  "Zona", "Piso", "Estrato", "Precio", "Area_construida",
  "Parqueaderos", "Banios", "Habitaciones", "Tipo",
  "Barrio", "Longitud", "Latitud"
)

vivienda_clean <- vivienda_clean[
  complete.cases(vivienda_clean[, cols_criticas]),
]


# --- 3.4 Corrección de tipos de datos ---

# Variables categóricas → factor
vivienda_clean$Zona   <- as.factor(vivienda_clean$Zona)
vivienda_clean$Barrio <- as.factor(vivienda_clean$Barrio)
vivienda_clean$Tipo   <- as.factor(vivienda_clean$Tipo)   # corregido: faltaba en original

# Variables numéricas — reconversión tras lapply del Stage 2
vivienda_clean$Precio        <- as.numeric(vivienda_clean$Precio)
vivienda_clean$Piso          <- as.numeric(vivienda_clean$Piso)
vivienda_clean$Estrato       <- as.numeric(vivienda_clean$Estrato)       # corregido
vivienda_clean$Habitaciones  <- as.numeric(vivienda_clean$Habitaciones)  # corregido
vivienda_clean$Banios        <- as.numeric(vivienda_clean$Banios)        # corregido
vivienda_clean$Latitud       <- as.numeric(vivienda_clean$Latitud)       # corregido
vivienda_clean$Longitud      <- as.numeric(vivienda_clean$Longitud)      # corregido
vivienda_clean$Area_construida <- as.numeric(vivienda_clean$Area_construida) # corregido

# Eliminar variable id si existe
if ("id" %in% names(vivienda_clean)) vivienda_clean$id <- NULL

# Verificación de tipos tras correcciones
sapply(vivienda_clean, class)


# =============================================================
# STAGE 4: Verificación final
# =============================================================

# Resumen general del dataset limpio
summary(vivienda_clean)

# Verificación de tipos
str(vivienda_clean)

# Confirmación de ausencia de NAs residuales
colSums(is.na(vivienda_clean))


# =============================================================
# FILTRADO — Construcción de ZonaNorte
# =============================================================

# Subconjunto: casas en Zona Norte según variable categórica
#ZonaNorte <- subset(vivienda_clean, Zona == "Zona Norte" & Tipo == "Casa")

