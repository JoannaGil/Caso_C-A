source("scripts/Exploracion_Datos.R")

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




