
source("scripts/Exploracion_Datos.R")

# -----------------------------------------------------------------------------
# 6. REGRESION LINEAL 
# -----------------------------------------------------------------------------

# =============================================================
# 1. Definir presupuesto máximo
# =============================================================

presupuesto_max <- 400

# =============================================================
# 2. Filtrar ofertas viables en ZonaNorte_limpio
#    Ajusta aquí las condiciones según la vivienda 1
# =============================================================

ofertas_base <- ZonaNorte_limpio %>%
  filter(
    Precio <= presupuesto_max,
    !is.na(Longitud),
    !is.na(Latitud)
  )

# Si quieres acercarte más a la vivienda 1, puedes agregar filtros como estos:
# ofertas_base <- ZonaNorte_limpio %>%
#   filter(
#     Precio <= presupuesto_max,
#     Estrato == 4,
#     Habitaciones >= 3,
#     Banios >= 2,
#     Parqueaderos >= 1,
#     Area_construida >= 100,
#     !is.na(Longitud),
#     !is.na(Latitud)
#  )

# =============================================================
# 3. Predecir precio con el modelo
# =============================================================

ofertas_base <- ofertas_base %>%
  mutate(
    pred_log = predict(modelo, newdata = ofertas_base),
    precio_estimado = 10^pred_log,
    diferencia_modelo = precio_estimado - Precio
  )

# =============================================================
# 4. Seleccionar 5 ofertas potenciales
#    Mayor diferencia positiva = posible oportunidad
# =============================================================

top_ofertas <- ofertas_base %>%
  arrange(desc(diferencia_modelo)) %>%
  slice(1:5)

# =============================================================
# 5. Convertir ofertas a objeto espacial
# =============================================================

top_ofertas_sf <- st_as_sf(
  top_ofertas,
  coords = c("Longitud", "Latitud"),
  crs = 4326,
  remove = FALSE
)

# =============================================================
# 6. Asegurar comunas en el mismo CRS
# =============================================================

comunas_4326 <- st_transform(comunas, 4326)

# =============================================================
# 7. Unir ofertas con comunas si quieres saber en cuál cae cada una
# =============================================================

top_ofertas_comuna <- st_join(top_ofertas_sf, comunas_4326)
