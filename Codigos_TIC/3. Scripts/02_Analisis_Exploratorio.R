# ************************************************************
# ANÁLISIS EXPLORATORIO
# ************************************************************


# ************************************************************
# 0. CONFIGURACIÓN DEL ENTORNO ----
# ************************************************************

source(here::here("2. Librerias - Funciones", "Librerias.R"))


df <- readRDS(here::here("1. Data", "Procesada", "df_final.RDS"))
df_clean <- readRDS(here::here("1. Data", "Procesada", "df_clean.RDS"))
oni <- readRDS(here::here("1. Data", "Procesada", "oni.RDS"))
nino <- readRDS(here::here("1. Data", "Procesada", "nino.RDS"))
dfClimateDay <- readRDS(here::here("1. Data", "Procesada", "dfClimateDay_pre_interp.RDS"))
df_clima <- readRDS(here::here("1. Data", "Procesada", "df_clima.RDS"))







# ************************************************************
# 1. ESTADÍSTICAS DESCRIPTIVAS ----
# ************************************************************

## Tabla de Estadisticas Descriptivas Pre Tratamiento --------

df_mensual <- nino %>%
  dplyr::select(date_obj, ninio) %>%
  dplyr::left_join(oni %>% dplyr::select(date_obj, oni), by = "date_obj")



cat("\n── ESTADÍSTICAS DESCRIPTIVAS CLIMÁTICAS (PRE-TRATAMIENTO) ──\n")

# Calculamos para las diarias
tabla_diaria <- dfClimateDay %>%
  dplyr::select(-date) %>%
  tidyr::pivot_longer(cols = everything(), names_to = "Variable", values_to = "Valor") %>%
  dplyr::group_by(Variable) %>%
  dplyr::summarise(
    Frecuencia = "Diaria",
    N_Validos = sum(!is.na(Valor))/nrow(dfClimateDay),
    N_Perdidos = sum(is.na(Valor))/nrow(dfClimateDay),
    Media = mean(Valor, na.rm = TRUE),
    Desv_Est = sd(Valor, na.rm = TRUE),
    Minimo = min(Valor, na.rm = TRUE),
    Q1 = quantile(Valor, 0.25, na.rm = TRUE),
    Mediana = median(Valor, na.rm = TRUE),
    Q3 = quantile(Valor, 0.75, na.rm = TRUE),
    Maximo = max(Valor, na.rm = TRUE)
  )

# Calculamos para las mensuales
tabla_mensual <- df_mensual %>%
  dplyr::select(-date_obj) %>%
  tidyr::pivot_longer(cols = everything(), names_to = "Variable", values_to = "Valor") %>%
  dplyr::group_by(Variable) %>%
  dplyr::summarise(
    Frecuencia = "Mensual",
    N_Validos = sum(!is.na(Valor))/nrow(df_mensual),
    N_Perdidos = sum(is.na(Valor))/nrow(df_mensual),
    Media = mean(Valor, na.rm = TRUE),
    Desv_Est = sd(Valor, na.rm = TRUE),
    Minimo = min(Valor, na.rm = TRUE),
    Q1 = quantile(Valor, 0.25, na.rm = TRUE),
    Mediana = median(Valor, na.rm = TRUE),
    Q3 = quantile(Valor, 0.75, na.rm = TRUE),
    Maximo = max(Valor, na.rm = TRUE)
  )

# Unimos ambas tablas y redondeamos a 2 decimales
tabla_descriptiva_total <- bind_rows(tabla_diaria, tabla_mensual) %>%
  dplyr::mutate(across(where(is.numeric), ~round(., 4)))

print(tabla_descriptiva_total)


## Histograma de las variables climatológicas ------


# --- A) GRÁFICO DE VARIABLES DIARIAS ---
# Transformamos a formato largo y aplicamos el recorte dinámico
df_plot_diario <- dfClimateDay %>%
  dplyr::select(-date) %>%
  tidyr::pivot_longer(cols = everything(), names_to = "Variable", values_to = "Valor") %>%
  dplyr::filter(!is.na(Valor)) %>%
  dplyr::group_by(Variable) %>%
  # Recorte automático (1% al 99%)
  dplyr::filter(
    Valor >= quantile(Valor, 0.01, na.rm = TRUE) &
      Valor <= quantile(Valor, 0.99, na.rm = TRUE)
  ) %>%
  dplyr::ungroup()

nombres_diarios <- c(
  "atmn" = "Temp. Mínima (°C)",
  "atmx" = "Temp. Máxima (°C)",
  "et"   = "Evapotranspiración (mm)",
  "ra"   = "Precipitación (mm)", 
  "rh"   = "Humedad Relativa (%)"
)

grafico_diario <- ggplot(df_plot_diario, aes(x = Valor)) +
  # EL TRUCO PARA FACETAS: ave(..., PANEL, ...) asegura que cada gráfico sume su propio 100%
  geom_histogram(aes(y = after_stat(count / ave(count, PANEL, FUN = sum))), 
                 fill = "#4575b4", color = "white", bins = 30, alpha = 0.8) +
  facet_wrap(~Variable, scales = "free", labeller = as_labeller(nombres_diarios)) +
  # Eje Y en formato de porcentaje limpio (como en tu código original)
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Distribución de Variables Climáticas (Diarias)",
    subtitle = "Registros históricos (2000 - 2019)",
    x = "Valor Registrado",
    y = "Proporción de Días (%)"
  ) +
  theme_minimal(base_size = 20) + # Ajustado para que el texto de las facetas no colapse
  theme(
    strip.background = element_rect(fill = "gray90", color = NA),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

print(grafico_diario)


# --- B) GRÁFICO DE VARIABLES MENSUALES (MACROCLIMA) ---
df_plot_mensual <- df_mensual %>%
  dplyr::select(-date_obj) %>%
  tidyr::pivot_longer(cols = everything(), names_to = "Variable", values_to = "Valor") %>%
  dplyr::filter(!is.na(Valor)) %>%
  dplyr::group_by(Variable) %>%
  dplyr::filter(
    Valor >= quantile(Valor, 0.01, na.rm = TRUE) &
      Valor <= quantile(Valor, 0.99, na.rm = TRUE)
  ) %>%
  dplyr::ungroup()

nombres_mensuales <- c(
  "ninio" = "Índice El Niño 3.4 (°C)",
  "oni"   = "Índice ONI (°C)"
)

grafico_mensual <- ggplot(df_plot_mensual, aes(x = Valor)) +
  # Misma lógica para forzar el 100% individual en cada panel macroclimático
  geom_histogram(aes(y = after_stat(count / ave(count, PANEL, FUN = sum))), 
                 fill = "#d73027", color = "white", bins = 20, alpha = 0.8) +
  facet_wrap(~Variable, scales = "free", ncol = 2, labeller = as_labeller(nombres_mensuales)) +
  # Eje Y en porcentaje
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Distribución de Índices Macroclimáticos (Mensuales)",
    subtitle = "Registros históricos (2000 - 2019)",
    x = "Anomalía / Temperatura",
    y = "Proporción de Meses (%)"
  ) +
  theme_minimal(base_size = 20) +
  theme(
    strip.background = element_rect(fill = "gray90", color = NA),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

print(grafico_mensual)









# ************************************************************
# 2. ANÁLISIS EXPLORATORIO ----
# ************************************************************


# Numero de especies disponibles originalmente
df_clean %>% distinct(name)

# Número de genes distintos:
df_clean %>% distinct(genus)

# Numero de especies disponibles tras la depuración:
df %>% distinct(name)




# 1. ANÁLISIS DE PERSISTENCIA TEMPORAL (25%, 50%, 100%)

# Dimensión temporal del estudio
TOTAL_SEMESTRES <- 31

# Cálculo de persistencia por taxón residente
persistencia_df <- df %>%
  group_by(name) %>%
  summarise(
    semestres_presente = sum(hormigas_incidence > 0, na.rm = TRUE),
    proporcion_presencia = semestres_presente / TOTAL_SEMESTRES,
    .groups = "drop"
  )

# Aislamiento de vectores según umbrales lógicos
spp_25_pct  <- persistencia_df %>% filter(proporcion_presencia >= 0.25) %>% pull(name)
spp_50_pct  <- persistencia_df %>% filter(proporcion_presencia >= 0.50) %>% pull(name)
spp_75_pct  <- persistencia_df %>% filter(proporcion_presencia >= 0.75) %>% pull(name)
spp_100_pct <- persistencia_df %>% filter(proporcion_presencia == 1.00) %>% pull(name)

cat("[!] Especies con >= 25% de presencia:", length(spp_25_pct), "\n")
cat("[!] Especies con >= 50% de presencia:", length(spp_50_pct), "\n")
cat("[!] Especies con >= 75% de presencia:", length(spp_75_pct), "\n")
cat("[!] Especies con 100% de presencia:", length(spp_100_pct), "\n\n")



# 2. IDENTIFICACIÓN DE ESPECIES ESTACIONALES EXCLUSIVAS

# Evaluación de ocurrencias segregadas por variable 'dry'
exclusividad_df <- df %>%
  group_by(name) %>%
  summarise(
    veces_en_seca = sum(hormigas_incidence > 0 & dry == 1, na.rm = TRUE),
    veces_en_humeda = sum(hormigas_incidence > 0 & dry == 0, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    solo_seca = (veces_en_seca > 0) & (veces_en_humeda == 0),
    solo_humeda = (veces_en_humeda > 0) & (veces_en_seca == 0)
  )

spp_exclusivas_seca <- exclusividad_df %>% filter(solo_seca) %>% pull(name)
spp_exclusivas_humeda <- exclusividad_df %>% filter(solo_humeda) %>% pull(name)

cat("[!] Especies exclusivas de Estación Seca:", length(spp_exclusivas_seca), "\n")
spp_exclusivas_seca

cat("[!] Especies exclusivas de Estación Húmeda:", length(spp_exclusivas_humeda), "\n\n")
spp_exclusivas_humeda





# ************************************************************
# SELECCIÓN DE ESPECIES DOMINANTES POR UMBRAL DE PRESENCIA
# ************************************************************

# 1. Parámetro de control ecológico
# Se fija en 0.45 para retener únicamente a las especies presentes en más del 45%
# de los semestres. Con esto entregamos consistencia a los modelos estadísticos contra el sesgo 
# provocado por la excesiva cantidad de ceros de las especies raras.
MIN_PROP_PRESENCIA <- 0.45 

# 2. Resumen y selección de especies usando df
especies_resumen <- df %>%
  group_by(name) %>%
  summarise(
    total_temporadas = n(), 
    veces_presente = sum(hormigas_incidence > 0, na.rm = TRUE),
    total_abundancia = sum(hormigas_abundance, na.rm = TRUE),
    
    # Proporción temporal de ocupación de la especie en la serie temporal
    prop_presencia = veces_presente / total_temporadas,
    .groups = 'drop'
  ) %>%
  # 3. MODIFICACIÓN: Filtrado dinámico por consistencia temporal en lugar de un Top N fijo
  filter(prop_presencia > MIN_PROP_PRESENCIA) %>%
  # Ordenamiento jerárquico: consistencia histórica primero, dominancia numérica después
  arrange(desc(prop_presencia), desc(total_abundancia))

# 4. Extraer el vector con las especies que superaron el umbral crítico
nombres_especies_clave <- especies_resumen$name

# Inspección de las estadísticas del grupo de especies seleccionadas
print(especies_resumen)

# 5. Filtrado de la matriz biológica original
# Se descartan las especies satélite, reteniendo la submatriz de la comunidad dominante
df_filtrado <- df %>%
  filter(name %in% nombres_especies_clave)



# Tabla con las proporciones:
# View(especies_resumen)



# 1. ESQUEMA DE ABUNDANCIA
verificacion_temporal <- df_filtrado %>%
  mutate(
    name = factor(name, levels = rev(nombres_especies_clave)),
    
    # Definir Estación basado en 'seasonYear' o 'dry' (si dry==1 es Dry)
    Season = if_else(str_detect(seasonYear, "Dry"), "Estación Seca", "Estación Húmeda"),
    # Crear etiqueta de Periodo usando 'sample_year'
    Periodo = paste(sample_year, Season, sep = "_")
  ) %>%
  # Agrupar por 'name' (especie) y 'sample_year'
  group_by(name, sample_year, Season) %>%
  # Sumar 'hormigas_abundance'
  summarise(Total = sum(hormigas_abundance), .groups = "drop")

# 2. Gráfico de Calor (Heatmap) - (Se mantiene idéntico)
grafico_abundancia <- ggplot(verificacion_temporal, aes(x = as.factor(sample_year), y = name, fill = Total)) +
  geom_tile(color = "white") +
  
  # Capa de texto
  geom_text(aes(label = ifelse(Total > 0, Total, "")), 
            color = "grey20", 
            size = 3, 
            fontface = "bold") +
  
  facet_grid(~Season) + 
  scale_fill_viridis_c(option = "magma", direction = -1) +
  labs(
    title = "Abundancia de Especies Clave",
    x = "Fecha",
    y = "Especie",
    fill = "Abundancia"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
  theme(
  # Ejes: Tamaño aumentado a 11pt y 12pt para lectura física clara
  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, color = "black", size = 11),
  axis.text.y = element_text( color = "black", size = 12),
  axis.title = element_text(face = "bold", color = "black", size = 15),
  
  # Formato de etiquetas superiores (Facetas de Estación)
  strip.background = element_rect(fill = "grey95", color = "grey80"),
  strip.text = element_text(face = "bold", size = 12, color = "black"),
  
  # Títulos y Leyenda
  plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
  legend.title = element_text(face = "bold", size = 13),
  legend.text = element_text(size = 11),
  panel.grid = element_blank()
)

ggsave("i1_abundancia.png", plot = grafico_abundancia, width = 14, height = 7, dpi = 300)




# 2. ESQUEMA DE INCIDENCIA
verificacion_incidencia <- df_filtrado %>% 
  mutate(
    # --- Mantenemos tu orden jerárquico de especies ---
    name = factor(name, levels = rev(nombres_especies_clave)),
    
    # Definir Estación
    Season = if_else(dry == 1, "Estación Seca", "Estación Húmeda")
  ) %>%
  group_by(name, sample_year, Season) %>%
  # Sumamos la incidencia (Rango confirmado 0-10)
  summarise(Incidencia_Total = sum(hormigas_incidence, na.rm = TRUE), .groups = "drop")

# 2. Gráfico de Calor con ETIQUETAS DE TEXTO
grafico_incidencia <- ggplot(verificacion_incidencia, aes(x = as.factor(sample_year), y = name, fill = Incidencia_Total)) +
  geom_tile(color = "white") + # Bordes blancos para separar
  
  # --- ETIQUETAS DE TEXTO ---
  geom_text(aes(label = ifelse(Incidencia_Total > 0, Incidencia_Total, ""), 
                color = Incidencia_Total > 5),      # UMBRAL RESTAURADO: Si es > 5 texto blanco
            size = 3, 
            fontface = "bold") +
  
  # Configuramos los colores del texto (FALSE = Gris oscuro, TRUE = Blanco)
  scale_color_manual(values = c("grey20", "white"), guide = "none") +
  
  facet_grid(~Season) + 
  
  # Escala de color para el fondo
  scale_fill_viridis_c(
    option = "mako",       
    direction = -1,        # Oscuro = Mayor incidencia
    limits = c(0, 10),     # LÍMITE RESTAURADO: Forzamos rango 0-10
    oob = scales::squish,  
    name = "Incidencia"
  ) +
  
  labs(
    title = "Incidencia de Especies Clave",
    x = "Fecha",
    y = "Especie"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    panel.grid.major = element_blank()
  ) +
  theme(
    # Homogeneización de tamaños de fuente para el bloque impreso
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, color = "black", size = 11),
    axis.text.y = element_text(color = "black", size = 12),
    axis.title = element_text(face = "bold", color = "black", size = 13),
    
    # Formato de etiquetas superiores (Facetas de Estación)
    strip.background = element_rect(fill = "grey95", color = "grey80"),
    strip.text = element_text(face = "bold", size = 12, color = "black"),
    
    # Títulos y Leyenda
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    legend.title = element_text(face = "bold", size = 11),
    legend.text = element_text(size = 11),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

ggsave("i2_incidencia.png", plot = grafico_incidencia, width = 12, height = 7, dpi = 300)





# 3. ALMACENAMIENTO ----------------------------------------------------------


# 
# saveRDS(nombres_especies_clave, 
#         here::here("1. Data", "Procesada", "nombre_especies_clave.RDS"))
# 



