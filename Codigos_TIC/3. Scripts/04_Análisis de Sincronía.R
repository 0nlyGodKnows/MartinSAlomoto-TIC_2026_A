# ************************************************************
# ANALISIS DE SINCRONÍA
# ************************************************************


# ************************************************************
# 0. CONFIGURACIÓN DEL ENTORNO ----
# ************************************************************

source(here::here("2. Librerias - Funciones", "Librerias.R"))



df_clean <- readRDS(here::here("1. Data", "Procesada", "df_clean.RDS"))
oni <- readRDS(here::here("1. Data", "Procesada", "oni.RDS"))
nino <- readRDS(here::here("1. Data", "Procesada", "nino.RDS"))
dfClimateDay <- readRDS(here::here("1. Data", "Procesada", "dfClimateDay_pre_interp.RDS"))

nombres_especies_clave <- readRDS(here::here("1. Data", "Procesada", "nombre_especies_clave.RDS"))
especies_ganadoras_estacionariedad <- readRDS(here::here("1. Data", "Procesada", 
                                                         "especies_ganadoras_estacionariedad.RDS"))


df <- readRDS(here::here("1. Data", "Procesada", "df_final_filtrado.RDS"))
df_clima <- readRDS(here::here("1. Data", "Procesada", "df_clima_final_filtrado.RDS"))



# ************************************************************
# 1. MATRIZ DE CORRELACIONES Y SIGNIFICANCIA ----
# ************************************************************



# ***** VARIABLE IMPORTANTE ******
var_interes <- "ln_incidence"
# ********************************


# A. Matriz Biótica Estacionaria (Y) 
# Filas = Especies (n), Columnas = Semestres (T). Dimensiones: [n x T]
Y_z_matrix <- df %>%
  dplyr::select(name, seasonYear, var_interes) %>%
  tidyr::pivot_wider(names_from = seasonYear, values_from = var_interes) %>%
  tibble::column_to_rownames("name") %>%
  as.matrix()


cat(sprintf("  ✓ Matriz Y generada: [%d especies × %d semestres]\n", 
            nrow(Y_z_matrix), ncol(Y_z_matrix)))


# C. Matriz Climática (C)
# Excluimos variables de control y la llave temporal. Dimensiones: [T x covariables]
C_matrix <- df_clima %>%
  tibble::column_to_rownames("seasonYear") %>%
  as.matrix()





Y_t <- t(Y_z_matrix) 
cor_matrix <- cor(Y_t, use = "complete.obs", method = "pearson")
kable(round(cor_matrix, 3), 
      align = "c", 
      caption = "Matriz de Correlación Sincrónica entre Especies")

# Cálculo de p-valores para la significancia de la co-ocurrencia
n_sp <- nrow(Y_z_matrix)
especies_nombres <- rownames(Y_z_matrix)
cor_pvalues <- matrix(NA, n_sp, n_sp, dimnames = list(especies_nombres, especies_nombres))

for (i in 1:n_sp) {
  for (j in 1:n_sp) {
    if (i != j) {
      # tryCatch evita que la función explote si una especie tiene varianza 0
      ct <- tryCatch(cor.test(Y_t[, i], Y_t[, j]), error = function(e) list(p.value = NA))
      cor_pvalues[i, j] <- ct$p.value
    }
  }
}




df_cor <- as.data.frame(cor_matrix)
df_cor$Especie1 <- rownames(df_cor)
df_cor <- pivot_longer(df_cor, cols = -Especie1, names_to = "Especie2", values_to = "r")

df_p <- as.data.frame(cor_pvalues)
df_p$Especie1 <- rownames(df_p)
df_p <- pivot_longer(df_p, cols = -Especie1, names_to = "Especie2", values_to = "p_val")

df_heatmap <- left_join(df_cor, df_p, by = c("Especie1", "Especie2"))


# ******************************************
# 2. OPCIÓN A: MAPA DE CALOR CLÁSICO
# ******************************************
grafico_clasico_top <- ggplot(df_heatmap, aes(x = Especie1, y = Especie2, fill = r)) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_gradient2(
    low = "#313695", mid = "#ffffbf", high = "#a50026", 
    midpoint = 0, limits = c(-1, 1), name = "Pearson (r)"
  ) +
  scale_x_discrete(position = "top") + 
  labs(
    title = "Matriz de Correlación Sincrónica",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 0, hjust = 0, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(face = "bold", color = "black", size = 20),
    panel.grid = element_blank(),
    legend.title = element_text(face = "bold", size = 11),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    aspect.ratio = 1
  )

print(grafico_clasico_top)


# ******************************************
# 3. OPCIÓN B: MAPA FILTRADO POR SIGNIFICANCIA
# ******************************************
df_heatmap_filtrado <- df_heatmap %>%
  mutate(r_sig = ifelse(p_val < 0.05 | Especie1 == Especie2, r, NA))

grafico_significativo_top <- ggplot(df_heatmap_filtrado, aes(x = Especie1, y = Especie2, fill = r_sig)) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_gradient2(
    low = "#313695", mid = "#f7f7f7", high = "#a50026", 
    midpoint = 0, limits = c(-1, 1), name = "Pearson (r)\n(Solo p < 0.05)",
    na.value = "gray92"
  ) +
  scale_x_discrete(position = "top") + 
  labs(
    title = "Matriz de Correlación Sincrónica (Filtro de Significancia)",
    subtitle = "Las celdas en gris indican interacciones neutrales (p \u2265 0.05)",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 0, hjust = 0, size = 9),
    axis.text.y = element_text(size = 9),
    panel.grid = element_blank(),
    aspect.ratio = 1,
    plot.subtitle = element_text(color = "gray30", size = 9)
  )

print(grafico_significativo_top)





# ******************************************************************************
# 1. CÁLCULO DE MATRICES (Se mantiene igual, calcula matriz completa)
# ******************************************************************************
n_sp <- nrow(Y_z_matrix)
especies_nombres <- rownames(Y_z_matrix)

cor_pvalues <- matrix(NA, n_sp, n_sp, dimnames = list(especies_nombres, especies_nombres))
cor_coefs <- matrix(NA, n_sp, n_sp, dimnames = list(especies_nombres, especies_nombres))

for (i in 1:n_sp) {
  for (j in 1:n_sp) {
    if (i != j) {
      ct <- tryCatch(cor.test(Y_t[, i], Y_t[, j]), error = function(e) list(p.value = NA, estimate = NA))
      cor_pvalues[i, j] <- ct$p.value
      cor_coefs[i, j] <- ct$estimate
    }
  }
}


# Extraemos p-valores forzando pares únicos (A < B alfabéticamente)
df_pvalues <- as.data.frame(as.table(cor_pvalues)) %>%
  dplyr::rename(Especie_A = Var1, Especie_B = Var2, p_value = Freq) %>%
  dplyr::filter(as.character(Especie_A) < as.character(Especie_B))

# Extraemos coeficientes con el mismo filtro
df_coefs <- as.data.frame(as.table(cor_coefs)) %>%
  dplyr::rename(Especie_A = Var1, Especie_B = Var2, correlacion = Freq) %>%
  dplyr::filter(as.character(Especie_A) < as.character(Especie_B))

# Unimos y filtramos significancia
pares_significativos <- df_pvalues %>%
  dplyr::inner_join(df_coefs, by = c("Especie_A", "Especie_B")) %>%
  dplyr::filter(!is.na(p_value) & p_value < 0.05) %>%
  dplyr::mutate(
    Especie_A = as.character(Especie_A),
    Especie_B = as.character(Especie_B)
  )

# ******************************************************************************
# 3. ANÁLISIS DE CENTRALIDAD (Red no dirigida)
# ******************************************************************************
# Ahora que 'pares_significativos' tiene pares únicos reales, 
# se desdoblan para contar el grado de centralidad de cada nodo
enlaces_A <- pares_significativos %>% 
  dplyr::select(Especie_Principal = Especie_A, Socio = Especie_B, correlacion)

enlaces_B <- pares_significativos %>% 
  dplyr::select(Especie_Principal = Especie_B, Socio = Especie_A, correlacion)

red_completa <- dplyr::bind_rows(enlaces_A, enlaces_B)

# Cálculo de métricas robustas
ranking_especies <- red_completa %>%
  dplyr::group_by(Especie_Principal) %>%
  dplyr::summarise(
    Grado_Total = n(),
    Conexiones_Positivas = sum(correlacion > 0),
    Conexiones_Negativas = sum(correlacion < 0),
    Magnitud_Media = mean(abs(correlacion)),
    Socio_Mas_Fuerte = Socio[which.max(abs(correlacion))],
    Correlacion_Socio = correlacion[which.max(abs(correlacion))]
  ) %>%
  dplyr::arrange(desc(Grado_Total), desc(Magnitud_Media))

cat("\n── RANKING DE ESPECIES CLAVE (CENTRALIDAD EN LA RED) ──\n")
print(n=100,ranking_especies)
# Visualizar Tabla:
# View(ranking_especies)






# 2. INDVAL Y SIMPER ------------------------------------------------------


## INDVAL ------

df11 <- readRDS(here::here("1. Data", "Procesada", "df_zero_filled.RDS"))
df11 <- df11 %>% filter(name %in% especies_ganadoras_estacionariedad)


# 1. Agrupar y Colapsar (Crear la Unidad Trampa-Temporada)
datos_colapsados <- df11 %>%
  # Extraer solo si es Dry o Wet de la columna seasonYear o comments
  mutate(Season = if_else(str_detect(seasonYear, "Dry"), "Dry", "Wet")) %>%
  group_by(sample_year , Season, trap, name) %>%
  summarise(abundancia_total = sum(hormigas_abundance ), .groups = "drop")

# 2. Crear la Matriz de Comunidad (Sitios x Especies)
matriz_comunidad <- datos_colapsados %>%
  dplyr::select(sample_year, Season, trap, name, abundancia_total) %>%
  pivot_wider(names_from = name, 
              values_from = abundancia_total, 
              values_fill = 0) 

# 3. Convertir a Incidencia (Presencia/Ausencia)
# Separamos los metadatos de la matriz numérica
metadatos <- matriz_comunidad %>% dplyr::select(sample_year, Season, trap)
matriz_X <- matriz_comunidad %>% dplyr::select(-sample_year, -Season, -trap)


matriz_abundancia <- matriz_X
# Binarización estricta (1 = Presente, 0 = Ausente)
matriz_X[matriz_X > 0] <- 1



vector_grupos <- metadatos$Season
# Resultado esperado: c("Dry", "Dry", "Wet", "Wet", ...) alineado con las filas de matriz_X



# Definir el esquema de permutación restringida
ctrl <- how(blocks = metadatos$sample_year , nperm = 999)

# Ejecutar IndVal
resultado_indval1 <- multipatt(
  x = matriz_abundancia, 
  cluster = vector_grupos, 
  func = "IndVal.g",    # Corrección para grupos desiguales
  control = ctrl,       # ¡Aquí aplicamos los bloques por año!
  duleg = TRUE          # TRUE si solo quieres Dry vs Wet puros. FALSE si aceptas combinaciones (no aplica si solo hay 2 grupos)
)

# Ejecutar IndVal
resultado_indval2 <- multipatt(
  x = matriz_X, 
  cluster = vector_grupos, 
  func = "IndVal.g",    # Corrección para grupos desiguales
  control = ctrl,       # ¡Aquí aplicamos los bloques por año!
  duleg = TRUE          # TRUE si solo quieres Dry vs Wet puros. FALSE si aceptas combinaciones (no aplica si solo hay 2 grupos)
)



cat("\n RESULTADOS: ABUNDANCIA (Top especies) \n")
summary(resultado_indval1, indvalcomp = TRUE)



cat("\n RESULTADOS: INCIDENCIA (Top especies) \n")
summary(resultado_indval2, indvalcomp = TRUE)



## SIMPER ------


cat("\n── PREPARACIÓN DE DATOS PARA SIMPER (A nivel de Ecosistema) ──\n")

# 1. Agrupar y Colapsar (Unidad: Año - Temporada)
# Ignoramos la trampa para obtener la biomasa ecosistémica total del semestre
datos_simper <- df11 %>%
  mutate(Season = if_else(str_detect(seasonYear, "Dry"), "Dry", "Wet")) %>%
  group_by(sample_year, Season, name) %>%
  summarise(abundancia_semestral = sum(hormigas_abundance), .groups = "drop")

# 2. Crear la Matriz de Comunidad Macroscópica
matriz_simper <- datos_simper %>%
  dplyr::select(sample_year, Season, name, abundancia_semestral) %>%
  pivot_wider(names_from = name, 
              values_from = abundancia_semestral, 
              values_fill = 0)

# 3. Separar metadatos y matriz numérica
metadatos_simper <- matriz_simper %>% dplyr::select(sample_year, Season)
matriz_Y <- matriz_simper %>% dplyr::select(-sample_year, -Season)

# 4. Ejecutar SIMPER
# El vector de grupos es simplemente si ese semestre fue Dry o Wet
resultado_simper_final <- simper(matriz_Y, group = metadatos_simper$Season)

cat("\n RESULTADOS: SIMPER (Aporte a la disimilitud Bray-Curtis) \n")
summary(resultado_simper_final)





# 3. SELECCIÓN DE COVARIABLES CLIMÁTICAS RELEVANTES -----------------------

df <- readRDS(here::here("1. Data", "Procesada", "df_final_filtrado.RDS"))
df_clima <- readRDS(here::here("1. Data", "Procesada", "df_clima_final_filtrado.RDS"))


especies_finales <- c("Rasopone_arhuaca",
                      "Ectatomma_ruidum",
                      "Anochetus_diegensis",
                      "Cryptopone_gilva",
                      "Rogeria_ACH0205")



df <- df %>% filter(name %in% especies_finales)



# A. Matriz Biótica Estacionaria (Y) 
# Filas = Especies (n), Columnas = Semestres (T). Dimensiones: [n x T]
Y_z_matrix <- df %>%
  dplyr::select(name, seasonYear, var_interes) %>%
  tidyr::pivot_wider(names_from = seasonYear, values_from = var_interes) %>%
  tibble::column_to_rownames("name") %>%
  as.matrix()


cat(sprintf("  ✓ Matriz Y generada: [%d especies × %d semestres]\n", 
            nrow(Y_z_matrix), ncol(Y_z_matrix)))


# C. Matriz Climática (C)
# Excluimos variables de control y la llave temporal. Dimensiones: [T x covariables]
C_matrix <- df_clima %>%
  tibble::column_to_rownames("seasonYear") %>%
  #dplyr::select(-dummy_dry, -dummy_wet) %>% # Retira dummies si solo quieres numéricas continuas
  as.matrix()



analizador_correlaciones <- function(df_bio, 
                                     df_clima, 
                                     var_respuesta, 
                                     metodo = "pearson") {
  # Identificamos el vector de especies disponibles (ej. las 5 ganadoras)
  especies_disponibles <- unique(df_bio$name)
  
  # Filtramos solo las variables climáticas numéricas (quitamos dummies y fechas)
  vars_climaticas <- df_clima %>% 
    dplyr::select(where(is.numeric), -any_of(c("dummy_dry", "dummy_wet"))) %>% 
    colnames()
  
  # Unimos los datasets por el semestre para alinear las observaciones
  df_unido <- dplyr::inner_join(df_bio, df_clima, by = "seasonYear")
  
  # Bucle 1: Iteramos por cada variable climática
  tabla_final <- lapply(vars_climaticas, function(climatica_actual) {
    
    # Creamos un vector vacío para guardar las correlaciones de esta variable
    correlaciones_absolutas <- numeric(length(especies_disponibles))
    
    # Bucle 2: Calculamos la correlación con cada especie
    for (i in seq_along(especies_disponibles)) {
      especie_actual <- especies_disponibles[i]
      
      # Filtramos la data solo para la especie en turno
      data_especie <- df_unido %>% dplyr::filter(name == especie_actual)
      
      # Calculamos la correlación y extraemos su valor absoluto
      valor_corr <- cor(data_especie[[var_respuesta]], 
                        data_especie[[climatica_actual]], 
                        use = "complete.obs", 
                        method = metodo)
      
      correlaciones_absolutas[i] <- abs(valor_corr)
    }
    
    # Reportamos las métricas de esta variable climática
    data.frame(
      Variable_Climatica = climatica_actual,
      Corr_Abs_Media = mean(correlaciones_absolutas, na.rm = TRUE),
      Corr_Abs_Min = min(correlaciones_absolutas, na.rm = TRUE),
      Corr_Abs_Max = max(correlaciones_absolutas, na.rm = TRUE),
      N_Especies = sum(!is.na(correlaciones_absolutas))
    )
    
  }) %>% dplyr::bind_rows()
  
  # Ordenamos la tabla de mayor a menor media
  tabla_final_ordenada <- tabla_final %>% dplyr::arrange(desc(Corr_Abs_Media))
  
  return(tabla_final_ordenada)
}


ranking_covariables <- analizador_correlaciones(
  df_bio = df, 
  df_clima = df_clima, 
  var_respuesta = "ln_incidence",
  metodo = "pearson"
)


# View(ranking_covariables)




# Sincronización Temporal (Aseguramos mismo orden entre biología y clima)
semestres_orden <- colnames(Y_z_matrix)
df_clima_candidatas <- df_clima %>%
  dplyr::arrange(match(seasonYear, semestres_orden))

sp_names <- rownames(Y_z_matrix)
z_vars <- df_clima_candidatas %>% 
  dplyr::select(starts_with("ln_")) %>% 
  colnames()

# **************************************************
# FASE 1: FILTRADO POR RELEVANCIA (Correlación > 0.20)
# ***********************************************

cat("[1] Calculando relevancia cruzada...\n")

# Correlación absoluta media de cada covariable climática vs las especies
avg_cor <- sapply(z_vars, function(v) {
  mean(sapply(sp_names, function(sp) {
    abs(cor(Y_z_matrix[sp, ], df_clima_candidatas[[v]], use = "complete.obs"))
  }))
})

# Filtrar y ordenar de mayor a menor impacto
ranking_covariables <- data.frame(
  Variable_Climatica = names(avg_cor),
  Corr_Abs_Media = avg_cor
) %>%
  dplyr::filter(Corr_Abs_Media > 0.20) %>%
  dplyr::arrange(desc(Corr_Abs_Media))

input_perfecto_vif <- ranking_covariables$Variable_Climatica
cat(sprintf("  ✓ Variables que superan umbral 20%%: %d variables\n", length(input_perfecto_vif)))


# **************************************************
# FASE 2: FILTRADO VIF ITERATIVO (Poda por multicolinealidad)
# ***********************************************

cat("[2] Ejecutando algoritmo Greedy-VIF...\n")

select_vif_robust_fixed <- function(vars, data, threshold = 10) {
  if(length(vars) == 0) return(character(0))
  
  selected <- vars[1] 
  
  if(length(vars) > 1) {
    for (v in vars[-1]) {
      current_set <- c(selected, v)
      
      # Si hay 2 variables, evaluamos correlación directa
      if (length(current_set) < 3) {
        if(abs(cor(data[[current_set[1]]], data[[current_set[2]]], use="complete.obs")) > 0.8) {
          next 
        }
        selected <- current_set
        next
      }
      
      # Si hay >= 3 variables, calculamos modelo y VIF
      form <- as.formula(paste(current_set[1], "~", paste(current_set[-1], collapse = "+")))
      modelo_test <- suppressWarnings(lm(form, data = data))
      
      # Blindaje contra matrices singulares (colinealidad perfecta)
      if (any(is.na(coef(modelo_test)))) {
        vif_val <- 999 
      } else {
        vif_val <- tryCatch(max(car::vif(modelo_test)), error = function(e) 999)
      }
      
      # Retener variable si no supera el umbral VIF
      if (vif_val < threshold) selected <- current_set
      if (length(selected) >= 17) break # Tope máximo para grados de libertad
    }
  }
  return(selected)
}

vars_definitivas_varx <- select_vif_robust_fixed(input_perfecto_vif, df_clima_candidatas)

cat(sprintf("  ✓ Retenidas por el algoritmo: %d covariables.\n", length(vars_definitivas_varx)))


# **************************************************
# FASE 3: EXTRACCIÓN Y VALIDACIÓN DE VALORES VIF DEFINITIVOS
# ***********************************************

cat("\n[3] Calculando la matriz de diagnóstico VIF final...\n")

if (length(vars_definitivas_varx) >= 2) {
  # Generamos un Y aleatorio para forzar el cálculo de la matriz X climática completa
  set.seed(201821199)
  y_dummy <- rnorm(nrow(df_clima_candidatas))
  df_vif_final <- as.data.frame(df_clima_candidatas[, vars_definitivas_varx, drop = FALSE])
  
  # Ajustamos el modelo lineal de diagnóstico
  modelo_vif_final <- lm(y_dummy ~ ., data = df_vif_final)
  vif_valores <- car::vif(modelo_vif_final)
  
  # Construimos la tabla de reporte 
  tabla_vif_reporte <- data.frame(
    Variable_Climatica = names(vif_valores),
    VIF_Calculado = round(as.numeric(vif_valores), 4),
    Umbral_4.0 = ifelse(vif_valores < 10.0, "Cumple (SÍ)", "Falla (NO)"),
    stringsAsFactors = FALSE
  ) %>% dplyr::arrange(VIF_Calculado)
  
} else if (length(vars_definitivas_varx) == 1) {
  tabla_vif_reporte <- data.frame(
    Variable_Climatica = vars_definitivas_varx,
    VIF_Calculado = 1.0000,
    Umbral_4.0 = "Cumple (SÍ)",
    stringsAsFactors = FALSE
  )
} else {
  tabla_vif_reporte <- data.frame(Variable_Climatica = character(0), VIF_Calculado = numeric(0), Umbral_4.0 = character(0))
}


cat("\n========================================================================\n")
print(tabla_vif_reporte, row.names = FALSE)
cat("========================================================================\n")















