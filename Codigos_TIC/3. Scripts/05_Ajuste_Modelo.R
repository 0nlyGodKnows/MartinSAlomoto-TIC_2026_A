# ********************************************************* 
# AJUSTE DEL MODELO MULTIVARIADO VARX 
# ************************************************************

# ********************************************************* 
# 0. CONFIGURACION DEL ENTORNO Y CARGA DE DATOS ----------
# ************************************************************

# Carga de librerias de soporte (vars, lmtest, igraph, tidyverse, ggplot2)
source(here::here("2. Librerias - Funciones", "Librerias.R"))
library(ggplot2)

df <- readRDS(here::here("1. Data", "Procesada", "df_final_filtrado.RDS"))
df_clima <- readRDS(here::here("1. Data", "Procesada", "df_clima_final_filtrado.RDS"))

# Variable biologica objetivo para las matrices de comunidad
var_interes <- "ln_incidence"

# ********************************************************* 
# 1. ALGORITMO DE SELECCIÓN DE VARIABLES EXÓGENAS ---------
# ************************************************************

# A. Matriz Biotica Estacionaria Base (Y) 
Y_z_matrix <- df %>%
  dplyr::select(name, seasonYear, all_of(var_interes)) %>%
  tidyr::pivot_wider(names_from = seasonYear, values_from = all_of(var_interes)) %>%
  tibble::column_to_rownames("name") %>%
  as.matrix()

cat(sprintf("  ✓ Matriz Y generada: [%d especies x %d semestres]\n", 
            nrow(Y_z_matrix), ncol(Y_z_matrix)))

# B. Sincronizacion del dataframe de covariables climaticas
semestres_orden <- colnames(Y_z_matrix)
df_clima_candidatas <- df_clima %>%
  dplyr::arrange(match(seasonYear, semestres_orden))

# C. Inyeccion de Variables Dummy para Eventos Extremos (Blindaje del Modelo)
semestre_catastrofe_purisima <- "MM-2011-Dry" 
semestres_catastrofe_post_nino <- c("MM-2016-Wet", "MM-2017-Wet")

df_clima_candidatas <- df_clima_candidatas %>%
  dplyr::mutate(
    dummy_purisima = ifelse(seasonYear == semestre_catastrofe_purisima, 1, 0),
    dummy_post_nino = ifelse(seasonYear %in% semestres_catastrofe_post_nino, 1, 0)
  )

# ********************************************************* 
# 2. SELECCIÓN DE VARIABLE EXÓGENA ---------
# ************************************************************

# Vector de la comunidad indicadora fija
especies_fijas <- c(
  "Rasopone_arhuaca",
  "Ectatomma_ruidum",
  "Anochetus_diegensis",
  "Cryptopone_gilva",
  "Rogeria_ACH0205"
)

# Controles deterministicos fijos (absorcion de estacionalidad y shocks)
variables_control_fijas <- c(
  "dummy_purisima",
  "dummy_post_nino",
  "dummy_wet"
)

# Pool exogeno optimizado libre de multicolinealidad estructural (VIF < 10)
pool_clima <- c(
  "ln_ra_sem2_lag_avg", 
  "ln_ra_sem1_lag", 
  "ln_ra_sem3_lag", 
  "ln_ra_week2_lag",
  "ln_ra_in_season_avg_cont",
  "ln_et_sem2_lag",
  "ln_et_sem4_lag_avg", 
  "ln_atmx_sem4_lag_avg",
  "ln_rh_sem1_lag"
)

k_clima_extra <- 3 # Numero de covariables rotativas por combinacion

# Construccion de la matriz exogena global balanceada
Y_fija <- t(Y_z_matrix)[, especies_fijas]
todas_las_exogenas <- c(variables_control_fijas, pool_clima)

C_full_mat <- df_clima_candidatas %>% 
  dplyr::arrange(match(seasonYear, colnames(Y_z_matrix))) %>%
  tibble::column_to_rownames("seasonYear") %>% 
  dplyr::select(any_of(todas_las_exogenas)) %>%
  as.matrix()

mode(C_full_mat) <- "numeric"

# Generacion factorial de las combinaciones exogenas
comb_clima <- combn(pool_clima, k_clima_extra, simplify = FALSE)



# ********************************************************* 
# 3. EJECUCIÓN DEL ALGORITMO DE SELECCIÓN ------------
# ************************************************************

lista_resultados <- list()
modelos_exitosos <- 0
modelos_singulares <- 0

for (j in seq_along(comb_clima)) {
  
  cl_test <- comb_clima[[j]]
  vector_exogeno_actual <- c(variables_control_fijas, cl_test)
  X_test <- C_full_mat[, vector_exogeno_actual, drop = FALSE]
  
  resultado_evaluacion <- tryCatch(
    {
      modelo_test <- suppressWarnings(vars::VAR(Y_fija, p = 1, exogen = X_test, type = "const"))
      
      aic_val <- AIC(modelo_test)
      bic_val <- BIC(modelo_test)
      raices <- max(vars::roots(modelo_test))
      es_stable <- if(raices < 1) "SÍ" else "NO"
      
      resumen <- summary(modelo_test)
      coef_sig <- 0
      for (eq in resumen$varresult) {
        p_valores <- eq$coefficients[, 4]
        coef_sig <- coef_sig + sum(p_valores < 0.05, na.rm = TRUE)
      }
      
      p_serial <- tryCatch(vars::serial.test(modelo_test, lags.pt = 4, type = "PT.asymptotic")$serial$p.value, error = function(e) NA)
      pass_serial <- ifelse(is.na(p_serial), "Error", ifelse(p_serial > 0.05, "SÍ", "NO"))
      
      p_norm <- tryCatch(vars::normality.test(modelo_test, multivariate.only = TRUE)$jb.mul$JB$p.value, error = function(e) NA)
      pass_norm <- ifelse(is.na(p_norm), "Error", ifelse(p_norm > 0.05, "SÍ", "NO"))
      
      p_arch <- tryCatch(vars::arch.test(modelo_test, lags.multi = 2, multivariate.only = TRUE)$arch.mul$p.value, error = function(e) NA)
      pass_arch <- ifelse(is.na(p_arch), "Error", ifelse(p_arch > 0.05, "SÍ", "NO"))
      
      data.frame(
        Controles = paste(variables_control_fijas, collapse = " | "),
        Clima_Rotativo = paste(cl_test, collapse = " | "),
        AIC = round(aic_val, 4),
        BIC = round(bic_val, 4),
        Max_Raiz = round(raices, 4),
        Coef_Sig = coef_sig,
        Estable = es_stable,
        Serial_OK = pass_serial,
        Norm_OK = pass_norm,
        ARCH_OK = pass_arch,
        stringsAsFactors = FALSE
      )
    },
    error = function(e) { return(NULL) }
  )
  
  if (!is.null(resultado_evaluacion)) {
    modelos_exitosos <- modelos_exitosos + 1
    lista_resultados[[modelos_exitosos]] <- resultado_evaluacion
  } else {
    modelos_singulares <- modelos_singulares + 1
  }
}

# Consolidacion jerarquica del Ranking Total
if(modelos_exitosos > 0) {
  ranking_total <- dplyr::bind_rows(lista_resultados)
  cat(sprintf("  ✓ Torneo completado con exito.\n    - Modelos estables computados: %d\n    - Convergencias fallidas: %d\n", 
              modelos_exitosos, modelos_singulares))
} else {
  stop("Alerta: Todos los modelos multivariados configurados colapsaron por singularidad.")
}



# ********************************************************* 
# 4. EXTRACCION Y DESPLIEGUE DEL TOP RANKING OPTIMO --------
# ************************************************************

ranking_estables <- ranking_total %>% 
  dplyr::filter(Estable == "SÍ")

top_ranking <- ranking_estables %>% 
  dplyr::arrange(AIC, desc(Coef_Sig)) %>%
  head(10)

cat("\n==================================================================================================\n")
cat("      TOP 10 VECTORES CLIMÁTICOS \n")
cat("==================================================================================================\n")
print(top_ranking %>% 
        dplyr::select(Clima_Rotativo, AIC, BIC, Coef_Sig, Max_Raiz, Serial_OK, Norm_OK, ARCH_OK), row.names = FALSE)

cat("\n(Combinacion Ganadora Validada):\n")
cat("Variables de Control : ", top_ranking$Controles[1], "\n")
cat("Clima Extraido       : ", top_ranking$Clima_Rotativo[1], "\n")



# ********************************************************* 
# 5. AJUSTE DEL MODELO DEFINITIVO ----------
# ************************************************************

especies_actuales <- c(
  "Anochetus_diegensis",
  "Ectatomma_ruidum",
  "Rogeria_ACH0205",
  "Rasopone_arhuaca",
  "Cryptopone_gilva"
)

# Mapeo del vector exogeno ganador (actualizado con post_nino)
variables_exogenas <- c(
  "ln_et_sem2_lag",
  "ln_et_sem4_lag_avg",
  "ln_ra_week2_lag",
  "dummy_wet",
  "dummy_post_nino",
  "dummy_purisima"
)

Y_matrix_final <- df %>%
  dplyr::filter(name %in% especies_actuales) %>%
  dplyr::select(name, seasonYear, all_of(var_interes)) %>%
  tidyr::pivot_wider(names_from = name, values_from = all_of(var_interes)) %>%
  tibble::column_to_rownames("seasonYear") %>%
  as.matrix()

C_matrix_final <- df_clima_candidatas %>%
  dplyr::arrange(match(seasonYear, rownames(Y_matrix_final))) %>%
  tibble::column_to_rownames("seasonYear") %>%
  dplyr::select(any_of(variables_exogenas)) %>%
  as.matrix()

mode(C_matrix_final) <- "numeric"

# ********************************************************* 
# 6. ANALISIS DE CAUSALIDAD DIRECCIONAL EN EL SENTIDO DE GRANGER-CHOLESKY) ----------
# ************************************************************

cat("\n[5] Evaluando topologia de interaccion biologica mediante Causalidad de Granger...\n")

granger_results <- expand.grid(Causa = especies_actuales, Efecto = especies_actuales, stringsAsFactors = FALSE) %>%
  dplyr::filter(Causa != Efecto) %>%
  dplyr::mutate(p_value = sapply(1:n(), function(i) {
    vec_causa <- as.numeric(Y_matrix_final[, Causa[i]])
    vec_efecto <- as.numeric(Y_matrix_final[, Efecto[i]])
    test <- tryCatch(lmtest::grangertest(vec_efecto ~ vec_causa, order = 1),
                     error = function(e) list(`Pr(>F)` = c(NA, 1)))
    return(test$`Pr(>F)`[2])
  }))

dominancia <- granger_results %>%
  dplyr::group_by(Causa) %>%
  dplyr::summarise(influencia = sum(p_value < 0.05, na.rm = TRUE),
                   fuerza = min(p_value, na.rm = TRUE)) %>%
  dplyr::arrange(desc(influencia), fuerza)

orden_cholesky <- dominancia$Causa 
Y_ordenado <- Y_matrix_final[, orden_cholesky]

cat("  ✓ Orden Jerarquico de Cholesky estructural:", paste(orden_cholesky, collapse=" -> "), "\n")

# ********************************************************* 
# 7. DIAGNOSIS DEL MODELO VARX ---------
# ************************************************************

cat("\n[6] Ajustando ecuacion fundamental del VARX con matriz indexada...\n")

rezagos_endogenos <- 1
varx_final <- vars::VAR(y = Y_ordenado, p = rezagos_endogenos, exogen = C_matrix_final, type = "const")

raices <- vars::roots(varx_final)
cat("  ✓ Maximo modulo de raices caracteristicas inverso:", round(max(raices), 4), "\n")
if(max(raices) >= 1) warning("Alerta del sistema: ¡El sistema dinamico estimado es inestable!") else cat("  ✓ Sistema Dinamico Estable Confirmado.\n")

cat("\n==================================================================================================\n")
cat("      REPORTES DE DIAGNÓSTICO DEL MODELO VARX\n")
cat("==================================================================================================\n")

print(vars::serial.test(varx_final, lags.pt = 4, type = "PT.asymptotic"))
print(vars::normality.test(varx_final, multivariate.only = TRUE))
print(vars::arch.test(varx_final, lags.multi = 2, multivariate.only = TRUE))

# ********************************************************* 
# 8. TRAYECTORIAS SIGNIFICATIVAS DEL IRF (BOOTSTRAP) ----------
# ************************************************************

set.seed(201821199) 
irf_boot <- vars::irf(varx_final, n.ahead = 6, boot = TRUE, runs = 1000, ci = 0.95)

resultados_sig <- data.frame()

for(imp in names(irf_boot$irf)) {
  for(resp in colnames(irf_boot$irf[[imp]])) {
    irf_val <- irf_boot$irf[[imp]][, resp]
    lower_val <- irf_boot$Lower[[imp]][, resp]
    upper_val <- irf_boot$Upper[[imp]][, resp]
    
    for(sem in 1:length(irf_val)) {
      significancia <- ifelse(lower_val[sem] > 0, "Positivo (+)",
                              ifelse(upper_val[sem] < 0, "Negativo (-)", "No Significativo"))
      
      if(significancia != "No Significativo") {
        if(!(imp == resp && sem == 1)) {
          resultados_sig <- rbind(resultados_sig, data.frame(
            Impulso = imp, 
            Respuesta = resp, 
            Semestre_Impacto = sem - 1, 
            Direccion = significancia,
            IRF_Media = round(irf_val[sem], 4),
            IC_95 = paste0("[", round(lower_val[sem], 4), " , ", round(upper_val[sem], 4), "]")
          ))
        }
      }
    }
  }
}

cat("\n==================================================================\n")
cat("   TRAYECTORIAS ESTADÍSTICAMENTE SIGNIFICATIVAS (IC 95%)\n")
cat("==================================================================\n")
print(resultados_sig)



# ********************************************************* 
# 9. DIAGRAMA IRF (MATRIZ 5x5 GGPLOT2) Y FEVD  -----------
# ************************************************************



cat("\n[+] Construyendo Matriz IRF 5x5 y extrayendo FEVD...\n")

# Función para extraer y tabular los datos del objeto IRF
extraer_irf_ggplot <- function(irf_obj) {
  df_list <- list()
  for(imp in names(irf_obj$irf)) {
    for(resp in colnames(irf_obj$irf[[imp]])) {
      temp_df <- data.frame(
        Horizonte = 0:(nrow(irf_obj$irf[[imp]]) - 1),
        Impulso = imp, 
        Respuesta = resp,
        Estimacion = irf_obj$irf[[imp]][, resp],
        Linf = irf_obj$Lower[[imp]][, resp],
        Lsup = irf_obj$Upper[[imp]][, resp]
      )
      df_list[[paste(imp, resp)]] <- temp_df
    }
  }
  return(do.call(rbind, df_list))
}

df_irf <- extraer_irf_ggplot(irf_boot)

# LIMPIEZA DE NOMBRES Y REORDENAMIENTO DE CHOLESKY
# Reemplazamos los guiones bajos por espacios para cumplir con el formato de la tesis
df_irf <- df_irf %>%
  dplyr::mutate(
    Impulso = gsub("_", " ", Impulso),
    Respuesta = gsub("_", " ", Respuesta)
  )

# Modificamos los niveles de Cholesky para que coincidan con las cadenas limpias
orden_cholesky_limpio <- gsub("_", " ", orden_cholesky)

df_irf$Impulso <- factor(df_irf$Impulso, levels = orden_cholesky_limpio)
df_irf$Respuesta <- factor(df_irf$Respuesta, levels = orden_cholesky_limpio)

# Renderizado de la matriz 5x5 con calidad de publicación e impresión
grafico_final <- ggplot(df_irf, aes(x = Horizonte, y = Estimacion)) +
  geom_hline(yintercept = 0, color = "red", linetype = "solid", linewidth = 0.6) +
  geom_ribbon(aes(ymin = Linf, ymax = Lsup), fill = "grey70", alpha = 0.5) +
  geom_line(color = "black", linewidth = 0.8) +
  facet_grid(Respuesta ~ Impulso, scales = "free_y", switch = "y") +
  theme_bw() +
  labs(x = "Semestres (Horizonte)", 
       y = "Respuesta de la Especie (Log-Incidencia)",
       title = "Funciones de Impulso-Respuesta Estructurales (IRF)",
       subtitle = "Bandas de confianza empíricas Bootstrap al 95% (1000 réplicas)") +
  theme(
    # Configuración de los bloques externos (Facetas)
    strip.background = element_rect(fill = "grey95", color = "black"),
    # Se fuerza negrita y cursiva para los nombres científicos de las hormigas a 11pt
    strip.text = element_text(face = "bold.italic", size = 13, color = "black"),
    strip.placement = "outside",
    
    # Escalado de textos de los ejes a 11pt y títulos a 13pt
    axis.text.x = element_text(color = "black", size = 14),
    axis.text.y = element_text(color = "black", size = 14),
    axis.title = element_text(face = "bold", size = 17, color = "black"),
    
    # Títulos principales escalados para impresión en formato apaisado
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, color = "grey30", size = 16),
    
    # Ajuste de espacio interno para evitar amontonamientos
    panel.spacing = unit(0.5, "lines")
  )

# Guardar la imagen en formato apaisado de alta definición
ggsave("irf_matriz_5x5_definitiva.png", plot = grafico_final, width = 17, height = 10.5, dpi = 300)
cat("\n  ✓ Gráfico IRF guardado como 'irf_matriz_5x5_definitiva.png'.\n")



# Cálculo e impresión del FEVD para tu tabla en LaTeX
fevd_datos <- vars::fevd(varx_final, n.ahead = 6)
cat("\n--- FEVD: Porcentaje de varianza explicada en el Semestre 1, 3 y 6 ---\n")
for(especie in names(fevd_datos)) {
  cat("\nVariable Respuesta:", especie, "\n")
  print(round(fevd_datos[[especie]][c(1, 3, 6), ] * 100, 2))
}





# 10. MATRIZ VISUAL DE SIGNIFICANCIA DE PARÁMETROS --------


cat("\n[+] Construyendo Matriz Visual de Significancia...\n")

# 1. Extraer los coeficientes y p-valores de cada ecuación del VARX
resumen_varx <- summary(varx_final)
df_coefs <- data.frame()

for(eq_name in names(resumen_varx$varresult)) {
  matriz_coef <- resumen_varx$varresult[[eq_name]]$coefficients
  temp_df <- data.frame(
    Respuesta = eq_name,
    Predictor = rownames(matriz_coef),
    Estimacion = matriz_coef[, "Estimate"],
    P_valor = matriz_coef[, "Pr(>|t|)"],
    stringsAsFactors = FALSE
  )
  df_coefs <- rbind(df_coefs, temp_df)
}

# 2. Limpieza de nombres y clasificación de significancia
df_coefs <- df_coefs %>%
  dplyr::mutate(
    # Limpiamos el ".l1" que R le pone a los rezagos
    Predictor = str_replace(Predictor, "\\.l1$", " (Rezago)"),
    Predictor = ifelse(Predictor == "const", "Intercepto", Predictor),
    
    # Lógica condicional para las flechas (Alfa = 0.10 según tu última actualización)
    Direccion = case_when(
      P_valor < 0.10 & Estimacion > 0 ~ "Positivo (+)",
      P_valor < 0.10 & Estimacion < 0 ~ "Negativo (-)",
      TRUE ~ "No Significativo"
    )
  )

# Bloqueamos el orden de las columnas (Respuestas) según tu Cholesky
df_coefs$Respuesta <- factor(df_coefs$Respuesta, levels = orden_cholesky)

# Bloqueamos el orden de las filas (Para que Intercepto y Clima queden arriba)
orden_predictores <- unique(df_coefs$Predictor)
orden_predictores <- c("Intercepto", setdiff(orden_predictores, "Intercepto"))
df_coefs$Predictor <- factor(df_coefs$Predictor, levels = rev(orden_predictores))

# 3. Generación del gráfico con ggplot2 optimizado para impresión
grafico_flechas <- ggplot(df_coefs, aes(x = Respuesta, y = Predictor)) +
  # Fondo de cuadrícula tipo Excel
  geom_tile(color = "black", fill = "white", linewidth = 0.35) +
  
  # Añadir los triángulos aumentando el tamaño para legibilidad de impresión (size = 8)
  geom_point(aes(shape = Direccion, color = Direccion, fill = Direccion), size = 5) +
  
  # Configuración exacta de colores y formas
  scale_shape_manual(values = c("Positivo (+)" = 24, "Negativo (-)" = 25, "No Significativo" = NA), 
                     breaks = c("Positivo (+)", "Negativo (-)")) +
  scale_color_manual(values = c("Positivo (+)" = "forestgreen", "Negativo (-)" = "firebrick", "No Significativo" = "transparent"), 
                     breaks = c("Positivo (+)", "Negativo (-)")) +
  scale_fill_manual(values = c("Positivo (+)" = "forestgreen", "Negativo (-)" = "firebrick", "No Significativo" = "transparent"), 
                    breaks = c("Positivo (+)", "Negativo (-)")) +
  
  # Limpieza de guiones bajos en el eje X (formato estético para nombres científicos)
  scale_x_discrete(labels = function(x) gsub("_", " ", x)) +
  
  # Estética limpia y escalado de fuentes
  theme_minimal() +
  labs(
    title = "Matriz de Significancia de Coeficientes (Modelo VARX)",
    subtitle = "Efectos directos estimados. Nivel de significancia \U03B1 = 0.10",
    x = "Especie Receptora", 
    y = "Variable Predictora"
  ) +
  theme(
    # Eje X: Cursiva y negrita por tratarse de especies, tamaño 11pt
    axis.text.x = element_text(angle = 45, hjust = 1, color = "black", size = 12),
    # Eje Y: Negrita para variables climáticas y rezagos, tamaño 11pt
    axis.text.y = element_text(color = "black", size = 12),
    # Títulos de los ejes a 12pt
    axis.title = element_text(face = "bold", color = "black", size = 12),
    # Título principal a 15pt y subtítulo a 12pt
    plot.title = element_text(face = "bold", hjust = 0.5, size = 15),
    plot.subtitle = element_text(hjust = 0.5, color = "grey30", size = 12),
    # Texto de la leyenda a 11pt
    legend.text = element_text(size = 13, face = "bold"),
    panel.grid = element_blank(),
    legend.position = "right",
    legend.title = element_blank()
  )

# 4. Guardar en alta calidad (Mantenemos las proporciones físicas)
ggsave("matriz_significancia_flechas.png", plot = grafico_flechas, width = 10, height = 6, dpi = 300)



















# 
# cat("\n[+] Construyendo Matriz Visual de Significancia (Con Valores)...\n")
# 
# # 1. Extraer los coeficientes y p-valores de cada ecuación del VARX
# resumen_varx <- summary(varx_final)
# df_coefs <- data.frame()
# 
# for(eq_name in names(resumen_varx$varresult)) {
#   matriz_coef <- resumen_varx$varresult[[eq_name]]$coefficients
#   temp_df <- data.frame(
#     Respuesta = eq_name,
#     Predictor = rownames(matriz_coef),
#     Estimacion = matriz_coef[, "Estimate"],
#     P_valor = matriz_coef[, "Pr(>|t|)"],
#     stringsAsFactors = FALSE
#   )
#   df_coefs <- rbind(df_coefs, temp_df)
# }
# 
# # 2. Limpieza, clasificación y formateo del texto
# df_coefs <- df_coefs %>%
#   dplyr::mutate(
#     Predictor = str_replace(Predictor, "\\.l1$", " (Rezago)"),
#     Predictor = ifelse(Predictor == "const", "Intercepto", Predictor),
#     
#     Direccion = case_when(
#       P_valor < 0.05 & Estimacion > 0 ~ "Positivo (+)",
#       P_valor < 0.05 & Estimacion < 0 ~ "Negativo (-)",
#       TRUE ~ "No Significativo"
#     ),
#     
#     # Creamos la etiqueta numérica (2 decimales) solo para los significativos
#     Etiqueta_Num = ifelse(Direccion != "No Significativo", sprintf("%.2f", Estimacion), "")
#   )
# 
# # Bloqueamos el orden estructural
# df_coefs$Respuesta <- factor(df_coefs$Respuesta, levels = orden_cholesky)
# orden_predictores <- unique(df_coefs$Predictor)
# orden_predictores <- c("Intercepto", setdiff(orden_predictores, "Intercepto"))
# df_coefs$Predictor <- factor(df_coefs$Predictor, levels = rev(orden_predictores)) 
# 
# # 3. Generación del gráfico
# grafico_flechas <- ggplot(df_coefs, aes(x = Respuesta, y = Predictor)) +
#   geom_tile(color = "black", fill = "white", linewidth = 0.3) +
#   
#   # Capa 1: Flechas desplazadas ligeramente hacia arriba (y = 0.15)
#   geom_point(aes(shape = Direccion, color = Direccion, fill = Direccion), 
#              size = 6, position = position_nudge(y = 0.25)) +
#   
#   # Capa 2: Texto del estimador desplazado ligeramente hacia abajo (y = -0.15)
#   geom_text(aes(label = Etiqueta_Num), 
#             size = 3.5, fontface = "bold", color = "black", 
#             position = position_nudge(y = -0.50)) +
#   
#   # Control de formas y colores (El argumento 'breaks' purga la leyenda)
#   scale_shape_manual(values = c("Positivo (+)" = 24, "Negativo (-)" = 25, "No Significativo" = NA), 
#                      breaks = c("Positivo (+)", "Negativo (-)")) +
#   scale_color_manual(values = c("Positivo (+)" = "forestgreen", "Negativo (-)" = "firebrick", "No Significativo" = "transparent"), 
#                      breaks = c("Positivo (+)", "Negativo (-)")) +
#   scale_fill_manual(values = c("Positivo (+)" = "forestgreen", "Negativo (-)" = "firebrick", "No Significativo" = "transparent"), 
#                     breaks = c("Positivo (+)", "Negativo (-)")) +
#   
#   # Estética
#   theme_minimal() +
#   labs(
#     title = "Matriz de Significancia y Estimación de Coeficientes (VARX)",
#     subtitle = "Valores reportados corresponden a efectos significativos (\U03B1 = 0.05)",
#     x = "Especie Receptora (Ecuación)", 
#     y = "Variable Predictora"
#   ) +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", color = "black"),
#     axis.text.y = element_text(face = "bold", color = "black"),
#     plot.title = element_text(face = "bold", hjust = 0.5),
#     plot.subtitle = element_text(hjust = 0.5, color = "grey40"),
#     panel.grid = element_blank(),
#     legend.position = "bottom",
#     legend.title = element_blank()
#   )
# 
# # 4. Guardar
# ggsave("matriz_significancia_con_valores.png", plot = grafico_flechas, width = 10, height = 8, dpi = 300)
# cat("\n  ✓ Matriz guardada exitosamente como 'matriz_significancia_con_valores.png'.\n")
# 
# 
# 
# 
# 
# 



