# ************************************************************
# SECCIÓN 1: DEPURACIÓN Y TRATAMIENTO DE DATOS (01) ----
# ************************************************************


# Función para leer y pivotar índices climáticos
preparar_indice <- function(file_name, col_name) {
  idx <- read.table(here::here("1. Data", "Original", file_name), 
                    header = FALSE, sep = "", 
                    stringsAsFactors = FALSE)
  colnames(idx) <- months
  idx %>%
    pivot_longer(cols = '01':'12', names_to = "month", values_to = col_name) %>%
    mutate(periodo = paste0(year, month),
           date_obj = as.Date(paste0(periodo, "01"), format = "%Y%m%d")) %>%
    filter(year >= 2000) %>%
    dplyr::select(-year, -month)
}






condensar_trampas <- function(df, df_grupos) {
  df %>%
    mutate(trap = as.character(trap)) %>%
    left_join(df_grupos, by = "trap") %>%
    group_by(name, sample_year, seasonYear, trap_group) %>%
    summarise(
      # Biología Cruda
      hormigas_abundance = sum(hormigas_abundance, na.rm = TRUE),
      hormigas_incidence = sum(hormigas_incidence, na.rm = TRUE),
      # hormigas_incidence_antigua = sum(hormigas_incidence_antigua, na.rm = TRUE),
      
      # Control y Clima (Usamos first porque son idénticos por semestre)
      semanas_presencia = first(semanas_presencia),
      fechas_presencia  = first(fechas_presencia),
      fechas_inicio     = first(fechas_inicio),
      dry               = first(dry),
      across(.cols = matches("_lag|_in_season|ninio|oni"), .fns = first),
      across(.cols = starts_with("z_"), .fns = first), # Aseguramos llevar los z-scores
      .groups = "drop"
    ) %>%
    mutate(
      ln_abundance = log(hormigas_abundance + 1),
      ln_incidence = log(hormigas_incidence + 1)
    ) %>%
    group_by(name) %>%
    mutate(
      z_ln_abundance = as.vector(scale(ln_abundance)),
      z_ln_incidence = as.vector(scale(ln_incidence)),
    ) %>%
    ungroup() %>%
    arrange(name, sample_year, seasonYear, trap_group)
}






## Funciones para colecciones de Variables Climáticas: ----

# ***********
# Colección 1 - Acumuladas con lag:
# ***********

calcular_clima_lag_inicial <- function(df_incidence, dfClimateDay1, n_semanas, 
                                       vars_suma = c("et", "atmx", "atmn", "ra"), 
                                       vars_prom = c("rh"),
                                       sufijo_personalizado = NULL) {
  
  # 1. NOMBRES
  if(is.null(sufijo_personalizado)) {
    sufijo <- paste0("_week", n_semanas, "_lag")
  } else {
    sufijo <- sufijo_personalizado
  }
  
  vars_suma_final <- paste0(vars_suma, sufijo)
  vars_prom_final <- paste0(vars_prom, sufijo)
  
  # Inicializamos en 0
  df_incidence[c(vars_suma_final, vars_prom_final)] <- 0
  
  # Días a mirar hacia atrás
  dias_retrovisor <- n_semanas * 7
  
  # 2. BUCLE MAESTRO
  for(i in 1:nrow(df_incidence)){
    
    # Validar fila
    if(is.na(df_incidence$fechas_inicio[i])) next
    
    # A. Extraemos y buscamos la FECHA MÍNIMA (El inicio real)
    # Se usa min() para asegurar que el rezago se cuente desde el primer día absoluto 
    # en que se instaló una trampa en esa temporada, ignorando las recolecciones tardías.
    texto_fechas <- strsplit(df_incidence$fechas_inicio[i], ", ")[[1]]
    fechas_obj   <- as.Date(texto_fechas, format = "%Y-%m-%d")
    fecha_arranque <- min(fechas_obj) 
    
    # B. Definimos la ventana temporal
    # Restamos días exactos para evitar cruces. Si arranca el día 10 y el lag es 1 semana (7 días),
    # la ventana es del día 3 al día 9 (fecha_fin = fecha_arranque - 1).
    fecha_ini <- fecha_arranque - days(dias_retrovisor)
    fecha_fin <- fecha_arranque - days(1)
    
    # C. Filtramos clima una sola vez (Usando dfClimateDay1)
    clima_ventana <- dfClimateDay1 %>% 
      filter(date >= fecha_ini, date <= fecha_fin)
    
    # D. Cálculos Directos
    
    # Sumas (ET, Temp, RA)
    if(length(vars_suma) > 0) {
      vals_suma <- colSums(clima_ventana[, vars_suma, drop=FALSE], na.rm = TRUE)
      df_incidence[i, vars_suma_final] <- as.list(vals_suma)
    }
    
    # Promedios (RH)
    if(length(vars_prom) > 0) {
      vals_prom <- colMeans(clima_ventana[, vars_prom, drop=FALSE], na.rm = TRUE)
      vals_prom[is.nan(vals_prom)] <- NA
      df_incidence[i, vars_prom_final] <- as.list(vals_prom)
    }
  }
  
  return(df_incidence)
}



# ***********
# Colección 2 - Promedios con lag:
# ***********

calcular_clima_lag_promedio <- function(df_incidence, dfClimateDay1, n_semanas, 
                                        # Por defecto metemos todas las que suelen promediarse
                                        vars_target = c("et", "atmx", "atmn", "ra", "rh"), 
                                        sufijo_personalizado = NULL) {
  
  # 1. NOMBRES DINÁMICOS
  if(is.null(sufijo_personalizado)) {
    sufijo <- paste0("_avg_week", n_semanas, "_lag")
  } else {
    sufijo <- sufijo_personalizado
  }
  
  # Nombres de las nuevas columnas
  vars_finales <- paste0(vars_target, sufijo)
  
  # Inicializamos en NA
  df_incidence[vars_finales] <- NA_real_
  
  # Días a mirar hacia atrás
  dias_retrovisor <- n_semanas * 7
  
  # 2. BUCLE MAESTRO
  for(i in 1:nrow(df_incidence)){
    
    # Validar fila
    if(is.na(df_incidence$fechas_inicio[i])) next
    
    # A. Fecha de Arranque (Mínima)
    texto_fechas <- strsplit(df_incidence$fechas_inicio[i], ", ")[[1]]
    fechas_obj   <- as.Date(texto_fechas, format = "%Y-%m-%d")
    fecha_arranque <- min(fechas_obj) 
    
    # B. Ventana Temporal
    fecha_ini <- fecha_arranque - days(dias_retrovisor)
    fecha_fin <- fecha_arranque - days(1)
    
    # C. Filtro (Usando dfClimateDay1)
    clima_ventana <- dfClimateDay1 %>% 
      filter(date >= fecha_ini, date <= fecha_fin)
    
    # D. Cálculo (PROMEDIO para todo)
    if(nrow(clima_ventana) > 0) {
      
      vals_promedio <- colMeans(clima_ventana[, vars_target, drop=FALSE], na.rm = TRUE)
      
      # Limpieza de NaNs
      vals_promedio[is.nan(vals_promedio)] <- NA
      
      # Asignación
      df_incidence[i, vars_finales] <- as.list(vals_promedio)
    }
  }
  
  return(df_incidence)
}



# ***********
# Colección 3 - Acumuladas durante toda la temporada de muestreo 
# (día 1 hasta día final):
# ***********

calcular_clima_durante_temporada <- function(df_incidence, dfClimateDay1, 
                                             # Mantenemos tu estándar: Sumar Temp/ET/Ra, Promediar RH
                                             vars_suma = c("et", "atmx", "atmn", "ra"), 
                                             vars_prom = c("rh"),
                                             sufijo = "_in_season") {
  
  # 1. PREPARACIÓN DE NOMBRES
  vars_suma_final <- paste0(vars_suma, sufijo)
  vars_prom_final <- paste0(vars_prom, sufijo)
  
  # Inicializamos
  df_incidence[c(vars_suma_final, vars_prom_final)] <- 0
  
  # 2. BUCLE MAESTRO
  for(i in 1:nrow(df_incidence)){
    
    # Validar fila
    if(is.na(df_incidence$fechas_inicio[i])) next
    
    # A. Análisis de Fechas de la Fila
    texto_fechas <- strsplit(df_incidence$fechas_inicio[i], ", ")[[1]]
    fechas_obj   <- as.Date(texto_fechas, format = "%Y-%m-%d")
    
    # B. Definición del RANGO TOTAL ("Desde el primero... hasta el último")
    fecha_inicio_absoluta <- min(fechas_obj)
    
    # El levantamiento es 7 días después de la colocación.
    # Si la colocación fue el día X, la trampa estuvo activa X... hasta X+6 (Total 7 días).
    fecha_fin_absoluta    <- max(fechas_obj) + days(6) 
    
    # C. Filtro Climático (Rango Continuo - usando dfClimateDay1)
    clima_ventana <- dfClimateDay1 %>% 
      filter(date >= fecha_inicio_absoluta, date <= fecha_fin_absoluta)
    
    # D. Cálculos
    
    # Sumas (Acumulación total en el periodo)
    if(length(vars_suma) > 0) {
      vals_suma <- colSums(clima_ventana[, vars_suma, drop=FALSE], na.rm = TRUE)
      df_incidence[i, vars_suma_final] <- as.list(vals_suma)
    }
    
    # Promedios (Estado medio en el periodo)
    if(length(vars_prom) > 0) {
      vals_prom <- colMeans(clima_ventana[, vars_prom, drop=FALSE], na.rm = TRUE)
      
      # Limpieza de NaNs
      vals_prom[is.nan(vals_prom)] <- NA
      df_incidence[i, vars_prom_final] <- as.list(vals_prom)
    }
  }
  
  return(df_incidence)
}


# ***********
# Colección 4 - Promedios durante toda la temporada de muestreo 
# (día 1 hasta día final):
# ***********

calcular_clima_in_season_promedio <- function(df_incidence, dfClimateDay1, 
                                              # Parámetros opcionales (ya configurados)
                                              vars_target = c("et", "atmx", "atmn", "ra", "rh"), 
                                              sufijo = "_in_season") {
  
  # 1. NOMBRES
  vars_finales <- paste0(vars_target, sufijo)
  
  # Inicializamos columnas en NA
  df_incidence[vars_finales] <- NA_real_
  
  # 2. BUCLE MAESTRO
  for(i in 1:nrow(df_incidence)){
    
    # A. Fechas
    texto_fechas <- strsplit(df_incidence$fechas_inicio[i], ", ")[[1]]
    fechas_obj   <- as.Date(texto_fechas, format = "%Y-%m-%d")
    
    # B. Rango de Temporada
    # Inicio: El primer día que se puso una trampa
    fecha_inicio_absoluta <- min(fechas_obj)
    # Fin: El último día que se levantó una trampa 
    fecha_fin_absoluta    <- max(fechas_obj) + days(6)
    
    # C. Filtro Climático
    # Usamos dfClimateDay1 directamente
    clima_ventana <- dfClimateDay1 %>% 
      filter(date >= fecha_inicio_absoluta, date <= fecha_fin_absoluta)
    
    # D. Cálculo: PROMEDIO (MEAN)
    if(nrow(clima_ventana) > 0) {
      
      vals_promedio <- colMeans(clima_ventana[, vars_target, drop=FALSE], na.rm = TRUE)
      
      # Limpieza de NaNs
      vals_promedio[is.nan(vals_promedio)] <- NA
      
      # Asignación
      df_incidence[i, vars_finales] <- as.list(vals_promedio)
    }
  }
  
  return(df_incidence)
}


# ***********
# Colección 5 - Promedios durante las semanas de muestreo 
# (versión discontinua de la anterior):
# ***********

calcular_clima_in_season_discontinuo <- function(df_incidence, dfClimateDay1, 
                                                 vars_target = c("et", "atmx", "atmn", "ra", "rh"), 
                                                 sufijo = "_in_season_avg") {
  
  # 1. PREPARAR COLUMNAS
  vars_finales <- paste0(vars_target, sufijo)
  df_incidence[vars_finales] <- NA_real_
  
  # 2. BUCLE
  for(i in 1:nrow(df_incidence)){
    
    # Check de seguridad por si no hay fechas
    if(is.na(df_incidence$fechas_inicio[i])) next
    
    # A. Leemos las fechas de inicio
    texto_fechas <- strsplit(df_incidence$fechas_inicio[i], ", ")[[1]]
    fechas_arranque <- as.Date(texto_fechas, format = "%Y-%m-%d")
    
    # B. GENERADOR DE DÍAS
    lista_dias <- list()
    
    for(k in 1:length(fechas_arranque)){
      fecha <- fechas_arranque[k]
      # Esto genera exactamente los 7 días de exposición por cada trampa puesta, 
      # saltándose los días donde las trampas estuvieron cerradas (discontinuidad).
      dias_semana <- fecha + 0:6 
      lista_dias[[k]] <- dias_semana
    }
    
    # Unimos todos los días en un solo vector y quitamos repetidos.
    todos_dias_validos <- unique(do.call("c", lista_dias))
    
    # C. FILTRO (Usamos %in% para pescar solo esos días exactos)
    clima_ventana <- dfClimateDay1 %>% 
      filter(date %in% todos_dias_validos)
    
    # D. PROMEDIO FINAL
    if(nrow(clima_ventana) > 0) {
      vals_promedio <- colMeans(clima_ventana[, vars_target, drop=FALSE], na.rm = TRUE)
      vals_promedio[is.nan(vals_promedio)] <- NA
      df_incidence[i, vars_finales] <- as.list(vals_promedio)
    }
  }
  
  return(df_incidence)
}


# ***********
# Colección 6 - Colección ONI / NINO 1 (Lags):
# ***********


calcular_indices_mensuales_lag <- function(df_incidence, nino, oni,
                                           n_meses_atras = 2, # Default: mes actual + 2 meses atrás
                                           sufijo_base = "_avg") {
  
  # 1. NOMBRES DE COLUMNAS
  sufijo_completo <- paste0(sufijo_base, "_", n_meses_atras, "m_lag")
  col_ninio <- paste0("ninio", sufijo_completo)
  col_oni   <- paste0("oni", sufijo_completo)
  
  # Inicializamos
  df_incidence[col_ninio] <- NA_real_
  df_incidence[col_oni]   <- NA_real_
  
  # 2. BUCLE MAESTRO
  for(i in 1:nrow(df_incidence)){
    
    # Check seguridad
    if(is.na(df_incidence$fechas_inicio[i])) next
    
    # A. FECHA DE ARRANQUE
    texto_fechas <- strsplit(df_incidence$fechas_inicio[i], ", ")[[1]]
    fechas_obj   <- as.Date(texto_fechas, format = "%Y-%m-%d")
    fecha_arranque <- min(fechas_obj)
    
    # B. IDENTIFICAR PERIODOS "YYYYMM"
    # Generamos la lista de los meses que nos interesan (Actual + pasados)
    periodos_target <- c()
    
    #for(k in 0:n_meses_atras) CAMBIAR AQUI SI DESEAMOS EXCLUIR EL MES DE RECOLECCION
    for(k in 1:n_meses_atras){
      fecha_lag <- fecha_arranque %m-% months(k)
      periodo_str <- format(fecha_lag, "%Y%m")
      periodos_target <- c(periodos_target, periodo_str)
    }
    
    # C. PROMEDIAR NIÑO
    vals_ninio <- nino %>%
      filter(periodo %in% periodos_target) %>%
      pull(ninio)
    
    if(length(vals_ninio) > 0) {
      df_incidence[i, col_ninio] <- mean(vals_ninio, na.rm = TRUE)
    }
    
    # D. PROMEDIAR ONI
    vals_oni <- oni %>%
      filter(periodo %in% periodos_target) %>%
      pull(oni)
    
    if(length(vals_oni) > 0) {
      df_incidence[i, col_oni] <- mean(vals_oni, na.rm = TRUE)
    }
  }
  
  return(df_incidence)
}


# ***********
# Colección 7 - Colección ONI / NINO 2 (Durante la Temporada):
# ***********

calcular_indices_mensuales_in_season <- function(df_incidence, nino, oni, 
                                                 sufijo = "_in_season_avg") {
  
  # 1. NOMBRES DE COLUMNAS
  col_ninio <- paste0("ninio", sufijo)
  col_oni   <- paste0("oni", sufijo)
  
  # Inicializamos
  df_incidence[col_ninio] <- NA_real_
  df_incidence[col_oni]   <- NA_real_
  
  # 2. BUCLE MAESTRO
  for(i in 1:nrow(df_incidence)){
    
    # Check seguridad
    if(is.na(df_incidence$fechas_inicio[i])) next
    
    # A. DEFINIR EL RANGO DE FECHAS EXACTO
    texto_fechas <- strsplit(df_incidence$fechas_inicio[i], ", ")[[1]]
    fechas_obj   <- as.Date(texto_fechas, format = "%Y-%m-%d")
    
    fecha_inicio_absoluta <- min(fechas_obj)
    fecha_fin_absoluta    <- max(fechas_obj) + 6 # Sumamos 6 días (semana completa)
    
    # B. GENERAR LISTA DE MESES INVOLUCRADOS
    mes_inicio <- floor_date(fecha_inicio_absoluta, "month")
    mes_fin    <- floor_date(fecha_fin_absoluta, "month")
    
    # Generamos la secuencia de meses (de 1 en 1) entre el inicio y el fin
    secuencia_meses <- seq(from = mes_inicio, to = mes_fin, by = "1 month")
    periodos_target <- format(secuencia_meses, "%Y%m")
    
    # C. PROMEDIAR NIÑO
    vals_ninio <- nino %>%
      filter(periodo %in% periodos_target) %>%
      pull(ninio)
    
    if(length(vals_ninio) > 0) {
      df_incidence[i, col_ninio] <- mean(vals_ninio, na.rm = TRUE)
    }
    
    # D. PROMEDIAR ONI
    vals_oni <- oni %>%
      filter(periodo %in% periodos_target) %>%
      pull(oni)
    
    if(length(vals_oni) > 0) {
      df_incidence[i, col_oni] <- mean(vals_oni, na.rm = TRUE)
    }
  }
  
  return(df_incidence)
}



# ***********
# Colección 8 - Acumuladas/Promedios (Ventana Continua) con Lag Semestral:
# ***********

# Toma la fecha exacta de inicio y fin de la temporada, la retrasa N semestres,
# y calcula la suma y el promedio de ese periodo en el pasado.

calcular_clima_semestre_lag_continuo <- function(df_incidence, dfClimateDay1, n_semestres, 
                                                 vars_suma = c("et", "atmx", "atmn", "ra"), 
                                                 vars_prom = c("rh"),
                                                 sufijo_personalizado = NULL) {
  
  # 1. NOMBRES (Ej: _sem1_lag o _sem2_lag)
  if(is.null(sufijo_personalizado)) {
    sufijo <- paste0("_sem", n_semestres, "_lag")
  } else {
    sufijo <- sufijo_personalizado
  }
  
  vars_suma_final <- paste0(vars_suma, sufijo)
  vars_prom_final <- paste0(vars_prom, sufijo)
  
  df_incidence[c(vars_suma_final, vars_prom_final)] <- 0
  
  # Factor de meses a restar (1 semestre = 6 meses)
  meses_retrovisor <- n_semestres * 6
  
  # 2. BUCLE MAESTRO
  for(i in 1:nrow(df_incidence)){
    if(is.na(df_incidence$fechas_inicio[i])) next
    
    texto_fechas <- strsplit(df_incidence$fechas_inicio[i], ", ")[[1]]
    fechas_obj   <- as.Date(texto_fechas, format = "%Y-%m-%d")
    
    # A. Fechas absolutas del muestreo original
    fecha_inicio_original <- min(fechas_obj)
    fecha_fin_original    <- max(fechas_obj) + days(6) 
    
    # B. VIAJE EN EL TIEMPO (Lag semestral exacto)
    fecha_inicio_lag <- fecha_inicio_original %m-% months(meses_retrovisor)
    fecha_fin_lag    <- fecha_fin_original %m-% months(meses_retrovisor)
    
    # C. Filtro Climático en el pasado
    clima_ventana <- dfClimateDay1 %>% 
      filter(date >= fecha_inicio_lag, date <= fecha_fin_lag)
    
    # D. Cálculos
    if(length(vars_suma) > 0) {
      vals_suma <- colSums(clima_ventana[, vars_suma, drop=FALSE], na.rm = TRUE)
      df_incidence[i, vars_suma_final] <- as.list(vals_suma)
    }
    if(length(vars_prom) > 0) {
      vals_prom <- colMeans(clima_ventana[, vars_prom, drop=FALSE], na.rm = TRUE)
      vals_prom[is.nan(vals_prom)] <- NA
      df_incidence[i, vars_prom_final] <- as.list(vals_prom)
    }
  }
  return(df_incidence)
}


# ***********
# Colección 9 - Promedios Discontinuos (Solo semanas exactas) con Lag Semestral:
# ***********
# Mismo principio de la Colección 5, pero agarra las fechas precisas de muestreo
# y busca cómo estuvo el clima exactamente en esos mismos días del año/semestre 
# anterior.

calcular_clima_semestre_lag_discontinuo <- function(df_incidence, dfClimateDay1, n_semestres, 
                                                    vars_target = c("et", "atmx", "atmn", "ra", "rh"), 
                                                    sufijo_personalizado = NULL) {
  
  if(is.null(sufijo_personalizado)) {
    sufijo <- paste0("_avg_sem", n_semestres, "_lag_discontinuo")
  } else {
    sufijo <- sufijo_personalizado
  }
  
  vars_finales <- paste0(vars_target, sufijo)
  df_incidence[vars_finales] <- NA_real_
  meses_retrovisor <- n_semestres * 6
  
  for(i in 1:nrow(df_incidence)){
    if(is.na(df_incidence$fechas_inicio[i])) next
    
    texto_fechas <- strsplit(df_incidence$fechas_inicio[i], ", ")[[1]]
    fechas_arranque <- as.Date(texto_fechas, format = "%Y-%m-%d")
    
    lista_dias <- list()
    for(k in 1:length(fechas_arranque)){
      fecha_lag <- fechas_arranque[k] %m-% months(meses_retrovisor)
      
      # Generamos los 7 días a partir de esa fecha en el pasado
      dias_semana <- fecha_lag + 0:6 
      lista_dias[[k]] <- dias_semana
    }
    
    todos_dias_validos <- unique(do.call("c", lista_dias))
    
    clima_ventana <- dfClimateDay1 %>% 
      filter(date %in% todos_dias_validos)
    
    if(nrow(clima_ventana) > 0) {
      vals_promedio <- colMeans(clima_ventana[, vars_target, drop=FALSE], na.rm = TRUE)
      vals_promedio[is.nan(vals_promedio)] <- NA
      df_incidence[i, vars_finales] <- as.list(vals_promedio)
    }
  }
  return(df_incidence)
}


# ***********
# Colección 10 - ONI / NINO durante la ventana con Lag Semestral:
# ***********
# Calcula el estado macroclimático promedio durante las semanas de muestreo, 
# pero desplazado 1 o 2 semestres al pasado.

calcular_indices_in_season_semestre_lag <- function(df_incidence, nino, oni, n_semestres, 
                                                    sufijo_personalizado = NULL) {
  
  if(is.null(sufijo_personalizado)) {
    sufijo <- paste0("_in_season_sem", n_semestres, "_lag")
  } else {
    sufijo <- sufijo_personalizado
  }
  
  col_ninio <- paste0("ninio", sufijo)
  col_oni   <- paste0("oni", sufijo)
  
  df_incidence[col_ninio] <- NA_real_
  df_incidence[col_oni]   <- NA_real_
  meses_retrovisor <- n_semestres * 6
  
  for(i in 1:nrow(df_incidence)){
    if(is.na(df_incidence$fechas_inicio[i])) next
    
    texto_fechas <- strsplit(df_incidence$fechas_inicio[i], ", ")[[1]]
    fechas_obj   <- as.Date(texto_fechas, format = "%Y-%m-%d")
    
    # Rango original
    fecha_inicio_original <- min(fechas_obj)
    fecha_fin_original    <- max(fechas_obj) + 6 
    
    # Viaje en el tiempo
    fecha_inicio_lag <- fecha_inicio_original %m-% months(meses_retrovisor)
    fecha_fin_lag    <- fecha_fin_original %m-% months(meses_retrovisor)
    
    mes_inicio <- floor_date(fecha_inicio_lag, "month")
    mes_fin    <- floor_date(fecha_fin_lag, "month")
    
    secuencia_meses <- seq(from = mes_inicio, to = mes_fin, by = "1 month")
    periodos_target <- format(secuencia_meses, "%Y%m")
    
    vals_ninio <- nino %>% filter(periodo %in% periodos_target) %>% pull(ninio)
    if(length(vals_ninio) > 0) df_incidence[i, col_ninio] <- mean(vals_ninio, na.rm = TRUE)
    
    vals_oni <- oni %>% filter(periodo %in% periodos_target) %>% pull(oni)
    if(length(vals_oni) > 0) df_incidence[i, col_oni] <- mean(vals_oni, na.rm = TRUE)
  }
  return(df_incidence)
}




# ************************************************************
# SECCIÓN 2: ANALISIS EXPLORATORIO (01) ----
# ************************************************************






# ************************************************************
# SECCIÓN 3: ANÁLISIS DE ESTACIONARIEDAD ----
# ************************************************************




analizar_estacionariedad_biotica_d <- function(df, variable_respuesta, d = 0) {
  library(tseries)
  library(dplyr)
  
  get_stars <- function(p) {
    if (is.na(p)) return("")
    if (p <= 0.001) return("***")
    if (p <= 0.01)  return("**")
    if (p <= 0.05)  return("*")
    if (p <= 0.1)   return(".")
    return("")
  }
  
  especies <- unique(df$name)
  
  tabla_resultados <- lapply(especies, function(esp) {
    serie <- df %>% filter(name == esp) %>% pull(!!sym(variable_respuesta)) %>% na.omit()
    
    if (d > 0 && length(serie) > d) {
      serie <- diff(serie, differences = d)
    }
    
    if(length(serie) < 10 || sd(serie) == 0) {
      return(data.frame(Especie = esp, DF_k0 = "Error", ADF_p_val = "Error", KPSS_p_val = "Error", Veredicto = "ERROR"))
    }
    
    # --- CÁLCULO DE K (Heurística de Schwert) ---
    # k_schwert <- floor(12 * (length(serie)/100)^0.25)
    
    # --- TEST DF SIMPLE (k = 0) ---
    test_df_k0 <- suppressWarnings(tryCatch(adf.test(serie, k = 0), error = function(e) list(p.value = NA)))
    p_df0 <- test_df_k0$p.value
    df0_label <- if(is.na(p_df0)) "NA" else sprintf("%.3f %s", p_df0, get_stars(p_df0))
    
    # --- TEST ADF (k estándar de tseries) ---
    # Para usar Schwert, cambia a: adf.test(serie, k = k_schwert)
    test_adf <- suppressWarnings(tryCatch(adf.test(serie), error = function(e) list(p.value = NA)))
    p_adf <- test_adf$p.value
    adf_label <- if(is.na(p_adf)) "NA" else sprintf("%.3f %s", p_adf, get_stars(p_adf))
    if(!is.na(p_adf) && p_adf <= 0.01) adf_label <- paste("<0.01", get_stars(p_adf))
    
    # --- TEST KPSS (H0: Estacionaria) ---
    test_kpss <- suppressWarnings(tryCatch(kpss.test(serie, null = "Level"), error = function(e) list(p.value = NA)))
    p_kpss <- test_kpss$p.value
    kpss_label <- if(is.na(p_kpss)) "NA" else sprintf("%.3f %s", p_kpss, get_stars(p_kpss))
    if(!is.na(p_kpss) && p_kpss >= 0.1) kpss_label <- paste(">0.10", get_stars(p_kpss))
    
    # Veredicto basado en ADF (k auto) y KPSS
    exito_adf <- !is.na(p_adf) && p_adf < 0.10
    exito_kpss <- !is.na(p_kpss) && p_kpss > 0.05
    
    
    
    # 
    # veredicto <- case_when(
    #   exito_adf & exito_kpss ~ "ESTRIC. ESTACIONARIO",
    #   exito_adf | exito_kpss ~ "ESTACIONARIO",
    #   TRUE                   ~ "NO ESTACIONARIO"
    # )
    # 
    # 
    
    
    # Veredicto basado en la combinación real de los tests
    veredicto <- case_when(
      exito_adf & exito_kpss  ~ "ESTACIONARIO OK",
      !exito_adf & !exito_kpss ~ "NO ESTACIONARIO",
      exito_adf & !exito_kpss  ~ "CONFLICTO (Tendencia Estacionaria?)",
      !exito_adf & exito_kpss  ~ "CONFLICTO (Posible Estacionario, ADF débil)"
    )
    
    
    
    data.frame(Especie = esp, DF_k0 = df0_label, ADF_p_val = adf_label, 
               KPSS_p_val = kpss_label, Veredicto = veredicto, stringsAsFactors = FALSE)
  }) %>% bind_rows()
  
  return(tabla_resultados)
}


analizar_estacionariedad_clima_d <- function(df_clima, d = 0) {
  library(tseries)
  library(dplyr)
  
  get_stars <- function(p) {
    if (is.na(p)) return("")
    if (p <= 0.001) return("***")
    if (p <= 0.01)  return("**")
    if (p <= 0.05)  return("*")
    if (p <= 0.1)   return(".")
    return("")
  }
  
  vars_to_test <- df_clima %>%
    dplyr::select(where(is.numeric), -any_of(c("sample_year", "dummy_dry", "dummy_wet"))) %>%
    colnames()
  
  tabla_resultados <- lapply(vars_to_test, function(var) {
    serie <- na.omit(df_clima[[var]])
    
    if (d > 0 && length(serie) > d) {
      serie <- diff(serie, differences = d)
    }
    
    if(length(serie) < 10 || sd(serie) == 0) {
      return(data.frame(Variable = var, ADF_Num = NA, DF_k0 = "Error", 
                        ADF_p_val = "Error", KPSS_p_val = "Error", Veredicto = "ERROR"))
    }
    
    # --- CÁLCULO DE K (Heurística de Schwert) ---
    # k_schwert <- floor(12 * (length(serie)/100)^0.25)
    
    # Test DF Simple (k=0)
    p_df0 <- suppressWarnings(tryCatch(adf.test(serie, k = 0)$p.value, error = function(e) NA))
    df0_label <- if(is.na(p_df0)) "NA" else sprintf("%.3f %s", p_df0, get_stars(p_df0))
    
    # Test ADF (k estándar de tseries)
    # Para usar Schwert, cambia a: adf.test(serie, k = k_schwert)
    p_adf <- suppressWarnings(tryCatch(adf.test(serie)$p.value, error = function(e) NA))
    adf_label <- if(is.na(p_adf)) "NA" else sprintf("%.3f %s", p_adf, get_stars(p_adf))
    
    # Test KPSS
    p_kpss <- suppressWarnings(tryCatch(kpss.test(serie, null = "Level")$p.value, error = function(e) NA))
    kpss_label <- if(is.na(p_kpss)) "NA" else sprintf("%.3f %s", p_kpss, get_stars(p_kpss))
    
    exito_adf <- !is.na(p_adf) && p_adf < 0.10      # Cambiar por 0.05 si deseamos 95%
    exito_kpss <- !is.na(p_kpss) && p_kpss >= 0.10
    
    # 
    # veredicto <- case_when(
    #   exito_adf & exito_kpss ~ "ESTRIC. ESTACIONARIO",
    #   exito_adf | exito_kpss ~ "ESTACIONARIO",
    #   TRUE                   ~ "NO ESTACIONARIO"
    # )
    # 
    # 
    
    # Veredicto basado en la combinación real de los tests
    veredicto <- case_when(
      exito_adf & exito_kpss  ~ "ESTACIONARIO OK",
      !exito_adf & !exito_kpss ~ "NO ESTACIONARIO",
      exito_adf & !exito_kpss  ~ "CONFLICTO (Tendencia Estacionaria?)",
      !exito_adf & exito_kpss  ~ "CONFLICTO (Posible Estacionario, ADF débil)"
    )
    
    
    
    data.frame(Variable = var, ADF_Num = p_adf, DF_k0 = df0_label, 
               ADF_p_val = adf_label, KPSS_p_val = kpss_label, Veredicto = veredicto)
  }) %>% bind_rows()
  
  return(tabla_resultados %>% arrange(ADF_Num) %>% dplyr::select(-ADF_Num))
}


# 3. Función de Simulación de Potencia de Monte Carlo
simular_potencia_estacionariedad <- function(df, variable_respuesta, S = 1000) {
  library(tseries)
  library(dplyr)
  library(stats)
  
  especies <- unique(df$name)
  
  tabla_potencia <- lapply(especies, function(esp) {
    # 1. Extraer la serie temporal empírica real (T = 31)
    serie_real <- df %>% filter(name == esp) %>% pull(!!sym(variable_respuesta)) %>% na.omit()
    T_obs <- length(serie_real)
    cat("Calculando Monte Carlo para:", esp, "...\n")
    
    # Si hay muy pocos datos, saltar
    if (T_obs < 15 || sd(serie_real) == 0) {
      return(data.frame(Especie = esp, Rho_AR1 = NA, Potencia_ADF = NA, Potencia_KPSS = NA))
    }
    
    # 2. Estimar el Proceso Generador de Datos (DGP) empírico
    # Ajustamos un modelo AR(1) para capturar la persistencia real de la hormiga
    ar_fit <- tryCatch(ar(serie_real, aic = FALSE, order.max = 1, method = "mle"), 
                       error = function(e) NULL)
    
    if(is.null(ar_fit)) return(data.frame(Especie = esp, Rho_AR1 = NA, Potencia_ADF = NA, Potencia_KPSS = NA))
    
    rho_empirico <- ar_fit$ar[1]
    sigma_empirico <- sqrt(ar_fit$var.pred)
    media_empirica <- mean(serie_real)
    
    # Contadores para Monte Carlo
    rechazos_adf <- 0
    rechazos_kpss <- 0
    
    # 3. Bucle de Simulación de Monte Carlo
    for (i in 1:S) {
      # Generar serie estacionaria sintética (universo paralelo)
      serie_sim <- arima.sim(model = list(ar = rho_empirico), n = T_obs, sd = sigma_empirico) + media_empirica
      
      # Test ADF (H0: No estacionaria. Buscamos p < 0.05 para rechazar H0 correctamente)
      p_adf <- suppressWarnings(tryCatch(adf.test(serie_sim)$p.value, error = function(e) 1))
      if (p_adf < 0.05) rechazos_adf <- rechazos_adf + 1
      
      # Test KPSS (H0: Estacionaria. Como la serie ES estacionaria, buscamos p > 0.05 para NO rechazar correctamente)
      p_kpss <- suppressWarnings(tryCatch(kpss.test(serie_sim, null = "Level")$p.value, error = function(e) 0))
      if (p_kpss > 0.05) rechazos_kpss <- rechazos_kpss + 1
    }
    
    # 4. Calcular la Potencia Empírica (%)
    potencia_adf_pct <- (rechazos_adf / S) * 100
    potencia_kpss_pct <- (rechazos_kpss / S) * 100
    
    data.frame(
      Especie = esp,
      Rho_AR1 = round(rho_empirico, 3),
      Potencia_ADF_pct = round(potencia_adf_pct, 1),
      Potencia_KPSS_pct = round(potencia_kpss_pct, 1)
    )
  }) %>% bind_rows()
  
  return(tabla_potencia)
}








