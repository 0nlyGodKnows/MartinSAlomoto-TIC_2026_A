# ************************************************************
# SCRIPT DE LIMPIEZA Y TRATAMIENTO DE DATOS
# ************************************************************


# ************************************************************
# 0. CONFIGURACIÓN DEL ENTORNO ----
# ************************************************************

source(here::here("2. Librerias - Funciones", "Librerias.R"))





# ************************************************************
# 1. LIMPIEZA Y TRATAMIENTO DE LOS DATOS -----
# ************************************************************

df <- read_excel(here::here("1. Data", "Original", "originalDatabaseAnts.xlsx"))

df$date_initiated <- as.Date(df$date_initiated, format = "%d/%m/%Y")
df$date_surveyed  <- as.Date(df$date_surveyed,  format = "%d/%m/%Y")
df$sample_year    <- as.numeric(df$sample_year)


# Corrección de semanas asignadas erroneamente:
df <- df %>% mutate(
  group = case_when(
    date_initiated == make_date(2012, 05, 23) ~ "MALAISEWEEK21",
    date_initiated == make_date(2012, 10, 03) ~ "MALAISEWEEK40",
    TRUE ~ group
  )
)

df <- df %>% mutate(date_surveyed = coalesce(date_surveyed, date_initiated + 7))
df$group = as.factor(df$group)

# Limpieza de nomenclaturas romanas
df <- df %>%
  mutate(
    date_label_clean = date_label %>%
      str_replace_all("\\.i\\.", ".1.") %>% str_replace_all("\\.ii\\.", ".2.") %>%
      str_replace_all("\\.iii\\.", ".3.") %>% str_replace_all("\\.iv\\.", ".4.") %>%
      str_replace_all("\\.v\\.", ".5.") %>% str_replace_all("\\.vi\\.", ".6.") %>%
      str_replace_all("\\.vii\\.", ".7.") %>% str_replace_all("\\.viii\\.", ".8.") %>%
      str_replace_all("\\.ix\\.", ".9.") %>% str_replace_all("\\.x\\.", ".10.") %>%
      str_replace_all("\\.xi\\.", ".11.") %>% str_replace_all("\\.xii\\.", ".12."),
    date_label = dmy(date_label_clean)
  ) %>% dplyr::select(-date_label_clean)


# Corrección manual de fechas mal ingresadas
df$date_surveyed[df$sample_year == 2013 & df$group == "MALAISEWEEK14"] <- as.Date("03/04/2013", format="%d/%m/%Y")
df$date_surveyed[df$sample_year == 2014 & df$group == "MALAISEWEEK52"] <- as.Date("31/12/2014", format="%d/%m/%Y")
df <- df %>%
  mutate(
    comments = case_when(
      sample_year == 2006 & group == "MALAISEWEEK13" ~ "MM-2006-Dry",
      sample_year == 2006 & group == "MALAISEWEEK03" ~ NA_character_,
      TRUE ~ comments
    )
  )

df <- df %>% distinct(ID, .keep_all = TRUE)
df_clean <- df




# ************************************************************
# 2. CREACIÓN DEL GRID CLIMÁTICO DIARIO ------
# ************************************************************

mapeo_clima <- list(
  et   = c("bci_cl1_et_man.csv",  "et"),
  rh   = c("bci_cl_rh_man.csv",   "rh"),
  atmx = c("bci_cl_atmx_man.csv", "atmx"),
  atmn = c("bci_cl_atmn_man.csv", "atmn"),
  ra   = c("bci_cl_ra_man.csv",   "ra")
)

# Selección de Fechas:
fechas_grid <- data.frame(date = seq.Date(as.Date("2000-01-01"),
                                          as.Date("2019-12-31"), by = "day"))
dfClimateDay <- fechas_grid

# Consolidación del Grid Climático Diario:
for(var_final in names(mapeo_clima)) {
  config <- mapeo_clima[[var_final]]
  archivo <- config[1]
  col_original <- config[2]
  
  tmp <- read.csv(here::here("1. Data", "Original", archivo))
  tmp$date <- as.Date(tmp$date, format = "%d/%m/%Y")
  
  tmp <- tmp %>%
    # Filtro sincronizado con la nueva fecha de término (2019)
    filter(date >= as.Date("2000-01-01") & date <= as.Date("2019-12-31")) %>%
    mutate(
      val = !!sym(col_original),
      val = ifelse(chk_note == 'missing', NA, val),
      val = ifelse(val == -999, NA, val)
    )
  
  if(str_detect(var_final, "rh")) {
    tmp <- tmp %>% mutate(val = ifelse(val == 0, NA, val))
  }
  
  tmp <- tmp %>%
    group_by(date) %>%
    summarise(!!sym(var_final) := mean(val, na.rm = TRUE), .groups = 'drop')
  
  tmp[[var_final]][is.nan(tmp[[var_final]])] <- NA
  dfClimateDay <- dfClimateDay %>% left_join(tmp, by = "date")
}


# Realizamos la interpolación de los datos:
dfClimateDay_interp <- dfClimateDay %>%
  mutate(across(-date, ~round(na_interpolation(ts(., start = c(2000,1,1),
                                                  frequency = 365), 
                                               option = "stine"), 3)))

dfClimateDay1 <- dfClimateDay_interp %>% dplyr::select(date, et, rh, 
                                                       atmx, atmn, ra)



# ************************************************************
# 3. CONFIGURACIÓN DE ÍNDICES CLIMÁTICOS ------
# ************************************************************

months <- c("year",
            "01", "02", "03",
            "04", "05", "06", 
            "07", "08", '09', 
            '10', '11', '12')

# preparar_indice <- function(file_name, col_name) {
#   idx <- read.table(here::here("1. Data", "Original", file_name), 
#                     header = FALSE, sep = "", 
#                     stringsAsFactors = FALSE)
#   colnames(idx) <- months
#   idx %>%
#     pivot_longer(cols = '01':'12', names_to = "month", values_to = col_name) %>%
#     mutate(periodo = paste0(year, month),
#            date_obj = as.Date(paste0(periodo, "01"), format = "%Y%m%d")) %>%
#     filter(year >= 2000) %>%
#     dplyr::select(-year, -month)
# }

nino <- preparar_indice("ninioModified.txt", "ninio")
oni  <- preparar_indice("oniModified.txt", "oni")





# ************************************************************
# 4. PRE-PROCESAMIENTO, ZERO-FILLING Y MATRIZ BIOLÓGICA TRAMPA --------
# ************************************************************

# Selección de las semanas para el análisis, nos interesa la selección
# "por defecto" identificada por el STRI:
por_defecto = TRUE
# semanas_dry = 1:36
# semanas_wet = 37:54


# A. Clasificación de Semestres
df_proc <- df_clean %>%
  mutate(
    week_num = as.numeric(str_extract(group, "\\d+")),
    year = sample_year,
    date_initiated = as.Date(date_initiated),
    seasonYear = if (por_defecto) {
      comments
    } else {
      case_when(
        week_num %in% semanas_dry ~ paste0("MM-", sample_year, "-Dry"),
        week_num %in% semanas_wet ~ paste0("MM-", sample_year, "-Wet"),
        TRUE ~ NA_character_
      )
    }
  ) %>% filter(!is.na(seasonYear), sex == 'M - male')

# B. Filtrado de especies raras.
trapSpecieUnique <- df_proc %>% distinct(name, trap) %>% count(name) %>%
  filter(n > 2) %>% pull(name)


# Agrupamos SOLO por año y época para obtener las fechas oficiales 
# (las 2 semanas) de toda la isla.
master_schedule <- df_proc %>%
  group_by(sample_year, seasonYear) %>%
  summarise(
    semanas_presencia = toString(sort(unique(week_num))),
    fechas_presencia  = toString(sort(unique(format(date_initiated, "%d-%b")))),
    fechas_inicio     = toString(sort(unique(format(date_initiated, "%Y-%m-%d")))),
    .groups = 'drop'
  )


# Asignamos este calendario maestro a TODAS las 10 trampas físicas para que
# no falte ninguna fecha.
df_trap_schedule <- expand_grid(
  sample_year = unique(master_schedule$sample_year),
  seasonYear = unique(master_schedule$seasonYear),
  trap = as.character(311:320)
) %>%
  inner_join(master_schedule %>% dplyr::select(sample_year, seasonYear), 
             by = c("sample_year", "seasonYear")) %>%
  left_join(master_schedule, by = c("sample_year", "seasonYear"))

# BASE BIOLÓGICA OBSERVADA: Definición Incidencia y Abundancia
df_obs <- df_proc %>%
  filter(name %in% trapSpecieUnique) %>%
  mutate(trap = as.character(trap)) %>% 
  group_by(name, sample_year, seasonYear, trap) %>%
  summarise(
    hormigas_abundance = sum(abundance),
    hormigas_incidence = ifelse(sum(abundance) > 0, 1, 0),
    .groups = 'drop'
  )

# Creamos todas las combinaciones posibles de Especie-Año-Temporada-Trampa
df_grid <- expand_grid(
  name = trapSpecieUnique,
  sample_year = unique(df_trap_schedule$sample_year),
  seasonYear = unique(df_trap_schedule$seasonYear),
  trap = unique(df_trap_schedule$trap)
) %>%
  inner_join(df_trap_schedule %>% 
               dplyr::select(sample_year, seasonYear) %>%
               distinct(), 
             by = c("sample_year", "seasonYear"))


# Unimos la grilla, las observaciones y el calendario estandarizado de la trampa.
df_zero_filled <- df_grid %>%
  left_join(df_obs, by = c("name", "sample_year", "seasonYear", "trap")) %>%
  mutate(
    hormigas_abundance = replace_na(hormigas_abundance, 0),
    hormigas_incidence = replace_na(hormigas_incidence, 0),
    dry = as.numeric(str_detect(seasonYear, "Dry"))
  ) %>%
  left_join(df_trap_schedule, by = c("sample_year", "seasonYear", "trap"))







# ************************************************************
# 5. INYECCIÓN CLIMÁTICA VECTORIZADA SOBRE EL SCHEDULE -----
# ************************************************************

df_trabajo  <- df_trap_schedule %>% dplyr::select(-trap) %>% distinct()
df_clima    <- dfClimateDay1 
df_nino_raw <- nino                
df_oni_raw  <- oni                 

vars_suma_custom  <- c("et", "atmx", "atmn", "ra")
vars_prom_custom  <- c("rh")
vars_todas_custom <- c(vars_suma_custom, vars_prom_custom)





df_2017 <- df_trabajo %>% filter(sample_year == 2017)
df_2018 <- df_2017 %>%
  mutate(
    sample_year = 2018,
    seasonYear = str_replace(seasonYear, "2017", "2018"),
    fechas_inicio = sapply(strsplit(fechas_inicio, ", "), function(x) {
      paste(as.Date(x) + years(1), collapse = ", ")
    }),
    fechas_presencia = sapply(strsplit(fechas_inicio, ", "), function(x) {
      paste(format(as.Date(x), "%d-%b"), collapse = ", ")
    })
  )

df_2019 <- df_2017 %>%
  mutate(
    sample_year = 2019,
    seasonYear = str_replace(seasonYear, "2017", "2019"),
    fechas_inicio = sapply(strsplit(fechas_inicio, ", "), function(x) {
      paste(as.Date(x) + years(2), collapse = ", ")
    }),
    fechas_presencia = sapply(strsplit(fechas_inicio, ", "), function(x) {
      paste(format(as.Date(x), "%d-%b"), collapse = ", ")
    })
  )
df_trabajo <- bind_rows(df_trabajo, df_2018, df_2019)





View(df_trabajo)



# BLOQUE 1:
for(k in 1:4){
  df_trabajo <- calcular_clima_lag_inicial(df_trabajo, df_clima, n_semanas = k, 
                                           vars_suma = vars_suma_custom, vars_prom = vars_prom_custom)
  df_trabajo <- calcular_clima_lag_promedio(df_trabajo, df_clima, n_semanas = k, 
                                            vars_target = vars_todas_custom)
}


# BLOQUE 2:
df_trabajo <- calcular_clima_durante_temporada(df_trabajo, df_clima, 
                                               vars_suma = vars_suma_custom, vars_prom = vars_prom_custom, sufijo = "_in_season_cont")
df_trabajo <- calcular_clima_in_season_promedio(df_trabajo, df_clima, 
                                                vars_target = vars_todas_custom, sufijo = "_in_season_avg_cont")
df_trabajo <- calcular_clima_in_season_discontinuo(df_trabajo, df_clima, 
                                                   vars_target = vars_todas_custom, sufijo = "_in_season_avg_disc")

df_trabajo <- calcular_indices_mensuales_in_season(df_trabajo, df_nino_raw, df_oni_raw)


# BLOQUE 3:
for(k in 1:4){
  df_trabajo <- calcular_clima_semestre_lag_continuo(df_trabajo, df_clima, n_semestres = k,
                                                     vars_suma = vars_suma_custom, vars_prom = vars_prom_custom)
  df_trabajo <- calcular_clima_semestre_lag_continuo(df_trabajo, df_clima, n_semestres = k,
                                                     vars_suma = NULL, vars_prom = vars_suma_custom,
                                                     sufijo_personalizado = paste0("_sem", k, "_lag_avg"))
  df_trabajo <- calcular_clima_semestre_lag_discontinuo(df_trabajo, df_clima, n_semestres = k,
                                                        vars_target = vars_todas_custom)
  df_trabajo <- calcular_indices_in_season_semestre_lag(df_trabajo, df_nino_raw, df_oni_raw, n_semestres = k)
}

df_trabajo <- df_trabajo %>% 
  dplyr::select(-"_sem1_lag_avg",-"_sem2_lag_avg",-"_sem3_lag_avg",-"_sem4_lag_avg")

dim(df_trabajo)
ncol(df_trabajo)
colnames(df_trabajo)
# EN TOTAL, SE HAN GENERADO 121 VARIABLES CLIMATICAS (SIN DUMMIES)







# ************************************************************
# 6. CONDENSACIÓN DE LA INFORMACIÓN BIÓTICA -----
# ************************************************************


df <- copy(df_zero_filled)

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


grupos_H0 <- data.frame(
  trap = as.character(311:320), 
  trap_group = "Isla_Completa"
)

df <-  condensar_trampas(df, grupos_H0)





# ************************************************************
# 7. TRANSFORMACIÓN DE LAS VARIABLES CLIMÁTICAS -----
# ************************************************************


df_clima <- copy(df_trabajo)

# 1. Identificar TODAS las variables numéricas en el dataset
vars_numericas <- df_clima %>% 
  dplyr::select(where(is.numeric)) %>% 
  colnames()

# 2. Excluir variables ENSO y demas, de la lista de procesamiento
vars_a_procesar <- vars_numericas[!str_detect(vars_numericas, "(?i)oni|ninio|sample_year")]

# 3. Función auxiliar para detectar si una columna tiene al menos un cero
tiene_ceros <- function(x) any(x == 0, na.rm = TRUE)

# 4. Dividir las variables a procesar en Grupo A (con ceros) y Grupo B (sin ceros)
vars_con_ceros <- df_clima %>% 
  dplyr::select(all_of(vars_a_procesar)) %>% 
  dplyr::select(where(tiene_ceros)) %>% 
  colnames()

vars_sin_ceros <- setdiff(vars_a_procesar, vars_con_ceros)

cat("\n--- Resumen de Transformaciones ---\n")
cat("Variables (ENSO) preservadas a escala original:", length(vars_numericas) - length(vars_a_procesar) - 1, "\n")
cat("Variables transformadas con ln(x+1):", length(vars_con_ceros), "\n")
cat("Variables transformadas con ln(x):", length(vars_sin_ceros), "\n")

# 5. EJECUTAR LAS TRANSFORMACIONES
df_clima <- df_clima %>%
  dplyr::mutate(
    # Aplicar ln(x+1) al Grupo A
    across(
      .cols = all_of(vars_con_ceros),
      .fns = ~log(.x + 1),
      .names = "ln_{.col}"
    ),
    # Aplicar ln(x) al Grupo B
    across(
      .cols = all_of(vars_sin_ceros),
      .fns = ~log(.x),
      .names = "ln_{.col}"
    )
  ) %>%
  # 6. Purgar únicamente las columnas climáticas locales originales
  # Como 'sample_year' no está en 'vars_a_procesar', se conserva intacto
  dplyr::select(-all_of(vars_a_procesar))

# 7. GENERACIÓN DE VARIABLES DUMMY ESTACIONALES
df_clima <- df_clima %>%
  dplyr::mutate(
    dummy_dry = as.integer(grepl("Dry", seasonYear)),
    dummy_wet = as.integer(grepl("Wet", seasonYear))
  )


# Variable auxiliar para los modelos posteriores
var_dependiente <- "ln_incidence"

colnames(df_clima)




# ************************************************************
# 8. ALMACENAMIENTO -----
# ************************************************************


# 
# saveRDS(df_clean,
#         here::here("1. Data", "Procesada", "df_clean.RDS"))


# 
# saveRDS(oni,
#         here::here("1. Data", "Procesada", "nino.RDS"))
# 

# 
# saveRDS(nino,
#         here::here("1. Data", "Procesada", "oni.RDS"))
# 


# 
# saveRDS(dfClimateDay,
#         here::here("1. Data", "Procesada", "dfClimateDay_pre_interp.RDS"))

# 
# saveRDS(dfClimateDay1,
#         here::here("1. Data", "Procesada", "dfClimateDay_interp.RDS"))
# 


# saveRDS(df_zero_filled,
#         here::here("1. Data", "Procesada", "df_zero_filled.RDS"))
# 

# 
# saveRDS(df_trabajo,
#         here::here("1. Data", "Procesada", "df_clima.RDS"))



# 
# saveRDS(df,
#         here::here("1. Data", "Procesada", "df_final.RDS"))
# 

# 
# saveRDS(df_clima,
#         here::here("1. Data", "Procesada", "df_clima_final.RDS"))
# 


