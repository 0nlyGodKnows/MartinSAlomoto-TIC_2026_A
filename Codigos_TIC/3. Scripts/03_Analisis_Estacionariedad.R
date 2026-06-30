# ************************************************************
# ANALISIS DE ESTACIONARIEDAD
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
df <- readRDS(here::here("1. Data", "Procesada", "df_final.RDS"))
df_clima <- readRDS(here::here("1. Data", "Procesada", "df_clima_final.RDS"))



# ************************************************************
# 1. ESTACIONARIEDAD ----
# ************************************************************


# Realizar el filtrado de datos:
df <- df %>%
  filter(name %in% nombres_especies_clave)

df_clima <- df_clima %>%dplyr::filter(sample_year <= 2017)

var_dependiente <- "ln_incidence" 


tabla_d0 <- analizar_estacionariedad_biotica_d(df, var_dependiente, d = 0)
print(tabla_d0)



especies_ganadoras_estacionariedad <- tabla_d0 %>% 
  filter(Veredicto %in% c("CONFLICTO (Posible Estacionario, ADF débil)","ESTACIONARIO OK")) %>% 
  pull(Especie)

cat(sprintf("\n[!] Supervivencia Biótica: %d especies de %d\n", 
            length(especies_ganadoras_estacionariedad), nrow(tabla_d0)))
print(especies_ganadoras_estacionariedad)



## Prueba de Potencia: Estacionariedad Series Bióticas -------

# Advertencia: Este proceso puede demorar unos minutos.

# set.seed(201821199)
# # Ejecución:
# tabla_potencia_biotica <- simular_potencia_estacionariedad(df, var_dependiente, S = 100)
# print(tabla_potencia_biotica)
# 
# 
# saveRDS(tabla_potencia_biotica,
#         here::here("1. Data", "Procesada", "tabla_potencia_biotica.RDS"))


a <- readRDS(here::here("1. Data", "Procesada", "tabla_potencia_biotica.RDS"))


especies_descartadas <- a %>%
  dplyr::filter(Potencia_KPSS_pct <= 95) %>%
  dplyr::pull(Especie)

b <- especies_ganadoras_estacionariedad[!(especies_ganadoras_estacionariedad %in% especies_descartadas)]
especies_ganadoras_estacionariedad <- b





## Series Climáticas ----

tabla_clima_d0 <- analizar_estacionariedad_clima_d(df_clima, d = 0)
print(tabla_clima_d0[1:121,])


# 2. Climáticos: Nombres de las variables estrictamente estacionarias
clima_ganador <- tabla_clima_d0 %>% 
  filter(Veredicto == "ESTACIONARIO OK") %>% 
  pull(Variable)

cat(sprintf("[!] Supervivencia Climática: %d variables de %d\n", 
            length(clima_ganador), nrow(tabla_clima_d0)))
print(clima_ganador)



# saveRDS(especies_ganadoras_estacionariedad,
#         here::here("1. Data", "Procesada", "especies_ganadoras_estacionariedad.RDS"))
# 
# 
# saveRDS(clima_ganador,
#         here::here("1. Data", "Procesada", "clima_ganador.RDS"))




especies_ganadoras_estacionariedad <- readRDS(here::here("1. Data", 
                                                         "Procesada",
                                                         "especies_ganadoras_estacionariedad.RDS"))


clima_ganador <- readRDS(here::here("1. Data", 
                                    "Procesada",
                                    "clima_ganador.RDS"))




# 2. Matriz Biótica Final
# Al no existir rezagos por diferenciación, no se pierde la primera fila (2002-Wet).
df_biotica_final <- df %>%
  dplyr::filter(name %in% especies_ganadoras_estacionariedad) %>%
  dplyr::select(name, seasonYear, all_of(var_dependiente))



# 3. Matriz Climática Final
df_clima_final <- df_clima %>%
  dplyr::select(seasonYear, dummy_dry, dummy_wet, all_of(clima_ganador))


# Verificación
print(head(df_biotica_final, 3))
print(head(df_clima_final, 3))




# 2. ALMACENAMIENTO -------------------------------------------------------


# 
# saveRDS(df_biotica_final,
#         here::here("1. Data", "Procesada", "df_final_filtrado.RDS"))
# 
# 
# saveRDS(df_clima_final,
#         here::here("1. Data", "Procesada", "df_clima_final_filtrado.RDS"))
# 







