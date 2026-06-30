# ********************************************************************
# 1. DEFINICIÓN DE DEPENDENCIAS -----
# ********************************************************************

paquetes <- c(
  # --- Gestión de Rutas y Archivos ---
  "here",          # Rutas relativas automáticas basadas en el .Rproj
  "readxl",        # Lectura de archivos Excel
  "writexl",       # Escritura de archivos Excel
  
  # --- Manipulación y Procesamiento de Datos ---
  "tidyverse",     # Carga dplyr, tidyr, ggplot2, purrr, etc.
  "dplyr",         # (Explícito por seguridad de enmascaramiento)
  "tidyr",         # (Explícito por seguridad de enmascaramiento)
  "imputeTS",      # Imputación de Datos
  "lubridate",
  "data.table",
 
  # --- Ecología y Análisis de Comunidades ---
  "vegan",         # Análisis de ordenación, diversidad y ecología comunitaria
  "indicspecies",  # Identificación de especies indicadoras y co-ocurrencia
  
  # --- Modelamiento Estadístico Avanzado ---
  "vars",          # Modelos Vector Autorregresivos (VAR)
  "tseries",       # Análisis de series temporales y pruebas de estacionariedad
  
  # --- Reportes, Tablas y Visualización Extra ---
  "DT",            # Tablas interactivas para análisis exploratorio
  "knitr",         # Generación de reportes y formato de tablas dinámicas
  "patchwork",      # Combinación profesional de gráficos ggplot2 para LaTeX
  "car",
  "lmtest",
  "naniar",
  "scales"
)

# ********************************************************************
# 2. PREÁMBULO DE INSTALACIÓN Y CARGA AUTOMÁTICA ----
# ********************************************************************

paquetes_faltantes <- paquetes[!(paquetes %in% installed.packages()[, "Package"])]

if (length(paquetes_faltantes) > 0) {
  install.packages(paquetes_faltantes, dependencies = TRUE)
}

# Carga de librerías del proyecto
invisible(lapply(paquetes, library, character.only = TRUE))

# Carga de funciones auxliares del proyecto
source(here::here("2. Librerias - Funciones", "Funciones_Auxiliares.R"))


