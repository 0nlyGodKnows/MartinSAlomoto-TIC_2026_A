
# DICCIONARIO DE ARCHIVOS DE DATOS

Este documento detalla la estructura y el propósito de cada uno de los archivos intermedios y finales necesarios para el desarrollo de la metodología y resultados de este Trabajo de Integración Curricular.


# ************************************************************
# DICCIONARIO DE ARCHIVOS DE DATOS (.RDS) - PROYECTO TIC HORMIGAS
# ************************************************************

Este documento detalla la estructura y el propósito de cada uno de los archivos intermedios y finales generados durante la fase de depuración, tratamiento y expansión climática-biológica de la tesis.

| Archivo | Descripción Técnica |
| :--- | :--- |
| **`df_clean.rds`** | Base de datos biológica purgada. Contiene los registros históricos originales de captura de hormigas tras la corrección de anomalías cronológicas, imputación de fechas logísticas y eliminación de registros duplicados. |
| **`df_zero_filled.rds`** | Matriz de comunidad expandida mediante *zero-filling* ecológico. Estructura balanceada bajo el diseño factorial **Especie × Año × Temporada × Trampa**, donde los ceros explícitos representan ausencias reales de la especie en el muestreo. |
| **`dfClimateDay_pre_interp.rds`** | Serie temporal diaria cruda (2000-01-01 a 2019-12-31) de las variables meteorológicas locales de la isla (ET, RH, Temp, RA), conservando los vacíos de información (*gaps*) originales causados por fallos en los sensores de campo. |
| **`dfClimateDay_interp.rds`** | Serie temporal diaria continua (2000-01-01 a 2019-12-31) de las variables meteorológicas locales, con todos los vacíos de información completamente subsanados mediante interpolación matemática de Stineman. |
| **`df_clima.rds`** | Matriz intermedia de covariables ambientales agregadas por semestre (escala original). Contiene las métricas de las colecciones temporales vectorizadas (rezagos semanales, promedios continuos y acumulados estacionales) antes del filtrado del año y transformaciones logarítmicas. |
| **`nino.rds`** | Serie temporal mensual del Índice de Anomalía de Temperatura Superficial del Mar en la región Niño 3.4, transformada a formato largo (*tidy*) para el análisis macroclimático de largo plazo. |
| **`oni.rds`** | Serie temporal mensual del Índice Oceánico de El Niño (ONI), el indicador principal de la NOAA para monitorear el fenómeno ENSO, estructurada en formato largo (*tidy*) desde el año 2000. |
| **`df_final.rds`** | Matriz biológica condensada para los fines del modelado. Contiene las series temporales agregadas de abundancia e incidencia, junto a sus respectivas transformaciones mediante logaritmo natural (`ln_abundance`, `ln_incidence`). |
| **`df_clima_final.rds`** | Matriz de covariables climáticas transformadas y extendidas (35 semestres). Incluye las 121 variables locales linearizadas mediante logaritmo, los 10 índices ENSO en su escala original, las variables *dummy* estacionales y el año cronológico preservado. |
| **`especies_clave.rds`** | Vector indexado de tipo carácter (`character`). Almacena los nombres taxonómicos de las especies de hormigas dominantes seleccionadas bajo el criterio de consistencia temporal (>45% de presencia histórica). |
| **`tabla_potencia_biotica.rds`** | Resultados de aplicar la prueba de potencia a las series temporales bióticas. |
| **`especies_ganadoras_estacionariedad.rds`** | Vector de tipo carácter (`character`). Contiene la subcomunidad definitiva de especies de hormigas cuyas series temporales superaron satisfactoriamente las pruebas de estacionariedad, garantizando su viabilidad para el modelado matemático. |
| **`clima_ganador.rds`** | Vector de tipo carácter (`character`). Almacena los nombres de las 40 covariables climáticas que demostraron consistencia estadística (rechazo en ADF y no rechazo en KPSS), definiendo la estructura predictiva final del modelo. |
| **`df_final_filtrado.rds`** | Matriz biótica definitiva y depurada para el modelado. Contiene exclusivamente las series temporales (en escala logarítmica pura, sin estandarizar) de las especies que superaron rigurosamente las pruebas de estacionariedad. |
| **`df_clima_final_filtrado.rds`** | Matriz climática definitiva acotada a las covariables viables. Contiene únicamente las 40 variables meteorológicas estacionarias en escala logarítmica y las variables *dummy* estacionales. |















