# Trabajo Final. NYC Taxis

Este repositorio contiene el desarrollo y la resolución del **Trabajo Final** del curso **Analítica de los Negocios**.

Datos y archivos de réplica para el análisis del caso por:

**David Santiago Aguirre Polanco**  
**María Juanita Rojas Chacón**

--------------------------------------------------

# **Resumen**

Este trabajo analiza los determinantes de la demanda de taxis amarillos en la ciudad de Nueva York durante el año 2024, desde una perspectiva de analítica de negocios. El objetivo principal es identificar y cuantificar los factores temporales, geográficos y climáticos que explican el número de viajes realizados, con el fin de generar evidencia empírica útil para la toma de decisiones en empresas del sector transporte.

A partir de una base de datos que integra registros oficiales del Taxi & Limousine Commission (TLC) y datos climáticos de la estación Central Park de la NOAA, se realiza un análisis cuantitativo para explorar patrones de demanda, construir modelos econométricos progresivos y desarrollar un dashboard interactivo orientado al cliente final.

El análisis incluye el estudio de variables como:

- trip_count (Número de viajes por hora y barrio)
- PU_Borough (Barrio de origen del viaje)
- hour (Hora del día)
- payment_type (Tipo de pago)
- passenger_count (Promedio de pasajeros)
- tmax_f (Temperatura máxima diaria en °F)
- rain (Presencia de lluvia)
- holiday_usa (Días festivos federales)
- fare_amount_sum (Suma de tarifas del grupo)

A través de estadísticas descriptivas, pruebas de diferencias de medias, construcción progresiva de modelos de regresión lineal con transformación logarítmica, verificación de supuestos y un dashboard interactivo en Shiny, se busca comprender los determinantes de la demanda y evaluar la capacidad predictiva del modelo final.

--------------------------------------------------

# **Estructura del repositorio**

El repositorio está organizado en las siguientes carpetas:

--------------------------------------------------

# **Carpeta Document**

Esta carpeta contiene los documentos finales relacionados con el análisis.

**Archivos incluidos:**

- **Trabajo final. Analítica de los negocios.pdf**

Documento principal donde se presenta el desarrollo completo del análisis. Incluye la exploración de datos, el diagnóstico de la variable dependiente, la construcción progresiva del modelo econométrico, la validación de supuestos y las conclusiones sobre los determinantes de la demanda de taxis en Nueva York.

- **Resumen ejecutivo. Trabajo final. Analítica de los negocios.pdf**

Documento que presenta una síntesis de los hallazgos más relevantes del análisis. Destaca las conclusiones clave sobre los determinantes de la demanda de taxis y las implicaciones para la toma de decisiones en empresas del sector transporte en Nueva York.

--------------------------------------------------

# **Carpeta Scripts**

Esta carpeta contiene el código utilizado para desarrollar el análisis de datos.

El análisis fue realizado utilizando el software **R**.

**Archivo incluido:**

- **Trabajo final. Analítica de los negocios.R**

Este script incluye:

- Tratamiento de registros con valores desconocidos y pruebas de impacto
- Preparación y limpieza de los datos
- Cálculo de estadísticas descriptivas
- Diagnóstico y justificación de la transformación logarítmica
- Análisis exploratorio por variable
- Pruebas de diferencias de medias e intervalos de confianza al 95%
- Construcción progresiva de modelos de regresión (variables temporales, geográficas, operativas y climáticas)
- Selección automática del modelo final por AIC y R² ajustado
- Evaluación y validación del modelo (supuestos, residuos, homocedasticidad)
- Análisis de heterogeneidad por barrio
- Dashboard interactivo en Shiny con predictor de viajes, estimador de tarifa, comparador de barrios, análisis por hora y ficha del modelo

--------------------------------------------------

# **Carpeta Stores**

Esta carpeta contiene las bases de datos utilizadas para el análisis.

**Archivos incluidos:**

- **Trabajo_final.xlsx**

Este archivo contiene la información utilizada en el análisis, incluyendo datos sobre:

- Fecha y hora de los viajes
- Barrio de origen y destino
- Número de viajes, pasajeros y distancia recorrida
- Tarifas, propinas y cargos adicionales
- Tipo de pago
- Variables climáticas (temperatura máxima y precipitación)
- Indicador de días festivos federales

Estos datos constituyen la base para el análisis estadístico y la construcción de los resultados obtenidos.

--------------------------------------------------

# **Carpeta Views**

Esta carpeta contiene todas las **figuras, tablas y gráficos generados durante el análisis**.

Entre las visualizaciones incluidas se encuentran:

- Gráficos de distribución de trip_count original y transformada
- Q-Q plots de la variable dependiente y los residuos
- Comparaciones de demanda por barrio, hora, tipo de pago, festivos y clima
- Gráficos de intervalos de confianza al 95% por grupo
- Valores reales vs. ajustados y análisis de residuos
- Coeficientes del modelo final con intervalos de confianza
- Tablas de estadísticos descriptivos y resumen comparativo de modelos

Estas visualizaciones permiten interpretar y comunicar de manera clara los resultados obtenidos en el análisis.

--------------------------------------------------

# **Notas**

Para ejecutar correctamente el análisis se recomienda utilizar **R o RStudio**.

Antes de ejecutar el script es recomendable:

1. Configurar el directorio de trabajo en la carpeta **Scripts** del repositorio.
2. Verificar que todos los paquetes necesarios estén previamente instalados.
3. Ejecutar el script completo desde el inicio para garantizar que todos los objetos estén disponibles para el dashboard.

La velocidad de ejecución puede variar dependiendo de las características del equipo en el que se ejecuten los scripts.
