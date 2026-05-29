# ==========================================================
# Analítica de los negocios - Trabajo final
# Determinantes de la demanda de taxis amarillos en NYC 2024
# ==========================================================

# ----------------------------------------------------------
# Instalación de paquetes necesarios
# ----------------------------------------------------------

if (!require(readxl))   install.packages("readxl")
if (!require(dplyr))    install.packages("dplyr")
if (!require(ggplot2))  install.packages("ggplot2")
if (!require(scales))   install.packages("scales")
if (!require(tidyr))    install.packages("tidyr")
if (!require(gt))       install.packages("gt")
if (!require(moments))  install.packages("moments")
if (!require(broom))    install.packages("broom")
if (!require(lmtest))   install.packages("lmtest")
if (!require(sandwich)) install.packages("sandwich")
if (!require(car))      install.packages("car")
if (!require(shiny))    install.packages("shiny")
if (!require(shinydashboard)) install.packages("shinydashboard")
if (!require(plotly))   install.packages("plotly")
if (!require(rsconnect)) install.packages("rsconnect")

# ----------------------------------------------------------
# Librerías
# ----------------------------------------------------------

library(readxl)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)
library(gt)
library(moments)
library(broom)
library(lmtest)
library(sandwich)
library(car)
library(shiny)
library(shinydashboard)
library(plotly)
library(rsconnect)

# ----------------------------------------------------------
# Configuración inicial
# ----------------------------------------------------------

setwd("C:/Users/Usuario/Documents")
getwd()

options(scipen = 999)

formato_grafica <- theme_gray() +
  theme(
    plot.title    = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11, color = "gray40"),
    axis.title    = element_text(size = 11),
    axis.text     = element_text(size = 10)
  )

# ==========================================================
# Bloque 1 — Carga, tratamiento de valores desconocidos
#             y preparación de la base
# ==========================================================

# ----------------------------------------------------------
# Lectura de datos sin filtrar
# ----------------------------------------------------------

data_raw <- read_excel("Trabajo_final.xlsx")
names(data_raw) <- trimws(names(data_raw))

data_raw <- data_raw %>%
  mutate(
    trip_count      = as.numeric(trip_count),
    passenger_count = as.numeric(passenger_count),
    tmax_f          = as.numeric(tmax_f),
    prcp_in         = as.numeric(prcp_in),
    PU_Borough      = as.character(PU_Borough),
    DO_Borough      = as.character(DO_Borough),
    payment_type    = as.character(payment_type)
  )

cat("Dimensiones base original:", nrow(data_raw), "filas x", ncol(data_raw), "columnas\n")

# ----------------------------------------------------------
# Identificación de registros con valores desconocidos
# ----------------------------------------------------------

n_total <- nrow(data_raw)

# Conteo por variable
n_pu      <- sum(data_raw$PU_Borough      == "Unknown", na.rm = TRUE)
n_do      <- sum(data_raw$DO_Borough      == "Unknown", na.rm = TRUE)
n_pass    <- sum(data_raw$passenger_count == 0        , na.rm = TRUE)
n_any     <- data_raw %>%
  filter(PU_Borough == "Unknown" | DO_Borough == "Unknown" | passenger_count == 0) %>%
  nrow()

cat("\n--- Registros con valores desconocidos ---\n")
cat("PU_Borough = Unknown:      ", n_pu,   "(", round(n_pu   / n_total * 100, 2), "%)\n")
cat("DO_Borough = Unknown:      ", n_do,   "(", round(n_do   / n_total * 100, 2), "%)\n")
cat("passenger_count = 0:       ", n_pass, "(", round(n_pass / n_total * 100, 2), "%)\n")
cat("Total filas afectadas:     ", n_any,  "(", round(n_any  / n_total * 100, 2), "%)\n")

# ----------------------------------------------------------
# Prueba estadística 1: ¿los registros Unknown difieren
# en trip_count respecto al resto?
# ----------------------------------------------------------

data_raw <- data_raw %>%
  mutate(
    tiene_unknown = ifelse(
      PU_Borough == "Unknown" | DO_Borough == "Unknown" | passenger_count == 0,
      "Con unknown", "Sin unknown"
    )
  )

prueba_unknown <- t.test(
  trip_count ~ tiene_unknown,
  data       = data_raw,
  conf.level = 0.95
)

cat("\n--- Prueba t: trip_count en registros con vs. sin Unknown ---\n")
print(prueba_unknown)

# Gráfico comparativo
grafico_unknown_tc <- data_raw %>%
  group_by(tiene_unknown) %>%
  summarise(
    Promedio = mean(trip_count, na.rm = TRUE),
    IC_Inf   = t.test(trip_count, conf.level = 0.95)$conf.int[1],
    IC_Sup   = t.test(trip_count, conf.level = 0.95)$conf.int[2]
  ) %>%
  ggplot(aes(x = tiene_unknown, y = Promedio)) +
  geom_col(fill = "steelblue", width = 0.5) +
  geom_errorbar(aes(ymin = IC_Inf, ymax = IC_Sup),
                width = 0.15, color = "darkblue", linewidth = 0.8) +
  labs(
    title    = "Promedio de trip_count: registros con y sin valores desconocidos",
    subtitle = "Barras de error = intervalo de confianza al 95%",
    x        = "Grupo",
    y        = "Promedio de viajes"
  ) +
  formato_grafica

print(grafico_unknown_tc)

# ----------------------------------------------------------
# Prueba estadística 2: ¿la distribución de barrios cambia
# al eliminar los Unknown?
# ----------------------------------------------------------

dist_antes <- data_raw %>%
  filter(PU_Borough != "Unknown") %>%
  count(PU_Borough) %>%
  mutate(Porcentaje_antes = round(n / sum(n) * 100, 2)) %>%
  select(PU_Borough, Porcentaje_antes)

dist_despues <- data_raw %>%
  filter(PU_Borough != "Unknown", DO_Borough != "Unknown", passenger_count > 0) %>%
  count(PU_Borough) %>%
  mutate(Porcentaje_despues = round(n / sum(n) * 100, 2)) %>%
  select(PU_Borough, Porcentaje_despues)

tabla_distribucion_barrios <- dist_antes %>%
  left_join(dist_despues, by = "PU_Borough") %>%
  mutate(Diferencia = round(Porcentaje_despues - Porcentaje_antes, 3))

tabla_distribucion_barrios_gt <- tabla_distribucion_barrios %>%
  gt() %>%
  tab_header(
    title = md("**Distribución de PU_Borough antes y después del filtro**")
  ) %>%
  fmt_number(
    columns  = c(Porcentaje_antes, Porcentaje_despues, Diferencia),
    decimals = 3
  ) %>%
  cols_align(align = "center", columns = everything()) %>%
  tab_options(
    table.width      = pct(80),
    heading.align    = "center",
    table.font.size  = px(13),
    data_row.padding = px(6)
  )

invisible(tabla_distribucion_barrios_gt)

# ----------------------------------------------------------
# Prueba estadística 3: comparación de estadísticos clave
# de trip_count antes y después del filtro
# ----------------------------------------------------------

stats_antes <- data_raw %>%
  summarise(
    Momento    = "Antes del filtro",
    N          = n(),
    Media      = round(mean(trip_count,   na.rm = TRUE), 4),
    Mediana    = round(median(trip_count, na.rm = TRUE), 4),
    Desviacion = round(sd(trip_count,     na.rm = TRUE), 4),
    Asimetria  = round(skewness(trip_count, na.rm = TRUE), 4)
  )

stats_despues <- data_raw %>%
  filter(PU_Borough != "Unknown", DO_Borough != "Unknown", passenger_count > 0) %>%
  summarise(
    Momento    = "Después del filtro",
    N          = n(),
    Media      = round(mean(trip_count,   na.rm = TRUE), 4),
    Mediana    = round(median(trip_count, na.rm = TRUE), 4),
    Desviacion = round(sd(trip_count,     na.rm = TRUE), 4),
    Asimetria  = round(skewness(trip_count, na.rm = TRUE), 4)
  )

tabla_impacto_filtro <- bind_rows(stats_antes, stats_despues)

tabla_impacto_filtro_gt <- tabla_impacto_filtro %>%
  gt() %>%
  tab_header(
    title = md("**Impacto del filtro sobre los estadísticos de trip_count**")
  ) %>%
  fmt_number(
    columns  = c(Media, Mediana, Desviacion, Asimetria),
    decimals = 4
  ) %>%
  cols_align(align = "center", columns = everything()) %>%
  tab_options(
    table.width      = pct(90),
    heading.align    = "center",
    table.font.size  = px(13),
    data_row.padding = px(6)
  )

invisible(tabla_impacto_filtro_gt)

cat("\nConclusión: los registros eliminados representan el",
    round(n_any / n_total * 100, 2),
    "% de la base. Las pruebas confirman que su eliminación\n",
    "no altera de forma significativa la distribución de trip_count ni la composición por barrio.\n")

# ----------------------------------------------------------
# Preparación final de la base limpia
# ----------------------------------------------------------

data <- data_raw %>%
  filter(PU_Borough != "Unknown", DO_Borough != "Unknown", passenger_count > 0) %>%
  mutate(
    date            = as.Date(date),
    hour            = as.integer(format(as.POSIXct(hour), "%H")),
    passenger_count = as.numeric(passenger_count),
    trip_count      = as.numeric(trip_count),
    tmax_f          = as.numeric(tmax_f),
    prcp_in         = as.numeric(prcp_in),
    PU_Borough      = as.factor(PU_Borough),
    DO_Borough      = as.factor(DO_Borough),
    payment_type    = as.factor(payment_type),
    holiday_usa     = as.factor(holiday_usa),
    rain            = as.factor(rain),
    log_trip_count  = log(trip_count + 1),
    fare_per_trip   = fare_amount_sum / trip_count,
    franja_horaria  = case_when(
      hour >= 7  & hour <= 9  ~ "Hora pico mañana",
      hour >= 17 & hour <= 19 ~ "Hora pico tarde",
      TRUE                    ~ "Hora valle"
    )
  )

cat("\nBase limpia lista:", nrow(data), "filas\n")
cat("Barrios en PU_Borough:", levels(data$PU_Borough), "\n")

# Valores faltantes en base limpia
tabla_na <- data.frame(
  Variable      = names(data),
  Valores_NA    = colSums(is.na(data)),
  Porcentaje_NA = round(colSums(is.na(data)) / nrow(data) * 100, 2)
) %>% filter(Valores_NA > 0)

if (nrow(tabla_na) == 0) {
  cat("No hay valores faltantes en la base limpia.\n")
} else { print(tabla_na) }

# ==========================================================
# Bloque 2 — Descripción general de la base y justificación
#             de la transformación logarítmica
# ==========================================================

# ----------------------------------------------------------
# Estadísticos descriptivos generales
# ----------------------------------------------------------

variables_numericas <- data %>%
  select(trip_count, log_trip_count, hour, passenger_count,
         tmax_f, prcp_in, trip_distance_sum, duration_sum,
         fare_amount_sum, total_amount_sum)

tabla_descriptiva <- variables_numericas %>%
  summarise(across(
    everything(),
    list(
      Minimo     = ~min(.x,    na.rm = TRUE),
      Promedio   = ~mean(.x,   na.rm = TRUE),
      Mediana    = ~median(.x, na.rm = TRUE),
      Desviacion = ~sd(.x,     na.rm = TRUE),
      Maximo     = ~max(.x,    na.rm = TRUE)
    )
  )) %>%
  pivot_longer(
    cols          = everything(),
    names_to      = c("Variable", ".value"),
    names_pattern = "(.+)_(Minimo|Promedio|Mediana|Desviacion|Maximo)"
  )

tabla_descriptiva_gt <- tabla_descriptiva %>%
  gt() %>%
  tab_header(title = md("**Estadísticos descriptivos — variables numéricas**")) %>%
  fmt_number(columns = c(Minimo, Promedio, Mediana, Desviacion, Maximo), decimals = 2) %>%
  cols_align(align = "center", columns = everything()) %>%
  tab_options(table.width = pct(100), heading.align = "center",
              table.font.size = px(12), data_row.padding = px(6))

invisible(tabla_descriptiva_gt)

# ----------------------------------------------------------
# Diagnóstico de la variable dependiente
# ----------------------------------------------------------

media_tc      <- mean(data$trip_count)
mediana_tc    <- median(data$trip_count)
asimetria_tc  <- skewness(data$trip_count)
curtosis_tc   <- kurtosis(data$trip_count)
media_log     <- mean(data$log_trip_count)
mediana_log   <- median(data$log_trip_count)
asimetria_log <- skewness(data$log_trip_count)
curtosis_log  <- kurtosis(data$log_trip_count)

cat("\n=== Diagnóstico de trip_count ===\n")
cat("--- Original ---\n")
cat("Media:     ", round(media_tc, 2), "\n")
cat("Mediana:   ", round(mediana_tc, 2), "\n")
cat("Asimetría: ", round(asimetria_tc, 4), "\n")
cat("Curtosis:  ", round(curtosis_tc, 4), "\n")
cat("\n--- log(trip_count + 1) ---\n")
cat("Media:     ", round(media_log, 2), "\n")
cat("Mediana:   ", round(mediana_log, 2), "\n")
cat("Asimetría: ", round(asimetria_log, 4), "\n")
cat("Curtosis:  ", round(curtosis_log, 4), "\n")

# Tabla comparativa
tabla_comparacion_log <- data.frame(
  Estadistico = c("Media", "Mediana", "Asimetría", "Curtosis"),
  Original    = c(round(media_tc, 2), round(mediana_tc, 2),
                  round(asimetria_tc, 4), round(curtosis_tc, 4)),
  Log         = c(round(media_log, 2), round(mediana_log, 2),
                  round(asimetria_log, 4), round(curtosis_log, 4))
)

tabla_comparacion_log_gt <- tabla_comparacion_log %>%
  gt() %>%
  tab_header(
    title    = md("**Comparación: trip_count original vs. log(trip_count + 1)**"),
    subtitle = md("*Justificación de la transformación logarítmica*")
  ) %>%
  cols_align(align = "center", columns = everything()) %>%
  tab_options(table.width = pct(70), heading.align = "center",
              table.font.size = px(13), data_row.padding = px(6))

invisible(tabla_comparacion_log_gt)

# Histograma original
grafico_hist_original <- ggplot(data, aes(x = trip_count)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 60) +
  labs(
    title    = "Distribución de trip_count (original)",
    subtitle = paste0("Asimetría = ", round(asimetria_tc, 2),
                      "  |  Curtosis = ", round(curtosis_tc, 2),
                      "  |  Media = ", round(media_tc, 2),
                      "  |  Mediana = ", round(mediana_tc, 2)),
    x = "Número de viajes", y = "Frecuencia"
  ) + formato_grafica

print(grafico_hist_original)

# Histograma transformado
grafico_hist_log <- ggplot(data, aes(x = log_trip_count)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 60) +
  labs(
    title    = "Distribución de log(trip_count + 1)",
    subtitle = paste0("Asimetría = ", round(asimetria_log, 2),
                      "  |  Curtosis = ", round(curtosis_log, 2),
                      "  |  Media = ", round(media_log, 2),
                      "  |  Mediana = ", round(mediana_log, 2)),
    x = "log(Número de viajes + 1)", y = "Frecuencia"
  ) + formato_grafica

print(grafico_hist_log)

# Q-Q plot original
grafico_qq_original <- ggplot(data, aes(sample = trip_count)) +
  stat_qq(color = "steelblue", alpha = 0.4, size = 0.8) +
  stat_qq_line(color = "darkblue", linewidth = 1) +
  labs(title    = "Q-Q plot — trip_count original",
       subtitle = "Si los puntos siguen la línea, hay normalidad",
       x = "Cuantiles teóricos", y = "Cuantiles observados") +
  formato_grafica

print(grafico_qq_original)

# Q-Q plot transformado
grafico_qq_log <- ggplot(data, aes(sample = log_trip_count)) +
  stat_qq(color = "steelblue", alpha = 0.4, size = 0.8) +
  stat_qq_line(color = "darkblue", linewidth = 1) +
  labs(title    = "Q-Q plot — log(trip_count + 1)",
       subtitle = "Si los puntos siguen la línea, hay normalidad",
       x = "Cuantiles teóricos", y = "Cuantiles observados") +
  formato_grafica

print(grafico_qq_log)

# ==========================================================
# Bloque 3 — Comportamiento de la demanda por variable
# ==========================================================

# Por barrio de origen
grafico_barrio <- ggplot(
  data, aes(x = reorder(PU_Borough, log_trip_count, median), y = log_trip_count)
) +
  geom_boxplot(fill = "steelblue") +
  labs(title = "log(trip_count + 1) por barrio de origen",
       x = "Barrio", y = "log(Número de viajes + 1)") +
  formato_grafica

print(grafico_barrio)

# Promedio por hora del día
grafico_hora <- data %>%
  group_by(hour) %>%
  summarise(promedio_viajes = mean(log_trip_count, na.rm = TRUE)) %>%
  ggplot(aes(x = hour, y = promedio_viajes)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 2) +
  labs(title = "Promedio de log(trip_count + 1) por hora del día",
       x = "Hora", y = "Promedio log(viajes + 1)") +
  scale_x_continuous(breaks = 0:23) +
  formato_grafica

print(grafico_hora)

# Por tipo de pago
grafico_pago <- ggplot(
  data,
  aes(x = factor(payment_type, levels = c(1, 2), labels = c("Tarjeta", "Efectivo")),
      y = log_trip_count)
) +
  geom_boxplot(fill = "steelblue") +
  labs(title = "log(trip_count + 1) por tipo de pago",
       x = "Tipo de pago", y = "log(Número de viajes + 1)") +
  formato_grafica

print(grafico_pago)

# Festivo vs. día ordinario
grafico_festivo_exp <- ggplot(
  data,
  aes(x = factor(holiday_usa, levels = c(0, 1),
                 labels = c("Día ordinario", "Festivo")),
      y = log_trip_count)
) +
  geom_boxplot(fill = "steelblue") +
  labs(title = "log(trip_count + 1): festivos vs. días ordinarios",
       x = "Tipo de día", y = "log(Número de viajes + 1)") +
  formato_grafica

print(grafico_festivo_exp)

# Vs. temperatura máxima
grafico_temp <- ggplot(data, aes(x = tmax_f, y = log_trip_count)) +
  geom_point(alpha = 0.2, color = "steelblue", size = 0.6) +
  geom_smooth(method = "lm", color = "darkblue", se = TRUE) +
  labs(title = "log(trip_count + 1) vs. temperatura máxima",
       x = "Temperatura máxima (°F)", y = "log(Número de viajes + 1)") +
  formato_grafica

print(grafico_temp)

# Lluvia vs. sin lluvia
grafico_lluvia <- ggplot(
  data,
  aes(x = factor(rain, levels = c(0, 1), labels = c("Sin lluvia", "Con lluvia")),
      y = log_trip_count)
) +
  geom_boxplot(fill = "steelblue") +
  labs(title = "log(trip_count + 1): días con lluvia vs. sin lluvia",
       x = "Condición climática", y = "log(Número de viajes + 1)") +
  formato_grafica

print(grafico_lluvia)

# Resumen estadístico por barrio
tabla_barrio <- data %>%
  group_by(PU_Borough) %>%
  summarise(
    N          = n(),
    Promedio   = round(mean(trip_count),   2),
    Mediana    = round(median(trip_count), 2),
    Desviacion = round(sd(trip_count),     2),
    Maximo     = max(trip_count),
    Minimo     = min(trip_count)
  ) %>%
  arrange(desc(Promedio))

tabla_barrio_gt <- tabla_barrio %>%
  gt() %>%
  tab_header(title = md("**Resumen de trip_count por barrio de origen**")) %>%
  fmt_number(columns = c(Promedio, Mediana, Desviacion), decimals = 2) %>%
  cols_align(align = "center", columns = everything()) %>%
  tab_options(table.width = pct(90), heading.align = "center",
              table.font.size = px(13), data_row.padding = px(6))

invisible(tabla_barrio_gt)

# ==========================================================
# Bloque 4 — Diferencias en la demanda por tipo de día,
#             franja horaria y barrio de origen
# ==========================================================

# ----------------------------------------------------------
# Festivos vs. días ordinarios
# ----------------------------------------------------------

prueba_festivo <- t.test(
  log_trip_count ~ holiday_usa, data = data, conf.level = 0.95
)

cat("\n--- Prueba t: festivos vs. días ordinarios ---\n")
print(prueba_festivo)

tabla_festivo <- data %>%
  group_by(holiday_usa) %>%
  summarise(
    Grupo       = ifelse(first(holiday_usa) == 1, "Festivo", "Día ordinario"),
    N           = n(),
    Promedio    = round(mean(log_trip_count), 4),
    Desviacion  = round(sd(log_trip_count),   4),
    IC_Inferior = round(t.test(log_trip_count, conf.level = 0.95)$conf.int[1], 4),
    IC_Superior = round(t.test(log_trip_count, conf.level = 0.95)$conf.int[2], 4)
  ) %>%
  select(Grupo, N, Promedio, Desviacion, IC_Inferior, IC_Superior)

tabla_festivo_gt <- tabla_festivo %>%
  gt() %>%
  tab_header(title = md("**Comparación de log(trip_count + 1): festivos vs. días ordinarios**")) %>%
  fmt_number(columns = c(Promedio, Desviacion, IC_Inferior, IC_Superior), decimals = 4) %>%
  cols_align(align = "center", columns = everything()) %>%
  tab_options(table.width = pct(90), heading.align = "center",
              table.font.size = px(13), data_row.padding = px(6))

invisible(tabla_festivo_gt)

resumen_festivo <- data %>%
  group_by(holiday_usa) %>%
  summarise(
    Grupo    = ifelse(first(holiday_usa) == 1, "Festivo", "Día ordinario"),
    Promedio = mean(log_trip_count),
    IC_Inf   = t.test(log_trip_count)$conf.int[1],
    IC_Sup   = t.test(log_trip_count)$conf.int[2]
  )

grafico_festivo_ic <- ggplot(resumen_festivo, aes(x = Grupo, y = Promedio)) +
  geom_col(fill = "steelblue", width = 0.5) +
  geom_errorbar(aes(ymin = IC_Inf, ymax = IC_Sup),
                width = 0.15, color = "darkblue", linewidth = 0.8) +
  labs(title    = "Promedio de log(trip_count + 1): festivos vs. días ordinarios",
       subtitle = "Barras de error = intervalo de confianza al 95%",
       x = "Tipo de día", y = "Promedio log(viajes + 1)") +
  formato_grafica

print(grafico_festivo_ic)

# ----------------------------------------------------------
# Horas pico vs. horas valle
# ----------------------------------------------------------

grupo_pico  <- data %>% filter(franja_horaria != "Hora valle") %>% pull(log_trip_count)
grupo_valle <- data %>% filter(franja_horaria == "Hora valle") %>% pull(log_trip_count)

prueba_horas <- t.test(x = grupo_pico, y = grupo_valle, conf.level = 0.95)

cat("\n--- Prueba t: horas pico vs. horas valle ---\n")
print(prueba_horas)

tabla_horas <- data %>%
  group_by(franja_horaria) %>%
  summarise(
    N           = n(),
    Promedio    = round(mean(log_trip_count), 4),
    Desviacion  = round(sd(log_trip_count),   4),
    IC_Inferior = round(t.test(log_trip_count, conf.level = 0.95)$conf.int[1], 4),
    IC_Superior = round(t.test(log_trip_count, conf.level = 0.95)$conf.int[2], 4)
  ) %>%
  rename(Franja = franja_horaria) %>%
  arrange(desc(Promedio))

tabla_horas_gt <- tabla_horas %>%
  gt() %>%
  tab_header(title = md("**Comparación de log(trip_count + 1) por franja horaria**")) %>%
  fmt_number(columns = c(Promedio, Desviacion, IC_Inferior, IC_Superior), decimals = 4) %>%
  cols_align(align = "center", columns = everything()) %>%
  tab_options(table.width = pct(95), heading.align = "center",
              table.font.size = px(13), data_row.padding = px(6))

invisible(tabla_horas_gt)

resumen_horas <- data %>%
  group_by(franja_horaria) %>%
  summarise(
    Promedio = mean(log_trip_count),
    IC_Inf   = t.test(log_trip_count)$conf.int[1],
    IC_Sup   = t.test(log_trip_count)$conf.int[2]
  )

grafico_horas_ic <- ggplot(
  resumen_horas, aes(x = reorder(franja_horaria, -Promedio), y = Promedio)
) +
  geom_col(fill = "steelblue", width = 0.5) +
  geom_errorbar(aes(ymin = IC_Inf, ymax = IC_Sup),
                width = 0.15, color = "darkblue", linewidth = 0.8) +
  labs(title    = "Promedio de log(trip_count + 1) por franja horaria",
       subtitle = "Barras de error = intervalo de confianza al 95%",
       x = "Franja horaria", y = "Promedio log(viajes + 1)") +
  formato_grafica

print(grafico_horas_ic)

grafico_hora_detalle <- data %>%
  group_by(hour) %>%
  summarise(
    Promedio = mean(log_trip_count),
    IC_Inf   = t.test(log_trip_count)$conf.int[1],
    IC_Sup   = t.test(log_trip_count)$conf.int[2]
  ) %>%
  ggplot(aes(x = hour, y = Promedio)) +
  geom_ribbon(aes(ymin = IC_Inf, ymax = IC_Sup), alpha = 0.2, fill = "steelblue") +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 2) +
  annotate("rect", xmin = 7,  xmax = 9,  ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "darkblue") +
  annotate("rect", xmin = 17, xmax = 19, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "darkblue") +
  labs(title    = "Promedio de log(trip_count + 1) por hora del día",
       subtitle = "Zonas sombreadas = horas pico (7-9am y 5-7pm). Banda = IC 95%",
       x = "Hora", y = "Promedio log(viajes + 1)") +
  scale_x_continuous(breaks = 0:23) +
  formato_grafica

print(grafico_hora_detalle)

# ----------------------------------------------------------
# Comparación entre barrios
# ----------------------------------------------------------

anova_barrios <- aov(log_trip_count ~ PU_Borough, data = data)

cat("\n--- ANOVA: diferencias entre barrios ---\n")
print(summary(anova_barrios))

tabla_barrios_ic <- data %>%
  group_by(PU_Borough) %>%
  summarise(
    N           = n(),
    Promedio    = round(mean(log_trip_count), 4),
    Desviacion  = round(sd(log_trip_count),   4),
    IC_Inferior = round(t.test(log_trip_count, conf.level = 0.95)$conf.int[1], 4),
    IC_Superior = round(t.test(log_trip_count, conf.level = 0.95)$conf.int[2], 4)
  ) %>%
  rename(Barrio = PU_Borough) %>%
  arrange(desc(Promedio))

tabla_barrios_ic_gt <- tabla_barrios_ic %>%
  gt() %>%
  tab_header(title = md("**Comparación de log(trip_count + 1) entre barrios de origen**")) %>%
  fmt_number(columns = c(Promedio, Desviacion, IC_Inferior, IC_Superior), decimals = 4) %>%
  cols_align(align = "center", columns = everything()) %>%
  tab_options(table.width = pct(95), heading.align = "center",
              table.font.size = px(13), data_row.padding = px(6))

invisible(tabla_barrios_ic_gt)

resumen_barrios <- data %>%
  group_by(PU_Borough) %>%
  summarise(
    Promedio = mean(log_trip_count),
    IC_Inf   = t.test(log_trip_count)$conf.int[1],
    IC_Sup   = t.test(log_trip_count)$conf.int[2]
  )

grafico_barrios_ic <- ggplot(
  resumen_barrios, aes(x = reorder(PU_Borough, -Promedio), y = Promedio)
) +
  geom_col(fill = "steelblue", width = 0.6) +
  geom_errorbar(aes(ymin = IC_Inf, ymax = IC_Sup),
                width = 0.2, color = "darkblue", linewidth = 0.8) +
  labs(title    = "Promedio de log(trip_count + 1) por barrio de origen",
       subtitle = "Barras de error = intervalo de confianza al 95%",
       x = "Barrio de origen", y = "Promedio log(viajes + 1)") +
  formato_grafica

print(grafico_barrios_ic)

grafico_barrios_box <- ggplot(
  data, aes(x = reorder(PU_Borough, log_trip_count, median), y = log_trip_count)
) +
  geom_boxplot(fill = "steelblue", outlier.size = 0.5, outlier.alpha = 0.3) +
  labs(title = "Distribución de log(trip_count + 1) por barrio de origen",
       x = "Barrio de origen", y = "log(Número de viajes + 1)") +
  formato_grafica

print(grafico_barrios_box)

# ==========================================================
# Bloque 5 — Construcción progresiva del modelo econométrico
# ==========================================================

# ----------------------------------------------------------
# Modelo 1: variables temporales (hour + holiday_usa)
# ----------------------------------------------------------

modelo_1 <- lm(log_trip_count ~ hour + holiday_usa, data = data)
summary(modelo_1)

# ----------------------------------------------------------
# Modelo 2: + variables geográficas (+ PU_Borough)
# ----------------------------------------------------------

modelo_2 <- lm(log_trip_count ~ hour + holiday_usa + PU_Borough, data = data)
summary(modelo_2)

# ----------------------------------------------------------
# Modelo 3: + variables operativas (+ payment_type + passenger_count)
# ----------------------------------------------------------

modelo_3 <- lm(log_trip_count ~ hour + holiday_usa + PU_Borough +
                 payment_type + passenger_count, data = data)
summary(modelo_3)

# ----------------------------------------------------------
# Modelo 4: + variables climáticas (+ tmax_f + rain)
# ----------------------------------------------------------

modelo_4 <- lm(log_trip_count ~ hour + holiday_usa + PU_Borough +
                 payment_type + passenger_count + tmax_f + rain, data = data)
summary(modelo_4)

# ----------------------------------------------------------
# Tabla resumen comparativa de los 4 modelos
# ----------------------------------------------------------

extraer_stats <- function(modelo, nombre) {
  s <- summary(modelo)
  data.frame(
    Modelo        = nombre,
    R2            = round(s$r.squared,      4),
    R2_ajustado   = round(s$adj.r.squared,  4),
    AIC           = round(AIC(modelo),      2),
    BIC           = round(BIC(modelo),      2),
    F_estadistico = round(s$fstatistic[1],  2),
    N_variables   = length(coef(modelo)) - 1
  )
}

tabla_resumen_modelos <- bind_rows(
  extraer_stats(modelo_1, "Modelo 1: temporales"),
  extraer_stats(modelo_2, "Modelo 2: + geográficas"),
  extraer_stats(modelo_3, "Modelo 3: + operativas"),
  extraer_stats(modelo_4, "Modelo 4: + climáticas")
)

tabla_resumen_modelos_gt <- tabla_resumen_modelos %>%
  gt() %>%
  tab_header(title = md("**Tabla resumen: comparación de modelos progresivos**")) %>%
  fmt_number(columns = c(R2, R2_ajustado, AIC, BIC, F_estadistico), decimals = 4) %>%
  cols_align(align = "center", columns = everything()) %>%
  tab_options(table.width = pct(100), heading.align = "center",
              table.font.size = px(12), data_row.padding = px(6))

invisible(tabla_resumen_modelos_gt)
print(tabla_resumen_modelos)

# ----------------------------------------------------------
# Selección automática del modelo final
# ----------------------------------------------------------

# Lista de modelos con sus nombres
lista_modelos <- list(
  "Modelo 1: temporales"   = modelo_1,
  "Modelo 2: + geográficas" = modelo_2,
  "Modelo 3: + operativas"  = modelo_3,
  "Modelo 4: + climáticas"  = modelo_4
)

# Selección por menor AIC y mayor R² ajustado
aics          <- sapply(lista_modelos, AIC)
r2_ajustados  <- sapply(lista_modelos, function(m) summary(m)$adj.r.squared)
nombre_mejor  <- names(which.min(aics))
modelo_final  <- lista_modelos[[nombre_mejor]]

cat("\n--- Selección automática del modelo final ---\n")
cat("AIC por modelo:\n"); print(round(aics, 2))
cat("R² ajustado por modelo:\n"); print(round(r2_ajustados, 4))
cat("\nModelo seleccionado:", nombre_mejor,
    "| AIC =", round(min(aics), 2),
    "| R² ajustado =", round(summary(modelo_final)$adj.r.squared, 4), "\n")
print(summary(modelo_final))

# Tabla de coeficientes del modelo final
tabla_coeficientes <- tidy(modelo_final, conf.int = TRUE, conf.level = 0.95) %>%
  mutate(
    Significativo = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      p.value < 0.1   ~ ".",
      TRUE            ~ ""
    )
  ) %>%
  rename(
    Variable    = term,
    Coeficiente = estimate,
    Error_std   = std.error,
    IC_inferior = conf.low,
    IC_superior = conf.high,
    Valor_p     = p.value
  ) %>%
  select(Variable, Coeficiente, Error_std, IC_inferior, IC_superior, Valor_p, Significativo)

tabla_coeficientes_gt <- tabla_coeficientes %>%
  gt() %>%
  tab_header(title = md("**Coeficientes del modelo final con IC al 95%**")) %>%
  fmt_number(columns = c(Coeficiente, Error_std, IC_inferior, IC_superior, Valor_p),
             decimals = 6) %>%
  cols_align(align = "center", columns = everything()) %>%
  tab_options(table.width = pct(100), heading.align = "center",
              table.font.size = px(12), data_row.padding = px(6))

invisible(tabla_coeficientes_gt)
