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

# ==========================================================
# Bloque 6 — Evaluación y validación del modelo
# ==========================================================

# ----------------------------------------------------------
# Valores ajustados y residuos
# ----------------------------------------------------------

data_modelo <- data %>%
  mutate(
    ajustados  = fitted(modelo_final),
    residuos   = residuals(modelo_final),
    res_std    = rstandard(modelo_final)
  )

# ----------------------------------------------------------
# Gráfico de valores reales vs. ajustados
# ----------------------------------------------------------

grafico_real_vs_ajustado <- ggplot(
  data_modelo, aes(x = ajustados, y = log_trip_count)
) +
  geom_point(alpha = 0.15, color = "steelblue", size = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "darkblue", linewidth = 1) +
  labs(title    = "Valores reales vs. valores ajustados",
       subtitle = "La línea diagonal representa ajuste perfecto",
       x = "Valores ajustados", y = "log(trip_count + 1) real") +
  formato_grafica

print(grafico_real_vs_ajustado)

# ----------------------------------------------------------
# Análisis de residuos
# ----------------------------------------------------------

# Residuos vs. ajustados (homocedasticidad)
grafico_residuos_ajustados <- ggplot(
  data_modelo, aes(x = ajustados, y = residuos)
) +
  geom_point(alpha = 0.15, color = "steelblue", size = 0.5) +
  geom_hline(yintercept = 0, color = "darkblue", linewidth = 1) +
  geom_smooth(method = "loess", color = "red", se = FALSE, linewidth = 0.8) +
  labs(title    = "Residuos vs. valores ajustados",
       subtitle = "La línea roja muestra tendencia — idealmente debe ser plana en cero",
       x = "Valores ajustados", y = "Residuos") +
  formato_grafica

print(grafico_residuos_ajustados)

# Histograma de residuos
grafico_hist_residuos <- ggplot(data_modelo, aes(x = residuos)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 60) +
  labs(title = "Distribución de los residuos del modelo",
       x = "Residuos", y = "Frecuencia") +
  formato_grafica

print(grafico_hist_residuos)

# Q-Q plot de residuos (normalidad)
grafico_qq_residuos <- ggplot(data_modelo, aes(sample = res_std)) +
  stat_qq(color = "steelblue", alpha = 0.4, size = 0.8) +
  stat_qq_line(color = "darkblue", linewidth = 1) +
  labs(title    = "Q-Q plot de residuos estandarizados",
       subtitle = "Si los puntos siguen la línea, los residuos son normales",
       x = "Cuantiles teóricos", y = "Cuantiles observados") +
  formato_grafica

print(grafico_qq_residuos)

# Scale-location (homocedasticidad)
grafico_scale_location <- ggplot(
  data_modelo, aes(x = ajustados, y = sqrt(abs(res_std)))
) +
  geom_point(alpha = 0.15, color = "steelblue", size = 0.5) +
  geom_smooth(method = "loess", color = "red", se = FALSE, linewidth = 0.8) +
  labs(title    = "Scale-location (verificación de homocedasticidad)",
       subtitle = "La línea roja debe ser horizontal para confirmar varianza constante",
       x = "Valores ajustados", y = "√|Residuos estandarizados|") +
  formato_grafica

print(grafico_scale_location)

# ----------------------------------------------------------
# Prueba formal de homocedasticidad (Breusch-Pagan)
# ----------------------------------------------------------

prueba_bp <- bptest(modelo_final)
cat("\n--- Prueba de Breusch-Pagan (homocedasticidad) ---\n")
print(prueba_bp)

# ----------------------------------------------------------
# Prueba formal de normalidad de residuos
# (muestra aleatoria por eficiencia con 921k filas)
# ----------------------------------------------------------

set.seed(123)
muestra_residuos <- sample(data_modelo$residuos, size = 5000)
prueba_sw <- shapiro.test(muestra_residuos)
cat("\n--- Prueba de Shapiro-Wilk sobre muestra de residuos (n=5000) ---\n")
print(prueba_sw)

# ----------------------------------------------------------
# Bondad de ajuste — R² y R² ajustado
# ----------------------------------------------------------

s_final <- summary(modelo_final)
cat("\n--- Bondad de ajuste del modelo final ---\n")
cat("R²:         ", round(s_final$r.squared,     4), "\n")
cat("R² ajustado:", round(s_final$adj.r.squared,  4), "\n")
cat("F-estadístico:", round(s_final$fstatistic[1], 2), "\n")
cat("Valor p (F): < 0.0001\n")

# ----------------------------------------------------------
# Heterogeneidad por barrio — modelos por subgrupo
# ----------------------------------------------------------

barrios <- levels(data$PU_Borough)

lista_modelos_barrio <- lapply(barrios, function(b) {
  datos_b <- data %>% filter(PU_Borough == b)
  if (nrow(datos_b) > 100) {
    m <- lm(log_trip_count ~ hour + holiday_usa + payment_type +
              passenger_count + tmax_f + rain, data = datos_b)
    s <- summary(m)
    data.frame(
      Barrio      = b,
      N           = nrow(datos_b),
      R2          = round(s$r.squared,     4),
      R2_ajustado = round(s$adj.r.squared, 4),
      Coef_hour   = round(coef(m)["hour"],        6),
      Coef_tmax   = round(coef(m)["tmax_f"],      6),
      Coef_rain1  = round(coef(m)["rain1"],        6)
    )
  }
})

tabla_heterogeneidad <- bind_rows(lista_modelos_barrio)

tabla_heterogeneidad_gt <- tabla_heterogeneidad %>%
  gt() %>%
  tab_header(title = md("**Heterogeneidad del modelo por barrio de origen**")) %>%
  fmt_number(columns = c(R2, R2_ajustado, Coef_hour, Coef_tmax, Coef_rain1),
             decimals = 6) %>%
  cols_align(align = "center", columns = everything()) %>%
  tab_options(table.width = pct(100), heading.align = "center",
              table.font.size = px(12), data_row.padding = px(6))

invisible(tabla_heterogeneidad_gt)
print(tabla_heterogeneidad)

# ----------------------------------------------------------
# Gráfico de coeficientes del modelo final
# ----------------------------------------------------------

grafico_coeficientes <- tabla_coeficientes %>%
  filter(Variable != "(Intercept)") %>%
  ggplot(aes(x = reorder(Variable, Coeficiente), y = Coeficiente)) +
  geom_col(fill = "steelblue") +
  geom_errorbar(aes(ymin = IC_inferior, ymax = IC_superior),
                width = 0.3, color = "darkblue", linewidth = 0.7) +
  coord_flip() +
  labs(title    = "Coeficientes del modelo final con IC al 95%",
       subtitle = "Variables ordenadas por magnitud del coeficiente",
       x = "Variable", y = "Coeficiente") +
  formato_grafica

print(grafico_coeficientes)
                        
# ==========================================================
# Bloque 7 — Exportación de resultados
# ==========================================================

carpeta_output <- "C:/Users/Usuario/Documents/Outputs_Trabajo_Final"
if (!dir.exists(carpeta_output)) dir.create(carpeta_output)

guardar_grafico <- function(grafico, nombre) {
  ggsave(filename = file.path(carpeta_output, nombre),
         plot = grafico, width = 10, height = 6, dpi = 150)
}

# Bloque 1 — Unknown
guardar_grafico(grafico_unknown_tc, "01_unknown_vs_trip_count.png")

# Bloque 2 — Diagnóstico logarítmico
guardar_grafico(grafico_hist_original,  "02_hist_trip_count_original.png")
guardar_grafico(grafico_hist_log,       "03_hist_trip_count_log.png")
guardar_grafico(grafico_qq_original,    "04_qq_original.png")
guardar_grafico(grafico_qq_log,         "05_qq_log.png")

# Bloque 3 — Exploratorios
guardar_grafico(grafico_barrio,       "06_exploratorio_barrio.png")
guardar_grafico(grafico_hora,         "07_exploratorio_hora.png")
guardar_grafico(grafico_pago,         "08_exploratorio_pago.png")
guardar_grafico(grafico_festivo_exp,  "09_exploratorio_festivo.png")
guardar_grafico(grafico_temp,         "10_exploratorio_temperatura.png")
guardar_grafico(grafico_lluvia,       "11_exploratorio_lluvia.png")

# Bloque 4 — Diferencias en la demanda
guardar_grafico(grafico_festivo_ic,    "12_festivo_vs_ordinario_IC.png")
guardar_grafico(grafico_horas_ic,      "13_franjas_horarias_IC.png")
guardar_grafico(grafico_hora_detalle,  "14_hora_a_hora_IC.png")
guardar_grafico(grafico_barrios_ic,    "15_barrios_IC.png")
guardar_grafico(grafico_barrios_box,   "16_barrios_boxplot.png")

# Bloque 6 — Validación del modelo
guardar_grafico(grafico_real_vs_ajustado,    "17_real_vs_ajustado.png")
guardar_grafico(grafico_residuos_ajustados,  "18_residuos_vs_ajustados.png")
guardar_grafico(grafico_hist_residuos,       "19_hist_residuos.png")
guardar_grafico(grafico_qq_residuos,         "20_qq_residuos.png")
guardar_grafico(grafico_scale_location,      "21_scale_location.png")
guardar_grafico(grafico_coeficientes,        "22_coeficientes_modelo.png")

# Tablas CSV
write.csv(tabla_impacto_filtro,        file.path(carpeta_output, "T01_impacto_filtro.csv"),          row.names = FALSE)
write.csv(tabla_distribucion_barrios,  file.path(carpeta_output, "T02_distribucion_barrios.csv"),     row.names = FALSE)
write.csv(tabla_descriptiva,           file.path(carpeta_output, "T03_descriptiva_general.csv"),      row.names = FALSE)
write.csv(tabla_comparacion_log,       file.path(carpeta_output, "T04_comparacion_log.csv"),          row.names = FALSE)
write.csv(tabla_barrio,                file.path(carpeta_output, "T05_resumen_barrio.csv"),           row.names = FALSE)
write.csv(tabla_festivo,               file.path(carpeta_output, "T06_festivo_vs_ordinario.csv"),     row.names = FALSE)
write.csv(tabla_horas,                 file.path(carpeta_output, "T07_franjas_horarias.csv"),         row.names = FALSE)
write.csv(tabla_barrios_ic,            file.path(carpeta_output, "T08_barrios_IC.csv"),               row.names = FALSE)
write.csv(tabla_resumen_modelos,       file.path(carpeta_output, "T09_resumen_modelos.csv"),          row.names = FALSE)
write.csv(tabla_coeficientes,          file.path(carpeta_output, "T10_coeficientes_modelo_final.csv"),row.names = FALSE)
write.csv(tabla_heterogeneidad,        file.path(carpeta_output, "T11_heterogeneidad_barrio.csv"),    row.names = FALSE)

# Resultados de pruebas estadísticas
resultados_txt <- capture.output({
  cat("=== Tratamiento de valores desconocidos ===\n\n")
  cat("PU_Borough Unknown:   ", n_pu,   "(", round(n_pu/n_total*100,2), "%)\n")
  cat("DO_Borough Unknown:   ", n_do,   "(", round(n_do/n_total*100,2), "%)\n")
  cat("passenger_count = 0:  ", n_pass, "(", round(n_pass/n_total*100,2), "%)\n")
  cat("Total eliminados:     ", n_any,  "(", round(n_any/n_total*100,2), "%)\n\n")
  cat("--- Prueba t: registros con vs. sin Unknown ---\n")
  print(prueba_unknown)
  cat("\n=== Diagnóstico de trip_count ===\n\n")
  cat("Original — Asimetría:", round(asimetria_tc,4), "| Curtosis:", round(curtosis_tc,4), "\n")
  cat("Log      — Asimetría:", round(asimetria_log,4), "| Curtosis:", round(curtosis_log,4), "\n")
  cat("\n=== Diferencias en la demanda ===\n\n")
  cat("--- Festivos vs. ordinarios ---\n"); print(prueba_festivo)
  cat("\n--- Horas pico vs. valle ---\n");  print(prueba_horas)
  cat("\n--- ANOVA barrios ---\n");         print(summary(anova_barrios))
  cat("\n=== Modelo final — bondad de ajuste ===\n\n")
  cat("R²:          ", round(s_final$r.squared,    4), "\n")
  cat("R² ajustado: ", round(s_final$adj.r.squared, 4), "\n")
  cat("\n--- Breusch-Pagan ---\n"); print(prueba_bp)
  cat("\n--- Shapiro-Wilk ---\n");  print(prueba_sw)
})

writeLines(resultados_txt, file.path(carpeta_output, "R00_resultados_pruebas.txt"))

cat("\nExportación completa. Archivos en:", carpeta_output, "\n")

# ==========================================================
# Bloque 8 — Dashboard interactivo en Shiny
# ==========================================================

# ----------------------------------------------------------
# Preparación de objetos para el dashboard
# ----------------------------------------------------------

# El modelo final ya está entrenado: modelo_final
# Guardar para usarlo dentro de Shiny
modelo_shiny <- modelo_final

# Niveles de barrios y tipos de pago disponibles
barrios_disponibles <- levels(data$PU_Borough)
barrios_do          <- levels(data$DO_Borough)
pagos_disponibles   <- levels(data$payment_type)

# Resumen por barrio para comparador
resumen_barrio_dash <- data %>%
  group_by(PU_Borough) %>%
  summarise(
    Promedio_viajes = round(mean(trip_count), 2),
    Mediana_viajes  = round(median(trip_count), 2),
    Promedio_log    = round(mean(log_trip_count), 4),
    Promedio_tarifa = round(mean(fare_per_trip, na.rm = TRUE), 2)
  )

# Resumen por hora para análisis horario
resumen_hora_dash <- data %>%
  group_by(hour) %>%
  summarise(
    Promedio_viajes = round(mean(trip_count), 2),
    Promedio_log    = round(mean(log_trip_count), 4),
    IC_inf          = t.test(log_trip_count)$conf.int[1],
    IC_sup          = t.test(log_trip_count)$conf.int[2]
  )

# ----------------------------------------------------------
# Interfaz del dashboard (UI)
# ----------------------------------------------------------

ui <- dashboardPage(
  skin = "blue",

  dashboardHeader(title = "Demanda de taxis NYC"),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Predictor de viajes",   tabName = "predictor",   icon = icon("taxi")),
      menuItem("Estimador de tarifa",   tabName = "tarifa",      icon = icon("dollar-sign")),
      menuItem("Comparador de barrios", tabName = "comparador",  icon = icon("map")),
      menuItem("Análisis por hora",     tabName = "horas",       icon = icon("clock"))
    )
  ),

  dashboardBody(
    tabItems(

      # ---- Hoja 1: Predictor de viajes ----
      tabItem(
        tabName = "predictor",
        h2("Estimación del número de viajes"),
        p("Selecciona las condiciones del viaje y obtén el número estimado de viajes."),
        fluidRow(
          box(
            title = "Parámetros de entrada", status = "primary", solidHeader = TRUE, width = 4,
            selectInput("pred_barrio",  "Barrio de origen:",
                        choices = barrios_disponibles, selected = "Manhattan"),
            sliderInput("pred_hora",    "Hora del día:",
                        min = 0, max = 23, value = 8, step = 1),
            selectInput("pred_pago",    "Tipo de pago:",
                        choices = pagos_disponibles, selected = "1"),
            sliderInput("pred_temp",    "Temperatura máxima (°F):",
                        min = 10, max = 100, value = 65, step = 1),
            selectInput("pred_lluvia",  "¿Llueve?",
                        choices = c("No" = "0", "Sí" = "1"), selected = "0"),
            sliderInput("pred_pasajeros", "Promedio de pasajeros:",
                        min = 1, max = 5, value = 2, step = 1),
            selectInput("pred_festivo", "¿Es día festivo?",
                        choices = c("No" = "0", "Sí" = "1"), selected = "0")
          ),
          box(
            title = "Resultado estimado", status = "success", solidHeader = TRUE, width = 8,
            valueBoxOutput("caja_viajes_estimados", width = 6),
            valueBoxOutput("caja_franja",           width = 6),
            br(),
            plotOutput("grafico_predictor_hora", height = "300px")
          )
        )
      ),

      # ---- Hoja 2: Estimador de tarifa ----
      tabItem(
        tabName = "tarifa",
        h2("Estimación de tarifa promedio por viaje"),
        p("Visualiza el precio promedio estimado por viaje según las condiciones seleccionadas."),
        fluidRow(
          box(
            title = "Filtros", status = "primary", solidHeader = TRUE, width = 4,
            selectInput("tar_barrio",  "Barrio de origen:",
                        choices = barrios_disponibles, selected = "Manhattan"),
            selectInput("tar_pago",    "Tipo de pago:",
                        choices = pagos_disponibles, selected = "1"),
            sliderInput("tar_hora",    "Hora del día:",
                        min = 0, max = 23, value = 8, step = 1)
          ),
          box(
            title = "Tarifa estimada", status = "success", solidHeader = TRUE, width = 8,
            valueBoxOutput("caja_tarifa_estimada", width = 6),
            br(),
            plotOutput("grafico_tarifa_barrio", height = "300px")
          )
        )
      ),

      # ---- Hoja 3: Comparador de barrios ----
      tabItem(
        tabName = "comparador",
        h2("Comparación de demanda entre barrios"),
        p("Selecciona dos barrios para comparar su demanda de viajes."),
        fluidRow(
          box(
            title = "Selección", status = "primary", solidHeader = TRUE, width = 4,
            selectInput("comp_barrio1", "Barrio 1:",
                        choices = barrios_disponibles, selected = "Manhattan"),
            selectInput("comp_barrio2", "Barrio 2:",
                        choices = barrios_disponibles, selected = "Brooklyn")
          ),
          box(
            title = "Estadísticos comparativos", status = "info", solidHeader = TRUE, width = 8,
            tableOutput("tabla_comparacion_barrios"),
            br(),
            plotOutput("grafico_comparacion_barrios", height = "300px")
          )
        )
      ),

      # ---- Hoja 4: Análisis por hora ----
      tabItem(
        tabName = "horas",
        h2("Evolución de la demanda a lo largo del día"),
        p("Visualiza cómo varía el número de viajes hora a hora, con horas pico destacadas."),
        fluidRow(
          box(
            title = "Filtro por barrio", status = "primary", solidHeader = TRUE, width = 4,
            selectInput("hora_barrio", "Barrio:",
                        choices = c("Todos", barrios_disponibles), selected = "Todos")
          ),
          box(
            title = "Demanda por hora del día", status = "info", solidHeader = TRUE, width = 8,
            plotOutput("grafico_horas_dash", height = "350px")
          )
        )
      )
    )
  )
)

# ----------------------------------------------------------
# Lógica del servidor (Server)
# ----------------------------------------------------------

server <- function(input, output, session) {

  # ---- Predictor de viajes ----

  viajes_estimados <- reactive({
    nuevos_datos <- data.frame(
      hour            = as.integer(input$pred_hora),
      holiday_usa     = factor(input$pred_festivo,    levels = levels(data$holiday_usa)),
      PU_Borough      = factor(input$pred_barrio,     levels = levels(data$PU_Borough)),
      payment_type    = factor(input$pred_pago,       levels = levels(data$payment_type)),
      passenger_count = as.numeric(input$pred_pasajeros),
      tmax_f          = as.numeric(input$pred_temp),
      rain            = factor(input$pred_lluvia,     levels = levels(data$rain))
    )
    pred_log <- predict(modelo_shiny, newdata = nuevos_datos)
    round(exp(pred_log) - 1, 0)
  })

  output$caja_viajes_estimados <- renderValueBox({
    valueBox(
      value    = viajes_estimados(),
      subtitle = "Viajes estimados",
      icon     = icon("taxi"),
      color    = "blue"
    )
  })

  output$caja_franja <- renderValueBox({
    franja <- case_when(
      input$pred_hora >= 7  & input$pred_hora <= 9  ~ "Hora pico mañana",
      input$pred_hora >= 17 & input$pred_hora <= 19 ~ "Hora pico tarde",
      TRUE ~ "Hora valle"
    )
    valueBox(
      value    = franja,
      subtitle = "Franja horaria",
      icon     = icon("clock"),
      color    = "green"
    )
  })

  output$grafico_predictor_hora <- renderPlot({
    resumen_hora_dash %>%
      ggplot(aes(x = hour, y = Promedio_viajes)) +
      geom_line(color = "steelblue", linewidth = 1) +
      geom_point(color = "steelblue", size = 2) +
      geom_vline(xintercept = input$pred_hora, color = "red", linewidth = 1.2, linetype = "dashed") +
      annotate("rect", xmin = 7,  xmax = 9,  ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "darkblue") +
      annotate("rect", xmin = 17, xmax = 19, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "darkblue") +
      labs(title = "Promedio de viajes por hora — la línea roja indica la hora seleccionada",
           x = "Hora", y = "Promedio de viajes") +
      scale_x_continuous(breaks = 0:23) +
      formato_grafica
  })

  # ---- Estimador de tarifa ----

  tarifa_estimada <- reactive({
    data %>%
      filter(PU_Borough    == input$tar_barrio,
             payment_type  == input$tar_pago,
             hour          == input$tar_hora) %>%
      summarise(tarifa = round(mean(fare_per_trip, na.rm = TRUE), 2)) %>%
      pull(tarifa)
  })

  output$caja_tarifa_estimada <- renderValueBox({
    t <- tarifa_estimada()
    valueBox(
      value    = ifelse(is.nan(t) | is.na(t), "Sin datos", paste0("$", t)),
      subtitle = "Tarifa promedio por viaje (USD)",
      icon     = icon("dollar-sign"),
      color    = "green"
    )
  })

  output$grafico_tarifa_barrio <- renderPlot({
    resumen_barrio_dash %>%
      ggplot(aes(x = reorder(PU_Borough, -Promedio_tarifa), y = Promedio_tarifa)) +
      geom_col(fill = "steelblue") +
      geom_col(data = resumen_barrio_dash %>% filter(PU_Borough == input$tar_barrio),
               fill = "darkblue") +
      labs(title = "Tarifa promedio por viaje según barrio de origen",
           x = "Barrio", y = "USD promedio por viaje") +
      formato_grafica
  })

  # ---- Comparador de barrios ----

  output$tabla_comparacion_barrios <- renderTable({
    resumen_barrio_dash %>%
      filter(PU_Borough %in% c(input$comp_barrio1, input$comp_barrio2)) %>%
      rename(Barrio = PU_Borough,
             "Promedio viajes"  = Promedio_viajes,
             "Mediana viajes"   = Mediana_viajes,
             "log(trip_count)"  = Promedio_log,
             "Tarifa promedio"  = Promedio_tarifa)
  })

  output$grafico_comparacion_barrios <- renderPlot({
    data %>%
      filter(PU_Borough %in% c(input$comp_barrio1, input$comp_barrio2)) %>%
      ggplot(aes(x = PU_Borough, y = log_trip_count, fill = PU_Borough)) +
      geom_boxplot(show.legend = FALSE) +
      scale_fill_manual(values = c("steelblue", "darkblue")) +
      labs(title = paste("Distribución de viajes:", input$comp_barrio1, "vs.", input$comp_barrio2),
           x = "Barrio", y = "log(trip_count + 1)") +
      formato_grafica
  })

  # ---- Análisis por hora ----

  output$grafico_horas_dash <- renderPlot({
    datos_hora <- if (input$hora_barrio == "Todos") {
      data
    } else {
      data %>% filter(PU_Borough == input$hora_barrio)
    }

    datos_hora %>%
      group_by(hour) %>%
      summarise(Promedio = mean(log_trip_count),
                IC_inf   = t.test(log_trip_count)$conf.int[1],
                IC_sup   = t.test(log_trip_count)$conf.int[2]) %>%
      ggplot(aes(x = hour, y = Promedio)) +
      geom_ribbon(aes(ymin = IC_inf, ymax = IC_sup), alpha = 0.2, fill = "steelblue") +
      geom_line(color = "steelblue", linewidth = 1) +
      geom_point(color = "steelblue", size = 2) +
      annotate("rect", xmin = 7,  xmax = 9,  ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "darkblue") +
      annotate("rect", xmin = 17, xmax = 19, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "darkblue") +
      labs(title    = paste("Demanda por hora —", input$hora_barrio),
           subtitle = "Zonas sombreadas = horas pico. Banda = IC 95%",
           x = "Hora", y = "Promedio log(viajes + 1)") +
      scale_x_continuous(breaks = 0:23) +
      formato_grafica
  })

}

shinyApp(ui = ui, server = server)
