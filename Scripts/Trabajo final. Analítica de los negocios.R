# ==========================================================
# Analítica de los negocios — Trabajo final
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