# global.R
# Cargado automáticamente por Shiny antes de ui.R y server.R

# ── Detectar entorno (debe ir ANTES de cargar paquetes) ───────────────────────
# shinyapps.io setea SHINYAPPS_APPLICATION_ID automáticamente.
# En local esa variable no existe, así que ES_LOCAL = TRUE.
ES_LOCAL <- !nzchar(Sys.getenv("SHINYAPPS_APPLICATION_ID"))

# ── Límites del analizador en la nube ─────────────────────────────────────────
# El análisis carga y parsea los PDFs en memoria. En shinyapps.io (RAM limitada)
# un volumen grande —p. ej. cientos de MB— satura la instancia y la tira abajo.
# Por eso en la nube acotamos cuántos PDFs y cuánto peso total se analizan de una;
# para volúmenes mayores, la app se corre en local (sin límites). En local no aplica.
MAX_PDFS_WEB <- 40L    # cantidad máxima de PDFs a analizar online
MAX_MB_WEB   <- 150L   # peso total máximo (MB) a analizar online

# ── Instalación automática de paquetes (solo en local) ────────────────────────
# En shinyapps.io los paquetes van bundleados durante el deploy,
# por lo que install.packages() no es necesario ni deseable allí.
if (ES_LOCAL) {
  .paquetes <- c(
    "shiny", "bslib", "DT", "plotly",
    "rvest", "httr", "pdftools", "dplyr", "ggplot2",
    "stringr", "purrr", "tidyr", "fs", "zip",
    "shinyFiles"
  )
  .faltantes <- .paquetes[!.paquetes %in% installed.packages()[, "Package"]]
  if (length(.faltantes) > 0) {
    message("\n── Instalando paquetes faltantes ──────────────────────────")
    message("   ", paste(.faltantes, collapse = ", "))
    message("   Esto puede tardar unos minutos la primera vez...\n")
    install.packages(.faltantes, dependencies = TRUE)
  }
  rm(.paquetes, .faltantes)
}

# ── Cargar paquetes ────────────────────────────────────────────────────────────
library(shiny)
library(bslib)
library(DT)
library(plotly)
library(rvest)
library(httr)
library(pdftools)
library(dplyr)
library(ggplot2)
library(stringr)
library(purrr)
library(tidyr)
library(fs)
library(zip)
# shinyFiles solo se usa para el selector de carpeta en modo local;
# en la nube no hace falta (allí se suben archivos, no se elige carpeta).
if (ES_LOCAL) library(shinyFiles)

source("R/scraper.R")
source("R/downloader.R")
source("R/analizar.R")
source("R/visualizar.R")

# Tabla de legislaturas disponibles en el parlamento uruguayo.
# Nota: los diarios de sesión de la Asamblea General están disponibles en línea
# a partir del 15/02/1985 (inicio de la XLII legislatura, restauración democrática).
# Las legislaturas XL y XLI corresponden al período previo al golpe de Estado
# de 1973; pueden tener documentos limitados o ninguno según el cuerpo buscado.
LEGISLATURAS <- tibble::tribble(
  ~etiqueta,                          ~lgl_id, ~fecha_desde,  ~fecha_hasta,
  "L — 2025 al presente",                 50, "2025-02-15", "2030-02-14",
  "XLIX — 2020 a 2025",                  49, "2020-02-15", "2025-02-14",
  "XLVIII — 2015 a 2020",               48, "2015-02-15", "2020-02-14",
  "XLVII — 2010 a 2015",                47, "2010-02-15", "2015-02-14",
  "XLVI — 2005 a 2010",                 46, "2005-02-15", "2010-02-14",
  "XLV — 2000 a 2005",                  45, "2000-02-15", "2005-02-14",
  "XLIV — 1995 a 2000",                 44, "1995-02-15", "2000-02-14",
  "XLIII — 1990 a 1995",               43, "1990-02-15", "1995-02-14",
  "XLII — 1985 a 1990",                42, "1985-02-15", "1990-02-14",
  "XLI — 1972 a 1973",                 41, "1972-02-15", "1973-06-27",
  "XL — 1967 a 1972",                  40, "1967-02-15", "1972-02-14"
)
