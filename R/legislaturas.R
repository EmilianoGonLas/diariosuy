# R/legislaturas.R
# Constantes del paquete: legislaturas disponibles y topes de uso.

#' Legislaturas disponibles en el buscador del Parlamento
#'
#' Tabla con las legislaturas que se pueden consultar, con el rango de fechas
#' que el buscador oficial espera para cada una.
#'
#' Los diarios de sesión están disponibles en línea a partir del 15/02/1985
#' (inicio de la XLII, restauración democrática). Las legislaturas XL y XLI son
#' previas al golpe de Estado de 1973: pueden tener documentos limitados o
#' ninguno según el cuerpo que se busque.
#'
#' @format Un tibble con una fila por legislatura y las columnas `etiqueta`,
#'   `lgl_id`, `fecha_desde` y `fecha_hasta`.
#' @export
LEGISLATURAS <- tibble::tribble(
  ~etiqueta,                          ~lgl_id, ~fecha_desde,  ~fecha_hasta,
  "L — 2025 al presente",             50, "2025-02-15", "2030-02-14",
  "XLIX — 2020 a 2025",               49, "2020-02-15", "2025-02-14",
  "XLVIII — 2015 a 2020",             48, "2015-02-15", "2020-02-14",
  "XLVII — 2010 a 2015",              47, "2010-02-15", "2015-02-14",
  "XLVI — 2005 a 2010",               46, "2005-02-15", "2010-02-14",
  "XLV — 2000 a 2005",                45, "2000-02-15", "2005-02-14",
  "XLIV — 1995 a 2000",               44, "1995-02-15", "2000-02-14",
  "XLIII — 1990 a 1995",              43, "1990-02-15", "1995-02-14",
  "XLII — 1985 a 1990",               42, "1985-02-15", "1990-02-14",
  "XLI — 1972 a 1973",                41, "1972-02-15", "1973-06-27",
  "XL — 1967 a 1972",                 40, "1967-02-15", "1972-02-14"
)

# ── Entorno de ejecución ──────────────────────────────────────────────────────
# El paquete está pensado para correr en la máquina del usuario, que es lo que
# permite llegar a parlamento.gub.uy (el sitio rechaza las IP de fuera de
# Uruguay). Igual conservamos la detección de shinyapps.io por si la app se
# publica en un servidor: ahí el analizador acota volumen para no agotar la RAM.
ES_LOCAL <- !nzchar(Sys.getenv("SHINYAPPS_APPLICATION_ID"))

# Topes que sólo aplican cuando la app corre publicada, no en local.
MAX_PDFS_WEB <- 40L    # cantidad máxima de PDFs a analizar
MAX_MB_WEB   <- 150L   # peso total máximo (MB) a analizar
