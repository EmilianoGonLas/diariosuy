#' @keywords internal
"_PACKAGE"

#' @import shiny
#' @import dplyr
#' @import ggplot2
#' @importFrom stringr str_count str_detect str_extract str_match str_remove
#' @importFrom stringr str_replace_all str_to_lower str_to_title
#' @importFrom rlang .data %||%
#' @importFrom utils write.csv installed.packages browseURL
#' @importFrom stats median setNames
NULL

# Silencia las notas de R CMD check por las variables que dplyr/ggplot evalúan
# sin comillas dentro de las funciones del paquete.
utils::globalVariables(c(
  "lgl_id", "fecha", "url_intermedia", "id_doc", "legislatura", "cuerpo",
  "etiqueta", "fecha_desde", "fecha_hasta", "archivo", "estado", "texto",
  "termino", "n", "freq", "pagina", "sesion", "valor",
  # Columnas que produce el analizador
  "paginas", "paginas_con_termino", "fragmentos_texto", "total_cruces", "ruta",
  "score_raw", "es_duplicado", "eje",
  # Nombres de columna que ve el usuario en la tabla de resultados
  " ", "Documento", "PDF", "Relevancia", "Ver", "Ver frases",
  # Marcador de posición de magrittr en obtener_url_pdf()
  "."
))
