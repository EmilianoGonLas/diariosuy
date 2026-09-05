# R/downloader.R
# Funciones para descargar PDFs de sesiones parlamentarias y empaquetar en ZIP.

# User-Agent compartido para todas las peticiones HTTP
.UA <- paste0(
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) ",
  "AppleWebKit/537.36 (KHTML, like Gecko) ",
  "Chrome/124.0.0.0 Safari/537.36"
)

#' Descarga PDFs de sesiones seleccionadas a un directorio local
#'
#' Cada PDF se nombra con el patrón: `Lgl_[LEGISLATURA]_[FECHA]_ID_[ID].pdf`
#' Si el archivo ya existe, lo omite sin re-descargarlo.
#'
#' @param sesiones        tibble con columnas: legislatura, fecha, id_doc, url_intermedia
#' @param directorio      Carpeta destino (se crea si no existe)
#' @param progreso_fn     Función opcional: function(valor, mensaje) para reportar progreso
#'
#' @return tibble con columnas: archivo, estado (resultado por cada PDF)
#' @export
descargar_sesiones <- function(sesiones, directorio, progreso_fn = NULL) {
  fs::dir_create(directorio)

  purrr::map_dfr(seq_len(nrow(sesiones)), function(i) {
    fila <- sesiones[i, ]

    if (!is.null(progreso_fn)) {
      progreso_fn(
        valor   = i / nrow(sesiones),
        mensaje = sprintf("Sesión %d de %d: %s", i, nrow(sesiones), fila$fecha)
      )
    }

    # Obtener URL directa del PDF (requiere una petición HTTP adicional)
    url_pdf <- obtener_url_pdf(fila$url_intermedia)

    if (is.na(url_pdf) || length(url_pdf) == 0) {
      return(tibble::tibble(
        archivo = NA_character_,
        estado  = paste("Error: no se encontró PDF para sesión", fila$id_doc)
      ))
    }

    # Asegurarse de que la URL sea absoluta
    if (!grepl("^https?://", url_pdf)) {
      url_pdf <- paste0("https://parlamento.gub.uy", url_pdf)
    }

    # Extraer solo el número romano de la etiqueta (ej: "XLIX — 2020 a 2025" -> "XLIX")
    lgl_romano <- stringr::str_extract(fila$legislatura, "^[A-Z]+")

    # Etiqueta legible del cuerpo para el nombre del archivo
    cuerpo_lbl <- switch(fila$cuerpo,
      "A"   = "AsambleaGeneral",
      "C"   = "ComisionPermanente",
      "R"   = "Representantes",
      "S"   = "Senadores",
      ""    # "All" u otro: sin etiqueta de cuerpo
    )

    safe_fecha <- gsub("[\\/:*?\"<>|]", "-", as.character(fila$fecha))
    nombre <- if (nzchar(cuerpo_lbl)) {
      sprintf("Lgl_%s_%s_%s_ID_%s.pdf", lgl_romano, cuerpo_lbl, safe_fecha, fila$id_doc)
    } else {
      sprintf("Lgl_%s_%s_ID_%s.pdf", lgl_romano, safe_fecha, fila$id_doc)
    }
    destino <- file.path(directorio, nombre)

    if (fs::file_exists(destino)) {
      return(tibble::tibble(archivo = nombre, estado = "Ya existía (omitido)"))
    }

    # Descargar con httr para poder enviar User-Agent y manejar errores correctamente
    ok <- tryCatch({
      resp <- httr::GET(
        url_pdf,
        httr::add_headers(`User-Agent` = .UA),
        httr::write_disk(destino, overwrite = TRUE),
        httr::timeout(60)
      )
      # Si el servidor devolvió un error HTTP, write_disk igual guarda el HTML de error
      # como .pdf — hay que borrarlo para no contaminar el ZIP
      if (httr::http_error(resp)) {
        if (fs::file_exists(destino)) fs::file_delete(destino)
        FALSE
      } else {
        TRUE
      }
    }, error = function(e) {
      if (fs::file_exists(destino)) fs::file_delete(destino)
      FALSE
    })

    Sys.sleep(1)  # pausa entre descargas

    tibble::tibble(
      archivo = nombre,
      estado  = if (ok) "Descargado correctamente" else "Error al descargar"
    )
  })
}

#' Empaqueta todos los PDFs de una carpeta en un archivo ZIP
#'
#' Usa nombres de archivo relativos para garantizar que el ZIP se pueda
#' abrir correctamente en cualquier sistema operativo.
#'
#' @param directorio Carpeta con los PDFs a empaquetar
#' @param destino    Ruta del archivo ZIP de salida
#' @return TRUE si el ZIP se creó correctamente, FALSE si no había PDFs
#' @export
crear_zip <- function(directorio, destino) {
  archivos_rel <- list.files(directorio, pattern = "\\.pdf$", full.names = FALSE)
  if (length(archivos_rel) == 0) return(FALSE)

  # Normalizar el destino ANTES de cambiar el directorio de trabajo
  destino_abs <- normalizePath(destino, mustWork = FALSE, winslash = "/")

  # Cambiar al directorio de los PDFs para que el ZIP use rutas relativas
  dir_anterior <- getwd()
  on.exit(setwd(dir_anterior), add = TRUE)
  setwd(directorio)

  zip::zip(zipfile = destino_abs, files = archivos_rel)
  TRUE
}
