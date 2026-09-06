# R/analizar.R
# Análisis de texto sobre PDFs de diarios de sesión parlamentaria.
# Implementa detección de menciones y co-ocurrencias por página y por proximidad.

# ── Helpers internos ────────────────────────────────────────────────────────────

#' Cuenta cuántas menciones de `patron1` tienen a `patron2` dentro de
#' `ventana` caracteres en el mismo texto (co-ocurrencia de proximidad).
#'
#' @param texto   Texto normalizado (minúsculas, espacios simples)
#' @param patron1 Regex del término principal
#' @param patron2 Regex del término de cruce
#' @param ventana Máx. distancia en caracteres para considerar "cercano" (default 500)
#' @return Entero: cuántas menciones de patron1 tienen a patron2 "cerca"
.contar_proximos <- function(texto, patron1, patron2, ventana = 500L) {
  pos1 <- stringr::str_locate_all(texto, patron1)[[1]]
  if (nrow(pos1) == 0L) return(0L)
  pos2 <- stringr::str_locate_all(texto, patron2)[[1]]
  if (nrow(pos2) == 0L) return(0L)
  sum(vapply(pos1[, "start"], function(c1) {
    any(abs(pos2[, "start"] - c1) <= ventana)
  }, logical(1L)))
}

#' Extrae texto de un PDF respetando el orden de lectura en documentos a dos columnas.
#'
#' Usa pdftools::pdf_data() para obtener las coordenadas de cada palabra y
#' reconstruir el texto leyendo primero la columna izquierda completa y luego
#' la derecha, en lugar de ir línea a línea de izquierda a derecha.
#' Si pdf_data() falla, cae en pdf_text() como fallback.
#'
#' @param ruta Ruta al archivo PDF
#' @return Character vector con el texto de cada página (largo = n° de páginas)
.extraer_texto_pdf <- function(ruta) {
  datos <- tryCatch(pdftools::pdf_data(ruta), error = function(e) NULL)
  if (is.null(datos)) return(pdftools::pdf_text(ruta))

  purrr::map_chr(datos, function(pag) {
    if (nrow(pag) == 0L) return("")

    xmin  <- min(pag$x,            na.rm = TRUE)
    xmax  <- max(pag$x + pag$width, na.rm = TRUE)
    ancho <- xmax - xmin

    # Página demasiado estrecha o sin palabras → una columna
    if (!is.finite(ancho) || ancho < 10) return(.reconstruir_columna(pag))

    # ── Detectar columnas con un histograma de posiciones x ──────────
    n_bins  <- 40L
    breaks  <- seq(xmin, xmax, length.out = n_bins + 1L)
    idx     <- pmax(1L, pmin(findInterval(pag$x, breaks, rightmost.closed = TRUE), n_bins))
    conteos <- tabulate(idx, nbins = n_bins)
    mids    <- (breaks[-1L] + breaks[-(n_bins + 1L)]) / 2L

    # Buscar un gap claro en la zona central de la página (25 %–75 %)
    zona <- mids >= xmin + 0.25 * ancho & mids <= xmin + 0.75 * ancho
    limite_col <- NULL
    if (sum(zona) >= 3L) {
      c_z <- conteos[zona]; m_z <- mids[zona]
      avg_c <- mean(c_z)
      # Gap confirmado: algún bin tiene <20 % del promedio de palabras/bin
      if (avg_c > 0 && min(c_z) < avg_c * 0.20) {
        limite_col <- m_z[which.min(c_z)]
      }
    }

    if (!is.null(limite_col)) {
      # Dos columnas: reconstruir cada una por separado y concatenar
      paste(
        .reconstruir_columna(pag[pag$x <  limite_col, ]),
        .reconstruir_columna(pag[pag$x >= limite_col, ])
      )
    } else {
      .reconstruir_columna(pag)
    }
  })
}

#' Reconstruye el texto de una columna agrupando palabras en líneas.
#'
#' Ordena por (y, x), detecta saltos de línea por el Δy entre palabras
#' consecutivas y une las palabras de cada línea con espacios.
#'
#' @param datos data.frame con columnas x, y, width, height, text (de pdf_data())
#' @return String con el texto de la columna en orden de lectura correcto
.reconstruir_columna <- function(datos) {
  if (nrow(datos) == 0L) return("")

  datos  <- datos[order(datos$y, datos$x), ]
  h_med  <- max(stats::median(datos$height, na.rm = TRUE) * 0.6, 2.0)
  dy     <- c(Inf, abs(diff(datos$y)))   # Inf fuerza nueva línea al inicio
  datos$ln <- cumsum(dy > h_med)

  partes <- vapply(split(datos[c("x", "text")], datos$ln), function(lin) {
    paste(lin$text[order(lin$x)], collapse = " ")
  }, character(1L))

  paste(partes, collapse = " ")
}

#' Extrae un fragmento de texto que empiece al inicio de una oración.
#'
#' Busca el último signo de puntuación en los `max_pre` caracteres anteriores
#' al match y empieza allí. Si no hay ninguno, toma desde el límite.
#' Cierra en el primer punto dentro de los `max_total` caracteres siguientes.
#'
#' @param texto     Texto normalizado
#' @param s,e       Inicio y fin del match a destacar
#' @param max_pre   Máx. chars a buscar hacia atrás para inicio de oración (default 90)
#' @param max_total Longitud máxima del fragmento (default 230)
#' @return Lista con: ws (índice inicio), we (índice fin), text (el fragmento)
.extraer_fragmento <- function(texto, s, e, max_pre = 90L, max_total = 230L) {
  n         <- nchar(texto)
  look_back <- max(1L, s - max_pre)
  sub_prev  <- stringr::str_sub(texto, look_back, s - 1L)

  # Inicio: justo después del último fin de oración en los max_pre chars anteriores
  puntos <- stringr::str_locate_all(sub_prev, "[.!?][ ]+")[[1]]
  ws <- if (nrow(puntos) > 0L) {
    min(look_back + puntos[nrow(puntos), "end"], s)
  } else {
    look_back
  }

  # Fin: primer fin de oración dentro de max_total chars (hard cap)
  we_max   <- min(n, ws + max_total)
  sub_next <- stringr::str_sub(texto, e + 1L, we_max)
  puntos2  <- stringr::str_locate_all(sub_next, "[.!?]")[[1]]
  we <- if (nrow(puntos2) > 0L) {
    min(e + puntos2[1L, "start"], we_max)
  } else {
    we_max
  }

  list(ws = ws, we = we, text = stringr::str_sub(texto, ws, we))
}

#' Extrae un ZIP a una carpeta temporal y devuelve su ruta
#'
#' Se usa para poder analizar directamente el ZIP que arma el Buscador, sin
#' pedirle a quien lo usa que lo descomprima antes.
#'
#' @param ruta_zip Ruta al archivo .zip
#' @return Ruta de la carpeta temporal donde quedó el contenido.
#' @keywords internal
.extraer_zip <- function(ruta_zip) {
  destino <- file.path(
    tempdir(),
    paste0("zip_", tools::file_path_sans_ext(basename(ruta_zip)), "_",
           as.integer(Sys.time()))
  )
  fs::dir_create(destino)
  zip::unzip(ruta_zip, exdir = destino)
  destino
}

#' Analiza PDFs buscando un término principal y co-ocurrencias opcionales
#'
#' @param directorio        Carpeta con los PDFs a analizar. También acepta la
#'   ruta de un archivo `.zip`: en ese caso se extrae a una carpeta temporal y
#'   se analiza su contenido, así el ZIP que arma el Buscador se puede usar tal
#'   cual. La búsqueda de PDFs es recursiva, de modo que no importa si quedaron
#'   dentro de subcarpetas.
#' @param termino_principal Expresión regular del término principal
#' @param terminos_cruce    Lista nombrada: list(turismo = "turist|extranjer")
#' @param progreso_fn       function(valor, mensaje) para reportar progreso
#'
#' @return tibble con columnas: archivo, ruta, legislatura, fecha, id_doc,
#'   paginas_con_termino, `cruce_[nombre]`, total_cruces, puntuacion,
#'   fragmentos_texto, estado
#' @export
analizar_pdfs <- function(directorio,
                           termino_principal,
                           terminos_cruce = list(),
                           progreso_fn    = NULL) {

  # El texto de las páginas se pasa a minúsculas antes de buscar, así que el
  # patrón tiene que ignorar mayúsculas o no encuentra nada: escribir "Cannabis"
  # daba cero menciones en documentos que sí hablaban del tema.
  #
  # Se marca el patrón como insensible a mayúsculas en vez de convertirlo con
  # str_to_lower(): el término es una expresión regular, y bajarlo de caso
  # cambiaría el significado de las clases escritas en mayúscula (\\S deja de ser
  # "no espacio" y pasa a ser "espacio").
  termino_principal <- stringr::regex(termino_principal, ignore_case = TRUE)
  if (length(terminos_cruce) > 0) {
    terminos_cruce <- lapply(terminos_cruce, stringr::regex, ignore_case = TRUE)
  }

  # Un ZIP se extrae y se analiza la carpeta resultante.
  if (length(directorio) == 1 &&
      !dir.exists(directorio) &&
      grepl("\\.zip$", directorio, ignore.case = TRUE)) {
    if (!file.exists(directorio)) {
      message("No existe el archivo: ", directorio)
      return(tibble::tibble())
    }
    directorio <- tryCatch(.extraer_zip(directorio), error = function(e) {
      message("No se pudo abrir el ZIP: ", conditionMessage(e))
      NULL
    })
    if (is.null(directorio)) return(tibble::tibble())
  }

  # recurse = TRUE porque al descomprimir los PDFs suelen quedar dentro de una
  # subcarpeta con el nombre del ZIP.
  archivos <- fs::dir_ls(directorio, glob = "*.pdf", recurse = TRUE)

  if (length(archivos) == 0) {
    message("No se encontraron PDFs en: ", directorio)
    return(tibble::tibble())
  }

  purrr::map_dfr(seq_along(archivos), function(i) {
    ruta <- archivos[i]

    if (!is.null(progreso_fn)) {
      progreso_fn(
        valor   = i / length(archivos),
        mensaje = sprintf("Analizando %d de %d: %s", i, length(archivos), basename(ruta))
      )
    }

    tryCatch({
      # .extraer_texto_pdf() usa pdf_data() con coordenadas de palabras para
      # reconstruir el orden correcto en documentos a dos columnas.
      # Si falla, cae automáticamente en pdf_text().
      paginas <- .extraer_texto_pdf(ruta) %>%
        str_to_lower() %>%
        str_replace_all("\\s+", " ")

      tiene_principal <- str_detect(paginas, termino_principal)

      # ── Parsear metadatos del nombre de archivo ─────────────────────────────
      # Intenta extraer legislatura, fecha e id_doc del nombre.
      # Acepta tanto el formato propio (Lgl_XLVII_2012-03-01_ID_12345.pdf)
      # como archivos con nombres arbitrarios, usando múltiples fallbacks.
      nombre <- basename(ruta)

      # Intento 1a: formato nuevo CON cuerpo
      #   Lgl_XLVII_Representantes_2012-03-01_ID_12345.pdf
      cuerpo_etiquetas <- paste(
        c("AsambleaGeneral", "ComisionPermanente", "Representantes", "Senadores"),
        collapse = "|"
      )
      partes <- stringr::str_match(
        nombre,
        sprintf("^Lgl_([A-Za-z]+)_(%s)_(.+?)_ID_([0-9]+)\\.pdf$", cuerpo_etiquetas)
      )
      if (!is.na(partes[1, 1])) {
        # Formato con cuerpo: grupos 2=lgl, 3=cuerpo, 4=fecha, 5=id
        legislatura_val <- partes[1, 2]
        fecha_str       <- partes[1, 4]
        id_doc_val      <- partes[1, 5]
      } else {
        # Intento 1b: formato sin cuerpo  Lgl_XLVII_2012-03-01_ID_12345.pdf
        partes <- stringr::str_match(nombre, "^Lgl_([A-Za-z]+)_(.+?)_ID_([0-9]+)\\.pdf$")
        legislatura_val <- partes[1, 2]
        fecha_str       <- partes[1, 3]
        id_doc_val      <- partes[1, 4]
      }

      # Fallback legislatura: número romano tras "Lgl_" con cualquier separador
      if (is.na(legislatura_val))
        legislatura_val <- stringr::str_extract(nombre, "(?i)(?<=lgl[_-])[IVXLCDM]+")

      # Fallback fecha: primer patrón de fecha en el nombre
      if (is.na(fecha_str))
        fecha_str <- stringr::str_extract(
          nombre,
          "\\d{4}-\\d{2}-\\d{2}|\\d{2}-\\d{2}-\\d{4}|\\d{2}/\\d{2}/\\d{4}"
        )

      # Fallback id_doc: último grupo de dígitos antes de .pdf
      if (is.na(id_doc_val))
        id_doc_val <- stringr::str_extract(nombre, "[0-9]+(?=\\.pdf$)")

      fecha_val <- tryCatch(
        as.Date(fecha_str, tryFormats = c("%Y-%m-%d", "%d-%m-%Y", "%d/%m/%Y")),
        error = function(e) as.Date(NA)
      )

      fila <- tibble::tibble(
        archivo             = nombre,
        ruta                = as.character(ruta),
        legislatura         = legislatura_val,
        fecha               = fecha_val,
        id_doc              = id_doc_val,
        paginas_con_termino = sum(tiene_principal),
        estado              = "OK"
      )

      # ── Cruces, proximidad y fragmentos ────────────────────────────────────────
      # Por cada tema de cruce:
      #   cruces_count = páginas donde aparecen ambos términos (señal moderada)
      #   prox_count   = veces que el cruce aparece dentro de 500 chars del
      #                  término principal (señal fuerte de co-discusión real)
      # Se extraen hasta MAX_FRAGS fragmentos por tema, cada uno empezando
      # al inicio de la oración donde aparece el término de cruce.

      MAX_FRAGS        <- 5L
      total_cruces     <- 0L
      total_prox       <- 0L
      fragmentos_lista <- c()

      for (nombre_cruce in names(terminos_cruce)) {
        tiene_cruce  <- str_detect(paginas, terminos_cruce[[nombre_cruce]])
        pags_cruce   <- which(tiene_principal & tiene_cruce)
        cruces_count <- length(pags_cruce)

        # Co-ocurrencia de proximidad: para cada página con cruce, contar
        # cuántas menciones del término principal tienen al cruce a ≤500 chars
        prox_count <- sum(vapply(pags_cruce, function(pg) {
          .contar_proximos(paginas[pg], termino_principal,
                           terminos_cruce[[nombre_cruce]])
        }, integer(1L)))

        fila[[paste0("cruce_", nombre_cruce)]] <- cruces_count
        total_cruces <- total_cruces + cruces_count
        total_prox   <- total_prox   + prox_count

        # ── Extraer hasta MAX_FRAGS fragmentos (uno por página con cruce) ──────
        n_frags <- min(MAX_FRAGS, cruces_count)
        for (pg_idx in seq_len(n_frags)) {
          pg         <- pags_cruce[pg_idx]
          txt        <- paginas[pg]
          match_info <- stringr::str_locate(txt, terminos_cruce[[nombre_cruce]])
          if (is.na(match_info[1L, "start"])) next

          s    <- match_info[1L, "start"]
          e    <- match_info[1L, "end"]
          frag <- .extraer_fragmento(txt, s, e)

          # Posición del match dentro del fragmento extraído
          pos_s    <- max(1L, s - frag$ws + 1L)
          pos_e    <- max(pos_s, min(e - frag$ws + 1L, nchar(frag$text)))

          snip_html <- paste0(
            stringr::str_sub(frag$text, 1L,         pos_s - 1L),
            "<b>",
            stringr::str_sub(frag$text, pos_s,       pos_e),
            "</b>",
            stringr::str_sub(frag$text, pos_e + 1L,  nchar(frag$text))
          )

          fragmentos_lista <- c(
            fragmentos_lista,
            paste0(
              '<div class="frase-card">',
                '<div class="frase-meta">',
                  '<span class="frase-tema">', nombre_cruce, '</span>',
                  '<span class="frase-pag">p\u00e1g.\u00a0', pg, '</span>',
                '</div>',
                '<blockquote class="frase-cita">\u201c',
                  trimws(snip_html),
                '\u201d</blockquote>',
              '</div>'
            )
          )
        }
      }

      fila[["total_cruces"]]     <- total_cruces
      fila[["fragmentos_texto"]] <- if (length(fragmentos_lista) > 0)
        paste(fragmentos_lista, collapse = "") else ""

      # ── Puntuación de relevancia (compuesta) ────────────────────────────────
      # Tres señales de distinto peso:
      #   Menciones del término principal:       1 pt / página   (señal base)
      #   Co-ocurrencia de página (mismo papel): 2 pt / página   (señal moderada)
      #   Co-ocurrencia de proximidad (≤500 c):  5 pt / mención  (señal fuerte)
      # La proximidad domina porque indica que los temas se discuten juntos de
      # verdad, no solo que ambos aparecen en algún lugar de la misma página.
      # El máximo se normaliza a estrellas en app.R sobre el conjunto completo.
      fila[["score_raw"]] <- sum(tiene_principal) +
                             (total_cruces * 2L)   +
                             (total_prox   * 5L)

      # Liberar el texto del PDF apenas se termina de procesar: en cuerpos
      # grandes (cientos de páginas) esto evita que la memoria se acumule
      # a lo largo de muchos archivos y tire abajo la instancia en la nube.
      rm(paginas, tiene_principal)
      gc(verbose = FALSE)

      fila

    }, error = function(e) {
      tibble::tibble(
        archivo             = basename(ruta),
        ruta                = as.character(ruta),
        legislatura         = NA_character_,
        fecha               = as.Date(NA),
        id_doc              = NA_character_,
        paginas_con_termino = 0L,
        estado              = paste("ERROR:", conditionMessage(e)),
        total_cruces        = 0L,
        fragmentos_texto    = "",
        score_raw           = 0L
      )
    })
  })
}

#' Convierte score_raw a estrellas visuales (★★★☆☆)
#'
#' @param score_raw  Vector numérico de scores crudos
#' @param max_stars  Número máximo de estrellas (default 5)
#' @return Vector de strings HTML con estrellas
estrellas_html <- function(score_raw, max_stars = 5L) {
  mx <- max(score_raw, na.rm = TRUE)
  if (mx == 0) return(rep(
    paste0('<span style="color:#ccc">', strrep("\u2605", max_stars), "</span>"),
    length(score_raw)
  ))
  nivel <- round((score_raw / mx) * max_stars)
  nivel <- pmax(nivel, 0L)
  nivel <- pmin(nivel, max_stars)
  vapply(nivel, function(n) {
    llenas  <- paste0('<span style="color:#f0a500">', strrep("\u2605", n),       "</span>")
    vacias  <- paste0('<span style="color:#ddd">',   strrep("\u2605", max_stars - n), "</span>")
    paste0(llenas, vacias)
  }, character(1))
}
