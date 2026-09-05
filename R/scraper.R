# R/scraper.R
# Funciones para buscar sesiones en el sitio del Parlamento de Uruguay.
# La búsqueda se hace contra el buscador oficial de parlamento.gub.uy,
# que acepta cualquier palabra clave.

BASE_URL <- "https://parlamento.gub.uy/index.php/documentosyleyes/documentos/diarios-de-sesion"

# User-Agent de navegador real: el sitio del Parlamento (nginx) responde 403 a
# clientes sin UA de navegador. read_html() no envía UA, por eso la búsqueda
# hay que hacerla con httr::GET enviando estos headers.
.UA_NAV <- paste0(
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) ",
  "AppleWebKit/537.36 (KHTML, like Gecko) ",
  "Chrome/124.0.0.0 Safari/537.36"
)

#' Descarga una página del buscador como HTML parseado, con headers de navegador.
#'
#' Devuelve siempre una lista list(html = , error = ):
#'   - éxito  -> html con el documento parseado, error NULL
#'   - fallo  -> html NULL, error con el motivo legible (código HTTP o error de red)
#'
#' Es importante que el motivo viaje hacia arriba: el sitio del Parlamento
#' bloquea las IP de fuera de Uruguay (responde 403 o directamente no contesta),
#' y sin este dato la app mostraba "no se encontraron sesiones" —indistinguible
#' de una búsqueda legítimamente vacía— en vez de explicar el bloqueo.
#'
#' @param url URL de la página del buscador a descargar.
#' @return Una lista con los elementos `html` y `error`.
#' @keywords internal
.leer_html_nav <- function(url) {
  resp <- tryCatch(
    httr::GET(
      url,
      httr::add_headers(
        `User-Agent`      = .UA_NAV,
        `Accept`          = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
        `Accept-Language` = "es-UY,es;q=0.9",
        `Referer`         = BASE_URL
      ),
      httr::timeout(40)
    ),
    error = function(e) e
  )

  if (inherits(resp, "condition")) {
    return(list(
      html  = NULL,
      error = paste0("No se pudo conectar con parlamento.gub.uy (",
                     conditionMessage(resp), ")")
    ))
  }

  estado <- httr::status_code(resp)
  if (estado >= 400) {
    return(list(
      html  = NULL,
      error = sprintf("parlamento.gub.uy respondi\u00f3 HTTP %d", estado)
    ))
  }

  pagina <- tryCatch(
    xml2::read_html(httr::content(resp, as = "text", encoding = "UTF-8")),
    error = function(e) NULL
  )
  if (is.null(pagina)) {
    return(list(html = NULL, error = "La respuesta del sitio no se pudo interpretar como HTML"))
  }

  list(html = pagina, error = NULL)
}

#' Busca sesiones parlamentarias que mencionan un término dado
#'
#' @param texto       Término a buscar (ej: "CANNABIS", "VIVIENDA SOCIAL")
#' @param camara      Cámara: "All" (ambas), "S" (Senado), "R" (Representantes)
#' @param lgl_ids     Vector de IDs de legislatura a buscar (ver LEGISLATURAS en global.R)
#' @param progreso_fn Función opcional para reportar progreso: function(valor, mensaje)
#'
#' @return tibble con columnas: legislatura, lgl_id, cuerpo, fecha, id_doc,
#'   url_intermedia. Lleva siempre el atributo "errores": vector de mensajes con
#'   los fallos de red/HTTP que hubo. Un tibble vacío CON errores significa
#'   "no se pudo consultar el sitio"; vacío SIN errores, "el sitio no devolvió nada".
#' @export
buscar_sesiones <- function(texto,
                             camara      = "All",
                             lgl_ids     = c(49, 50),
                             progreso_fn = NULL) {

  lgls <- LEGISLATURAS %>% filter(lgl_id %in% lgl_ids)
  if (nrow(lgls) == 0) return(structure(tibble::tibble(), errores = character(0)))

  resultados <- list()
  errores    <- character(0)

  for (i in seq_len(nrow(lgls))) {
    lgl <- lgls[i, ]

    if (!is.null(progreso_fn)) {
      progreso_fn(
        valor   = (i - 1) / nrow(lgls),
        mensaje = paste("Buscando en", lgl$etiqueta, "...")
      )
    }

    page_num      <- 0
    hay_siguiente <- TRUE

    # El buscador del parlamento usa form-encoding estándar: espacios como "+"
    # No usar comillas — el sitio no las soporta y devuelve 0 resultados.
    texto_url <- gsub(" +", "+", trimws(texto))

    while (hay_siguiente) {
      url_busqueda <- sprintf(
        "%s?Cpo_codigo=%s&Lgl_Nro=%d&fecha_desde=%s&fecha_hasta=%s&Ts_diario=&Ssn_Nro=&Tipobusqueda=All&Texto=%s&page=%d",
        BASE_URL, camara, lgl$lgl_id,
        lgl$fecha_desde, lgl$fecha_hasta,
        texto_url,
        page_num
      )

      respuesta <- .leer_html_nav(url_busqueda)
      if (is.null(respuesta$html)) {
        errores <- c(errores, sprintf("%s: %s", lgl$etiqueta, respuesta$error))
        hay_siguiente <- FALSE
        break
      }
      pagina <- respuesta$html

      links <- pagina %>%
        rvest::html_nodes("td.views-field-DS-File-IMG a") %>%
        rvest::html_attr("href")

      fechas <- pagina %>%
        rvest::html_nodes("td.views-field-DS-Fecha") %>%
        rvest::html_text(trim = TRUE)

      if (length(links) == 0) { hay_siguiente <- FALSE; break }

      for (j in seq_along(links)) {
        link <- links[j]
        if (!grepl("^http", link)) link <- paste0("https://parlamento.gub.uy", link)
        link <- sub("^http://", "https://", link)

        # El link es .../diarios-de-sesion/7017/IMG : el ID es el último grupo
        # de dígitos del path, no necesariamente el final de la cadena.
        id_doc    <- str_extract(link, "(?<=diarios-de-sesion/)\\d+")
        if (is.na(id_doc)) id_doc <- str_extract(link, "\\d+(?=/[A-Za-z]+/?$)")
        if (is.na(id_doc)) id_doc <- str_extract(link, "\\d+$")
        raw_fecha <- trimws(fechas[j])
        parsed_d <- tryCatch(
          as.Date(raw_fecha, tryFormats = c("%d-%m-%Y", "%d/%m/%Y", "%Y-%m-%d")),
          error = function(e) NA
        )
        if (is.na(parsed_d)) {
          fecha_fmt <- gsub("[^a-zA-Z0-9-]", "-", raw_fecha)
          fecha_fmt <- gsub("-+", "-", fecha_fmt) # Quitar dobles guiones
        } else {
          fecha_fmt <- as.character(parsed_d)
        }

        resultados[[length(resultados) + 1]] <- tibble::tibble(
          legislatura    = lgl$etiqueta,
          lgl_id         = lgl$lgl_id,
          cuerpo         = camara,      # código del cuerpo: A, C, R, S, All
          fecha          = fecha_fmt,
          id_doc         = id_doc,
          url_intermedia = link
        )
      }

      siguiente <- pagina %>% rvest::html_node("li.pager__item--next a")
      if (is.null(siguiente) || is.na(siguiente)) {
        hay_siguiente <- FALSE
      } else {
        page_num <- page_num + 1
        Sys.sleep(0.5)  # pausa amigable con el servidor
      }
    }
  }

  if (length(resultados) == 0) return(structure(tibble::tibble(), errores = errores))
  structure(
    bind_rows(resultados) %>% arrange(desc(fecha)),
    errores = errores
  )
}

#' Obtiene la URL directa del PDF desde la página intermedia del parlamento
#'
#' Usa httr con User-Agent de navegador real. Prueba tres selectores CSS en
#' cascada para cubrir tanto la estructura actual del sitio como páginas
#' antiguas de legislaturas anteriores (que usan HTML diferente).
#'
#' @param url_intermedia URL del visor de documentos del parlamento
#' @return URL del PDF como string, o NA si no se encuentra
#' @export
obtener_url_pdf <- function(url_intermedia) {
  ua <- paste0(
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) ",
    "AppleWebKit/537.36 (KHTML, like Gecko) ",
    "Chrome/124.0.0.0 Safari/537.36"
  )

  resp <- tryCatch(
    httr::GET(
      url_intermedia,
      httr::add_headers(`User-Agent` = ua, `Accept-Language` = "es-UY,es;q=0.9"),
      httr::timeout(30)
    ),
    error = function(e) NULL
  )

  if (is.null(resp) || httr::http_error(resp)) return(NA_character_)

  # Dejar que httr detecte el encoding (no forzar UTF-8; páginas viejas usan Latin-1)
  pagina <- tryCatch(
    httr::content(resp, as = "parsed"),
    error = function(e) NULL
  )
  if (is.null(pagina)) return(NA_character_)

  todos_links <- pagina %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href")

  # ── Selector 1: bloque de contenido de la estructura NUEVA del sitio ─────────
  pdf_link <- pagina %>%
    rvest::html_nodes("#block-custom-parlamento-content a") %>%
    rvest::html_attr("href") %>%
    grep("\\.pdf($|\\?)", ., ignore.case = TRUE, value = TRUE) %>%
    .[1]

  if (length(pdf_link) > 0 && !is.na(pdf_link)) return(pdf_link)

  # ── Selector 2: contenido principal (Drupal clásico, páginas viejas) ─────────
  pdf_link <- pagina %>%
    rvest::html_nodes(
      "#content a, #main-content a, .region-content a, article a, .view-content a"
    ) %>%
    rvest::html_attr("href") %>%
    grep("\\.pdf($|\\?)", ., ignore.case = TRUE, value = TRUE) %>%
    .[1]

  if (length(pdf_link) > 0 && !is.na(pdf_link)) return(pdf_link)

  # ── Selector 3: cualquier link a PDF en toda la página (último recurso) ───────
  pdf_link <- todos_links %>%
    grep("\\.pdf($|\\?)", ., ignore.case = TRUE, value = TRUE) %>%
    # Excluir anchors de navegación y links a otras páginas de búsqueda
    grep("diario|sesion|DS-|documentos|/files/", ., ignore.case = TRUE, value = TRUE) %>%
    .[1]

  if (length(pdf_link) > 0 && !is.na(pdf_link)) return(pdf_link)

  # ── Selector 4: cualquier link a PDF, sin filtrar ────────────────────────────
  pdf_link <- todos_links %>%
    grep("\\.pdf($|\\?)", ., ignore.case = TRUE, value = TRUE) %>%
    .[1]

  if (length(pdf_link) > 0 && !is.na(pdf_link)) return(pdf_link)

  return(NA_character_)
}
