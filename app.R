# app.R
# Diarios del Parlamento Uruguay — Buscador y Analizador
# github.com/EmilianoGonLas/diarios-parlamento-uy

source("global.R")

# ══════════════════════════════════════════════════════════════════════════════
# UI
# ══════════════════════════════════════════════════════════════════════════════

ui <- tagList(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1")
  ),
  bslib::page_navbar(
  title = tagList(
    tags$span(style = "font-size:1.1em; margin-right:7px; vertical-align:-.05em;", "\U0001f3db\ufe0f"),
    tags$span(
      style = "font-weight:700; letter-spacing:-.01em;",
      "Diarios del Parlamento"
    ),
    tags$span(
      style = "font-weight:300; color:rgba(255,255,255,.65); margin-left:5px;",
      "\u00b7 Uruguay"
    )
  ),
  theme = bslib::bs_theme(
    bootswatch  = "flatly",
    primary     = "#1a4971",
    base_font   = bslib::font_google("Inter"),
    "navbar-bg" = "#1a4971"
  ),
  fillable = FALSE,

  # ── Tab 1: Buscador (siempre visible) ──────────────────────────────────────
  bslib::nav_panel(
    title = tagList(icon("magnifying-glass", class = "me-1"), "Buscador"),

    bslib::card(
      bslib::card_header(
        tagList(icon("magnifying-glass", class = "me-2"), "Búsqueda en diarios de sesión")
      ),
      bslib::card_body(
        bslib::layout_columns(
          col_widths = c(5, 4, 3),
          div(
            textInput("txt_termino",
                       label       = "Término de búsqueda",
                       placeholder = "Ej: cannabis, salud, educacion"),
            tags$small(class = "text-muted",
                        "Separ\u00e1 varios t\u00e9rminos con comas para combinar resultados.")
          ),
          selectInput("sel_camara", "Cuerpo legislativo",
                       choices = c(
                         "- Cualquiera -"               = "All",
                         "Asamblea General"              = "A",
                         "Comisi\u00f3n Permanente"      = "C",
                         "C\u00e1mara de Representantes" = "R",
                         "C\u00e1mara de Senadores"      = "S"
                       )),
          div(class = "d-flex align-items-end pb-1",
              actionButton("btn_buscar", "Buscar sesiones",
                            class = "btn-primary w-100",
                            icon  = icon("magnifying-glass"))
          )
        ),
        div(class = "mt-3",
            checkboxGroupInput(
              "sel_legislaturas",
              label    = "Legislaturas",
              choices  = setNames(LEGISLATURAS$lgl_id, LEGISLATURAS$etiqueta),
              selected = c(49, 50),
              inline   = TRUE
            )
        )
      )
    ),

    uiOutput("panel_resultados")
  ),

  # ── Tab 2: Analizador (local: carpeta; web: subida de archivos) ───────────
  bslib::nav_panel(
    title = tagList(icon("chart-bar", class = "me-1"), "Analizador"),
    uiOutput("panel_tab2")
  ),

  # ── Link a GitHub ───────────────────────────────────────────────────────────
  bslib::nav_spacer(),
  bslib::nav_item(
    tags$a(
      href   = "https://github.com/EmilianoGonLas/diarios-parlamento-uy",
      target = "_blank",
      style  = "font-size:.88rem;",
      icon("github", class = "me-1"), "GitHub"
    )
  )
)) # cierre de page_navbar y tagList

# ══════════════════════════════════════════════════════════════════════════════
# SERVER
# ══════════════════════════════════════════════════════════════════════════════

server <- function(input, output, session) {

  rv <- reactiveValues(
    resultados       = NULL,  # tibble: sesiones encontradas por el buscador
    analisis_res     = NULL,  # tibble: resultados del análisis de PDFs
    carpeta_analisis = NULL,  # ruta de la carpeta analizada (para abrir PDFs)
    terminos_ids     = c(),   # IDs de los términos de comparación activos
    contador         = 0,     # contador para generar IDs únicos
    ultimo_dl        = NULL,  # estadísticas del último ZIP descargado
    dir_pdfs         = NULL   # directorio de PDFs subidos (solo en versión web)
  )

  # ── Búsqueda ────────────────────────────────────────────────────────────────

  observeEvent(input$btn_buscar, {
    # Soporta múltiples términos separados por coma: "aborto, familia, salud"
    terminos <- trimws(strsplit(input$txt_termino, ",")[[1]])
    terminos <- terminos[nzchar(terminos)]

    if (length(terminos) == 0) {
      showNotification(
        ui       = tagList(icon("triangle-exclamation", class = "me-1"),
                           "Ingres\u00e1 al menos un t\u00e9rmino de b\u00fasqueda."),
        type     = "warning",
        duration = 5
      )
      return()
    }
    if (length(input$sel_legislaturas) == 0) {
      showNotification(
        ui       = tagList(icon("triangle-exclamation", class = "me-1"),
                           "Seleccion\u00e1 al menos una legislatura."),
        type     = "warning",
        duration = 5
      )
      return()
    }

    withProgress(message = "Buscando en parlamento.gub.uy...", value = 0, {
      todos <- purrr::map(seq_along(terminos), function(k) {
        setProgress(
          value   = (k - 1) / length(terminos),
          message = sprintf("Buscando \"%s\"\u2026 (%d de %d t\u00e9rminos)", terminos[k], k, length(terminos))
        )
        buscar_sesiones(
          texto   = terminos[k],
          camara  = input$sel_camara,
          lgl_ids = as.integer(input$sel_legislaturas)
          # Sin progreso_fn: la barra exterior ya reporta el avance por término
        )
      })

      setProgress(value = 1, message = "Combinando resultados\u2026")
      combinado <- dplyr::bind_rows(todos)
      # bind_rows de tibbles vacíos produce un df sin columnas → verificar antes de operar
      if (nrow(combinado) > 0 && "url_intermedia" %in% names(combinado)) {
        combinado <- dplyr::distinct(combinado, url_intermedia, .keep_all = TRUE)
      }
      if (nrow(combinado) > 0 && "fecha" %in% names(combinado)) {
        combinado <- dplyr::arrange(combinado, dplyr::desc(fecha))
      }
      rv$resultados <- combinado
    })
  })

  # Panel de resultados (aparece después de buscar)
  output$panel_resultados <- renderUI({
    req(rv$resultados)

    if (nrow(rv$resultados) == 0) {
      return(
        bslib::card(
          bslib::card_body(
            p(class = "text-muted text-center py-4",
              "\u26a0\ufe0f No se encontraron sesiones con ese término. Probá con otra palabra clave.")
          )
        )
      )
    }

    bslib::card(
      bslib::card_header(
        bslib::layout_columns(
          col_widths = c(6, 6),
          div(
            strong(sprintf("\u2705 %d sesiones encontradas", nrow(rv$resultados))),
            uiOutput("sel_count", inline = TRUE)
          ),
          div(class = "text-end d-flex gap-2 justify-content-end align-items-center",
              actionButton("btn_sel_todos", "Seleccionar todas",
                            class = "btn-sm btn-outline-secondary"),
              actionButton("btn_desel_todos", "Deseleccionar todas",
                            class = "btn-sm btn-outline-danger"),
              downloadButton("btn_descargar", "Descargar ZIP",
                              class = "btn-sm btn-success")
          )
        )
      ),
      bslib::card_body(
        tags$small(class = "text-muted d-block mb-2",
                    "Hac\u00e9 clic en una fila para seleccionarla o deseleccionarla."),
        DT::DTOutput("tbl_resultados")
      )
    )
  })

  # Tabla interactiva de resultados
  output$tbl_resultados <- DT::renderDT({
    req(rv$resultados)

    rv$resultados %>%
      mutate(
        ` `  = "",   # columna de checkbox visual (ver styles.css)
        Ver  = sprintf(
          '<a href="%s" target="_blank"><i class="fa fa-file-pdf"></i> Ver</a>',
          url_intermedia
        )
      ) %>%
      select(
        ` `,
        Legislatura = legislatura,
        Fecha       = fecha,
        `ID`        = id_doc,
        Ver
      ) %>%
      DT::datatable(
        selection = "multiple",
        rownames  = FALSE,
        escape    = FALSE,
        options   = list(
          pageLength = 20,
          scrollY    = "420px",
          dom        = "frtip",
          columnDefs = list(
            list(orderable = FALSE, width = "36px", targets = 0)
          ),
          language   = list(
            url = "//cdn.datatables.net/plug-ins/1.13.6/i18n/es-ES.json"
          )
        )
      )
  })

  # Contador de seleccionadas
  output$sel_count <- renderUI({
    n_sel <- length(input$tbl_resultados_rows_selected)
    n_tot <- if (!is.null(rv$resultados)) nrow(rv$resultados) else 0
    if (n_sel == 0) {
      span(class = "text-muted ms-2 small", "— ninguna seleccionada")
    } else {
      tagList(
        span(class = "ms-2"),
        tags$span(class = "badge bg-primary",
                   sprintf("%d de %d seleccionadas", n_sel, n_tot))
      )
    }
  })

  # Seleccionar todas las filas
  observeEvent(input$btn_sel_todos, {
    req(rv$resultados)
    DT::selectRows(DT::dataTableProxy("tbl_resultados"), seq_len(nrow(rv$resultados)))
  })

  # Deseleccionar todas las filas
  observeEvent(input$btn_desel_todos, {
    DT::selectRows(DT::dataTableProxy("tbl_resultados"), NULL)
  })

  # ── Descarga de PDFs como ZIP ───────────────────────────────────────────────

  output$btn_descargar <- downloadHandler(
    filename = function() {
      termino_limpio <- gsub("[^a-zA-Z0-9]", "_", toupper(input$txt_termino))
      paste0("sesiones_", termino_limpio, "_", Sys.Date(), ".zip")
    },
    content = function(zip_destino) {
      filas_sel <- input$tbl_resultados_rows_selected

      if (length(filas_sel) == 0) {
        showNotification(
          "Seleccioná al menos una sesión en la tabla antes de descargar.",
          type = "warning", duration = 5
        )
        return(NULL)
      }

      sesiones_sel <- rv$resultados[filas_sel, ]
      tmp <- file.path(tempdir(), paste0("dl_", as.integer(Sys.time())))
      fs::dir_create(tmp)

      resultado_dl <- withProgress(message = "Descargando PDFs...", value = 0, {
        descargar_sesiones(
          sesiones    = sesiones_sel,
          directorio  = tmp,
          progreso_fn = function(valor, mensaje) {
            setProgress(value = valor, message = mensaje)
          }
        )
      })

      # Guardar estadísticas para mostrar notificación post-descarga
      n_ok  <- sum(grepl("Descargado", resultado_dl$estado, fixed = TRUE), na.rm = TRUE)
      n_tot <- nrow(sesiones_sel)
      rv$ultimo_dl <- list(ok = n_ok, total = n_tot, t = Sys.time())

      crear_zip(tmp, zip_destino)
    }
  )

  # Notificación post-descarga: cuántos PDFs se obtuvieron vs. cuántos no
  observeEvent(rv$ultimo_dl, {
    req(!is.null(rv$ultimo_dl))
    r <- rv$ultimo_dl
    if (r$ok == r$total) {
      showNotification(
        sprintf("\u2705 %d PDFs descargados correctamente.", r$ok),
        type = "message", duration = 6
      )
    } else {
      showNotification(
        ui = tagList(
          tags$b(sprintf("\u26a0\ufe0f %d de %d PDFs descargados.", r$ok, r$total)),
          tags$br(),
          tags$span(
            class = "small",
            sprintf(
              "%d sesi%s no tiene%s PDF accesible en el sitio del parlamento.",
              r$total - r$ok,
              if (r$total - r$ok == 1) "\u00f3n" else "ones",
              if (r$total - r$ok == 1) "" else "n"
            )
          )
        ),
        type = "warning", duration = 10
      )
    }
  }, ignoreInit = TRUE)

  # ── Tab 2: Analizador (con selector de carpeta en local, subida en web) ────

  output$panel_tab2 <- renderUI({
    ui_analizador()
  })

  # ── Análisis de PDFs ────────────────────────────────────────────────────────

  # En local: selector nativo de carpeta via shinyFiles
  if (ES_LOCAL) {
    roots_locales <- c(
      `Este proyecto` = normalizePath("."),
      Escritorio      = normalizePath(file.path(Sys.getenv("USERPROFILE"), "Desktop")),
      Documentos      = normalizePath(file.path(Sys.getenv("USERPROFILE"), "Documents")),
      Descargas       = normalizePath(file.path(Sys.getenv("USERPROFILE"), "Downloads"))
    )
    shinyFiles::shinyDirChoose(input, "btn_carpeta", roots = roots_locales,
                                allowDirCreate = FALSE)
  }

  # En web: manejar subida de archivos → copiar a carpeta temporal
  if (!ES_LOCAL) {
    observeEvent(input$pdf_upload, {
      req(input$pdf_upload)

      subida <- input$pdf_upload
      n_subidos <- nrow(subida)

      # ── Tope de volumen (evita saturar la instancia en la nube) ──────────────
      # Se aceptan archivos en orden hasta llegar al límite de cantidad o de peso
      # total; el resto se descarta con aviso, sugiriendo la versión local.
      tam_mb <- subida$size / 1024^2
      acum_mb <- cumsum(tam_mb)
      dentro  <- seq_len(n_subidos) <= MAX_PDFS_WEB & acum_mb <= MAX_MB_WEB
      if (!any(dentro)) dentro[1] <- TRUE  # al menos el primero, aunque sea grande
      subida  <- subida[dentro, , drop = FALSE]
      n <- nrow(subida)
      n_descartados <- n_subidos - n

      tmp <- file.path(tempdir(), paste0("upload_", as.integer(Sys.time())))
      fs::dir_create(tmp)
      purrr::walk(seq_len(n), function(i) {
        file.copy(subida$datapath[i], file.path(tmp, subida$name[i]))
      })
      rv$dir_pdfs <- tmp

      if (n_descartados > 0) {
        showNotification(
          ui = tagList(
            tags$b(sprintf("⚠️ Se cargaron %d de %d PDFs.", n, n_subidos)),
            tags$br(),
            tags$span(class = "small", sprintf(
              "En la nube el análisis se limita a %d PDFs o %d MB para no saturar el servidor. Para analizar el conjunto completo, corré la app en tu computadora (ver pestaña de instrucciones).",
              MAX_PDFS_WEB, MAX_MB_WEB))
          ),
          type = "warning", duration = 12
        )
      } else {
        showNotification(
          ui       = tagList(icon("check-circle", class = "me-1"),
                             sprintf("%d PDF%s listo%s para analizar.",
                                     n,
                                     if (n == 1) "" else "s",
                                     if (n == 1) "" else "s")),
          type     = "message",
          duration = 4
        )
      }
    })
  }

  # Reactivo unificado: devuelve la carpeta de PDFs sin importar el entorno
  carpeta_pdfs <- reactive({
    if (ES_LOCAL) {
      req(input$btn_carpeta)
      p <- shinyFiles::parseDirPath(roots_locales, input$btn_carpeta)
      if (length(p) == 0 || !nzchar(p)) return(NULL)
      p
    } else {
      rv$dir_pdfs
    }
  })

  output$lbl_carpeta <- renderText({
    if (!ES_LOCAL) return("")
    p <- carpeta_pdfs()
    if (is.null(p)) "Ninguna carpeta seleccionada" else p
  })

  # Función auxiliar: leer todos los términos de comparación activos.
  # Acepta términos separados por coma O por | en el campo de patrón:
  # "turismo, viajes, extranjero" → "turismo|viajes|extranjero"
  leer_terminos_cruce <- function() {
    terminos <- list()
    for (id in rv$terminos_ids) {
      lbl <- trimws(input[[paste0("lbl_", id)]])
      pat <- trimws(input[[paste0("pat_", id)]])
      if (nzchar(lbl) && nzchar(pat)) {
        # Normalizar: "a, b, c" → "a|b|c"
        pat_regex <- paste(trimws(strsplit(pat, "[,|]+")[[1]]), collapse = "|")
        terminos[[lbl]] <- pat_regex
      }
    }
    terminos
  }

  # Agregar un nuevo término de comparación
  observeEvent(input$btn_agregar_termino, {
    rv$contador <- rv$contador + 1
    id <- rv$contador

    insertUI(
      selector = "#contenedor_terminos",
      where    = "beforeEnd",
      ui = div(
        id    = paste0("fila_termino_", id),
        class = "d-flex gap-2 mb-2 align-items-end",
        div(style = "width:35%",
            textInput(paste0("lbl_", id),
                       label       = "Nombre del tema",
                       placeholder = "ej: turismo")),
        div(style = "flex:1",
            textInput(paste0("pat_", id),
                       label       = "Palabras a buscar (separá con comas)",
                       placeholder = "ej: turismo, extranjero, no residente")),
        div(style = "padding-bottom:1px",
            actionButton(paste0("rm_", id), NULL,
                          icon  = icon("times"),
                          class = "btn-outline-danger btn-sm",
                          title = "Eliminar este término"))
      )
    )

    # Observer para eliminar esta fila (once=TRUE evita disparos múltiples)
    observeEvent(input[[paste0("rm_", id)]], {
      removeUI(selector = paste0("#fila_termino_", id))
      rv$terminos_ids <- setdiff(rv$terminos_ids, id)
    }, ignoreInit = TRUE, once = TRUE)

    rv$terminos_ids <- c(rv$terminos_ids, id)
  })

  observeEvent(input$btn_analizar, {
    req(input$txt_termino_analisis)
    ruta <- carpeta_pdfs()

    if (is.null(ruta)) {
      if (ES_LOCAL) {
        showNotification(
          ui       = tagList(icon("triangle-exclamation", class = "me-1"),
                             "Seleccion\u00e1 la carpeta con los PDFs antes de analizar."),
          type     = "warning", duration = 5
        )
      } else {
        showNotification(
          ui       = tagList(icon("triangle-exclamation", class = "me-1"),
                             "Sub\u00ed los PDFs antes de analizar."),
          type     = "warning", duration = 5
        )
      }
      return()
    }

    rv$carpeta_analisis <- ruta   # guardar para armar rutas en la tabla

    # Normalizar término principal: "cannabis, marihuana" → "cannabis|marihuana"
    termino_regex <- paste(
      trimws(strsplit(input$txt_termino_analisis, "[,|]+")[[1]]),
      collapse = "|"
    )

    withProgress(message = "Analizando PDFs...", value = 0, {
      rv$analisis_res <- analizar_pdfs(
        directorio        = ruta,
        termino_principal = termino_regex,
        terminos_cruce    = leer_terminos_cruce(),
        progreso_fn       = function(valor, mensaje) {
          setProgress(value = valor, message = mensaje)
        }
      )
    })

    n_ok <- sum(rv$analisis_res$estado == "OK", na.rm = TRUE)
    showNotification(
      sprintf("Análisis completado: %d sesiones procesadas.", n_ok),
      type = "message", duration = 4
    )
  })

  output$panel_timeline <- renderUI({
    if (is.null(rv$analisis_res)) {
      placeholder_grafico(
        icono     = "chart-bar",
        titulo    = "\u00bfCu\u00e1ndo se mencion\u00f3 el tema?",
        descripcion = paste(
          "Una vez que analices los PDFs, ac\u00e1 vas a ver un gr\u00e1fico de barras",
          "que muestra cu\u00e1ntas p\u00e1ginas de cada sesi\u00f3n mencionan el tema",
          "que buscaste, ordenadas por fecha.",
          "As\u00ed pod\u00e9s ver si el tema fue creciendo, bajando o tuvo picos en",
          "momentos concretos (por ejemplo, cuando se debati\u00f3 una ley)."
        )
      )
    } else {
      tagList(
        p(class = "text-muted small mt-2",
          "Cada barra es una sesi\u00f3n. La l\u00ednea roja muestra la tendencia general.",
          "Pas\u00e1 el cursor sobre las barras para ver los detalles."),
        plotly::plotlyOutput("grafico_timeline", height = "350px")
      )
    }
  })

  output$grafico_timeline <- plotly::renderPlotly({
    req(rv$analisis_res)
    grafico_timeline(
      rv$analisis_res,
      titulo = paste("Menciones de", dQuote(input$txt_termino_analisis), "en el tiempo")
    )
  })

  output$panel_cruce <- renderUI({
    if (is.null(rv$analisis_res)) {
      placeholder_grafico(
        icono     = "magnifying-glass-chart",
        titulo    = "\u00bfJunto a qu\u00e9 otros temas aparece?",
        descripcion = paste(
          "Si agregaste temas de comparaci\u00f3n en la configuraci\u00f3n,",
          "ac\u00e1 vas a ver cu\u00e1ntas veces el tema principal apareci\u00f3",
          "en la misma p\u00e1gina que esos otros temas.",
          "Sirve para detectar si dos temas se discuten juntos.",
          "Por ejemplo: \u00bfse habla de cannabis y turismo en las mismas sesiones?"
        )
      )
    } else if (length(rv$terminos_ids) == 0) {
      placeholder_grafico(
        icono     = "plus-circle",
        titulo    = "No hay temas de comparaci\u00f3n configurados",
        descripcion = paste(
          "Para ver este gr\u00e1fico, agreg\u00e1 al menos un tema de comparaci\u00f3n",
          "en el panel de configuraci\u00f3n (bot\u00f3n \u2018Agregar tema\u2019)",
          "y vol\u00e9 a hacer clic en \u2018Analizar PDFs\u2019."
        )
      )
    } else {
      tagList(
        p(class = "text-muted small mt-2",
          "Muestra cu\u00e1ntas p\u00e1ginas tienen el tema principal y el tema de comparaci\u00f3n",
          "al mismo tiempo. Si la barra es alta, esos temas se discuten juntos."),
        plotly::plotlyOutput("grafico_cruce", height = "350px")
      )
    }
  })

  output$grafico_cruce <- plotly::renderPlotly({
    req(rv$analisis_res)
    grafico_coocurrencias(rv$analisis_res, leer_terminos_cruce())
  })

  output$tbl_analisis <- DT::renderDT({
    req(rv$analisis_res, rv$carpeta_analisis)

    df <- rv$analisis_res %>%
      filter(estado == "OK") %>%
      arrange(desc(total_cruces), desc(paginas_con_termino))

    # Columnas de cruce presentes (cruce_turismo, cruce_internacional, etc.)
    cols_cruce <- names(df)[startsWith(names(df), "cruce_")]

    # Renombrar cruce_X → X para mostrar el nombre limpio
    nombres_cruce <- setNames(
      cols_cruce,
      stringr::str_remove(cols_cruce, "^cruce_") %>% stringr::str_to_title()
    )

    # Marcar posibles duplicados: mismas menciones + mismo score (>0)
    df <- df %>%
      group_by(paginas_con_termino, score_raw) %>%
      mutate(es_duplicado = n() > 1 & paginas_con_termino > 0) %>%
      ungroup()

    df <- df %>%
      mutate(
        fecha     = ifelse(is.na(fecha), "—", as.character(fecha)),
        legislatura = ifelse(is.na(legislatura), "—", legislatura),

        # Nombre corto del archivo para mostrar en tabla
        Documento = ifelse(
          es_duplicado,
          paste0('<span title="', archivo, '">', stringr::str_trunc(archivo, 28),
                 '</span> <span class="badge bg-warning text-dark" title="Mismo contenido que otra sesi\u00f3n">duplicado</span>'),
          paste0('<span title="', archivo, '">', stringr::str_trunc(archivo, 32), '</span>')
        ),

        # Puntuación visual con estrellas
        Relevancia = estrellas_html(score_raw),

        # Botón abrir PDF
        PDF = sprintf(
          '<button class="btn btn-sm btn-outline-primary abrir-pdf"
                   data-path="%s" title="Abrir este PDF">
             <i class="fa fa-file-pdf"></i> Abrir
           </button>',
          file.path(rv$carpeta_analisis, archivo)
        ),

        # Botón ver frases (solo si hay fragmentos)
        `Ver frases` = ifelse(
          !is.na(fragmentos_texto) & nzchar(fragmentos_texto),
          sprintf(
            '<button class="btn btn-sm btn-outline-info ver-frases"
                     data-id="%s" title="Ver frases donde aparecen juntos los temas">
               <i class="fa fa-quote-left"></i> Ver frases
             </button>',
            archivo
          ),
          '<span class="text-muted small">sin cruce</span>'
        )
      ) %>%
      rename(all_of(nombres_cruce)) %>%
      select(
        Relevancia,
        PDF,
        `Ver frases`,
        Legislatura = legislatura,
        Fecha       = fecha,
        Documento,
        `Menciones` = paginas_con_termino,
        all_of(names(nombres_cruce))  # columnas de cruce con nombre limpio
      )

    n_cruce_cols <- length(cols_cruce)

    DT::datatable(
      df,
      rownames  = FALSE,
      escape    = FALSE,
      selection = "none",
      callback  = DT::JS("
        table.on('click', 'button.abrir-pdf', function() {
          Shiny.setInputValue('abrir_pdf_local',
            $(this).data('path'), {priority: 'event'});
        });
        table.on('click', 'button.ver-frases', function() {
          Shiny.setInputValue('ver_frases_click',
            $(this).data('id'), {priority: 'event'});
        });
      "),
      options = list(
        pageLength  = 15,
        language    = list(url = "//cdn.datatables.net/plug-ins/1.13.6/i18n/es-ES.json"),
        columnDefs  = list(
          list(orderable = FALSE, targets = 0:2, width = "90px"),
          list(className = "dt-center", targets = 0)
        )
      )
    )
  })

  # Modal con frases de contexto
  observeEvent(input$ver_frases_click, {
    req(rv$analisis_res)
    fila <- rv$analisis_res %>%
      filter(archivo == input$ver_frases_click) %>%
      slice(1)

    req(nrow(fila) == 1)

    fecha_str <- if (is.na(fila$fecha)) "fecha desconocida" else
      format(fila$fecha, "%d/%m/%Y")
    lgl_badge <- if (!is.na(fila$legislatura))
      tags$span(class = "badge bg-primary ms-2 fw-normal", fila$legislatura)
    else NULL

    showModal(modalDialog(
      title = tagList(
        icon("quote-left", class = "me-2 text-primary"),
        "Contexto de menciones",
        lgl_badge,
        if (!is.na(fila$fecha))
          tags$span(class = "badge bg-light text-muted ms-2 fw-normal border",
                    icon("calendar", class = "me-1"), fecha_str)
      ),
      tags$div(
        tags$p(
          class = "text-muted small mb-3",
          "Los t\u00e9rminos del tema de comparaci\u00f3n aparecen en ",
          tags$strong("negrita"), ". Cada fragmento es el contexto cercano",
          " en la p\u00e1gina donde tambi\u00e9n se mencion\u00f3 el tema principal."
        ),
        HTML(fila$fragmentos_texto)
      ),
      easyClose = TRUE,
      size      = "l",
      footer    = tagList(
        tags$span(class = "text-muted small me-auto",
                  icon("file-pdf", class = "me-1"), fila$archivo),
        modalButton("Cerrar")
      )
    ))
  })

  # Abrir PDF local con el visor del sistema operativo
  observeEvent(input$abrir_pdf_local, {
    req(input$abrir_pdf_local, ES_LOCAL)
    ruta_pdf <- normalizePath(input$abrir_pdf_local, mustWork = FALSE)
    browseURL(paste0("file:///", gsub("\\\\", "/", ruta_pdf)))
  })

  output$btn_descargar_csv <- downloadHandler(
    filename = function() paste0("analisis_", Sys.Date(), ".csv"),
    content  = function(f) write.csv(rv$analisis_res, f, row.names = FALSE)
  )
}

# ══════════════════════════════════════════════════════════════════════════════
# Helpers de UI (definidas fuera del server para que sean accesibles)
# ══════════════════════════════════════════════════════════════════════════════

ui_analizador <- function() {
  tagList(
    bslib::layout_columns(
      col_widths = c(4, 8),

      # Panel izquierdo: configuración
      bslib::card(
        bslib::card_header("\u2699\ufe0f Configuración"),
        bslib::card_body(

          # ── Fuente de PDFs: carpeta (local) o subida (web) ──────────────
          if (ES_LOCAL) {
            tagList(
              tags$label(class = "form-label", "Carpeta con los PDFs descargados"),
              div(class = "d-flex gap-2 align-items-center mb-1",
                  shinyFiles::shinyDirButton(
                    "btn_carpeta",
                    label = "Elegir carpeta\u2026",
                    title = "Seleccion\u00e1 la carpeta donde guardaste los PDFs",
                    class = "btn btn-outline-secondary btn-sm",
                    icon  = icon("folder-open")
                  ),
                  tags$code(class = "text-muted small flex-grow-1 text-truncate",
                             textOutput("lbl_carpeta", inline = TRUE))
              ),
              tags$small(class = "text-muted d-block mb-3",
                          "Los PDFs que descargaste desde el Buscador llegan como ZIP.",
                          " Descomprimilo y seleccion\u00e1 esa carpeta.")
            )
          } else {
            tagList(
              div(class = "dp-hero",
                  HTML(sprintf(
                    "<b>An\u00e1lisis en la nube (acotado).</b> Para no saturar el servidor, ac\u00e1 se analizan hasta <b>%d PDFs</b> o <b>%d MB</b> por vez. \u00bfTenN\u00e9s muchos m\u00e1s (cientos de MB)? Corr\u00e9 la app en tu computadora \u2014 sin l\u00edmites (ver la pesta\u00f1a de instrucciones).",
                    MAX_PDFS_WEB, MAX_MB_WEB))),
              fileInput(
                "pdf_upload",
                label       = "PDFs a analizar",
                multiple    = TRUE,
                accept      = ".pdf",
                buttonLabel = tagList(icon("upload"), " Elegir PDFs\u2026"),
                placeholder = "Ning\u00fan archivo seleccionado"
              ),
              tags$small(class = "text-muted d-block mb-3",
                          "Descarg\u00e1 el ZIP desde el Buscador, descomprimilo",
                          " y sub\u00ed los PDFs ac\u00e1.",
                          tags$br(),
                          "Pod\u00e9s seleccionar varios archivos a la vez (Ctrl+clic).")
            )
          },

          tags$hr(),

          # ── Término principal ─────────────────────────────────────────
          textInput("txt_termino_analisis",
                     label       = "Palabra o tema principal a analizar",
                     placeholder = "ej: cannabis, marihuana"),
          tags$small(class = "text-muted d-block mb-3",
                      "Pod\u00e9s escribir varias palabras separadas por comas (o por",
                      tags$code("|"), ").",
                      " Ej:", tags$code("cannabis, marihuana"),
                      "busca sesiones que mencionen cualquiera de las dos."),

          tags$hr(),

          # ── Términos de comparación dinámicos ─────────────────────────
          div(class = "d-flex justify-content-between align-items-center mb-2",
              tags$span(class = "fw-bold small text-uppercase text-muted",
                         "Temas para comparar (opcional)"),
              actionButton("btn_agregar_termino", "Agregar tema",
                            class = "btn-sm btn-outline-primary",
                            icon  = icon("plus"))
          ),
          tags$small(class = "text-muted d-block mb-2",
                      "Podés agregar temas para ver si aparecen",
                      " en las mismas sesiones que el tema principal."),
          div(id = "contenedor_terminos"),  # insertUI agrega aquí

          tags$hr(),
          actionButton("btn_analizar", "Analizar PDFs",
                        class = "btn-primary w-100 mt-2",
                        icon  = icon("chart-bar"))
        )
      ),

      # Panel derecho: resultados en pestañas
      bslib::navset_card_tab(
        bslib::nav_panel(
          title = "\U0001f4cb Resultados",
          div(class = "text-end mt-2 mb-2",
              downloadButton("btn_descargar_csv", "Descargar CSV",
                              class = "btn-sm btn-outline-secondary")
          ),
          DT::DTOutput("tbl_analisis")
        ),
        bslib::nav_panel(
          title = "\U0001f4c5 \u00bfCu\u00e1ndo se mencion\u00f3?",
          uiOutput("panel_timeline")
        ),
        bslib::nav_panel(
          title = "\U0001f50d \u00bfJunto a qu\u00e9 otros temas?",
          uiOutput("panel_cruce")
        )
      )
    )
  )
}

placeholder_grafico <- function(icono, titulo, descripcion) {
  div(
    class = "text-center text-muted py-5 px-4",
    style = "max-width: 520px; margin: 0 auto;",
    icon(icono, style = "font-size:2.8rem; opacity:0.18; margin-bottom:16px;"),
    h6(class = "fw-bold mb-2", titulo),
    p(class = "small", descripcion),
    tags$small("\u2190 Configur\u00e1 el an\u00e1lisis y hac\u00e9 clic en ", tags$strong("Analizar PDFs"))
  )
}

ui_instrucciones <- function() {
  tagList(
    bslib::layout_columns(
      col_widths = c(8, 4),

      # Instrucciones paso a paso
      bslib::card(
        bslib::card_header("\U0001f4bb C\u00f3mo analizar los datos en tu computadora"),
        bslib::card_body(
          p(
            "Esta versi\u00f3n web te permite ", strong("buscar y descargar"),
            " sesiones parlamentarias directamente desde el sitio del Parlamento."
          ),
          p(
            "Para el an\u00e1lisis completo —gr\u00e1ficos de evoluci\u00f3n temporal,",
            " co-ocurrencias entre t\u00e9rminos y exportaci\u00f3n de datos—",
            " necesit\u00e1s ejecutar la app en tu propia computadora.",
            " Es m\u00e1s simple de lo que parece:"
          ),
          tags$hr(),

          # Paso 1
          tags$h5(tags$span(class = "badge bg-primary me-2", "1"), "Instal\u00e1 R y RStudio"),
          tags$ul(
            tags$li(tags$a("Descargar R (cran.r-project.org)",
                            href = "https://cran.r-project.org/", target = "_blank")),
            tags$li(tags$a("Descargar RStudio (posit.co)",
                            href = "https://posit.co/download/rstudio-desktop/", target = "_blank"))
          ),

          # Paso 2
          tags$h5(class = "mt-3",
                   tags$span(class = "badge bg-primary me-2", "2"),
                   "Descarg\u00e1 el proyecto"),
          p(
            "And\u00e1 a ",
            tags$a("github.com/EmilianoGonLas/diarios-parlamento-uy",
                    href   = "https://github.com/EmilianoGonLas/diarios-parlamento-uy",
                    target = "_blank"),
            " y hac\u00e9 clic en ", tags$code("Code \u2192 Download ZIP"),
            ". Descomprim\u00ed la carpeta donde quieras."
          ),

          # Paso 3
          tags$h5(class = "mt-3",
                   tags$span(class = "badge bg-primary me-2", "3"),
                   "Instal\u00e1 los paquetes necesarios"),
          p("Abr\u00ed el archivo ", tags$code("diarios-parlamento-uy.Rproj"),
            " con RStudio y ejecut\u00e1 esto en la consola:"),
          tags$pre(
            class = "bg-light border rounded p-3",
            tags$code(
'install.packages(c(
  "shiny", "bslib", "DT", "plotly",
  "rvest", "pdftools", "dplyr", "ggplot2",
  "stringr", "purrr", "tidyr", "fs", "zip"
))'
            )
          ),

          # Paso 4
          tags$h5(class = "mt-3",
                   tags$span(class = "badge bg-primary me-2", "4"),
                   "Ejecut\u00e1 la app"),
          tags$pre(
            class = "bg-light border rounded p-3",
            tags$code('shiny::runApp()')
          ),
          p(
            "La app se abrir\u00e1 en tu navegador con la pesta\u00f1a ",
            tags$strong("\U0001f4ca Analizador"),
            " disponible."
          ),

          tags$hr(),
          p(
            class = "text-muted small",
            tags$strong("Tip: "),
            "Los PDFs que descarg\u00e1s desde esta web llegan como un ZIP.",
            " Descomprimilo dentro de la carpeta ", tags$code("sesiones/"),
            " del proyecto y la app los encontrar\u00e1 autom\u00e1ticamente."
          )
        )
      ),

      # Panel lateral: qué incluye la versión local
      bslib::card(
        bslib::card_header("\U0001f4ca Versi\u00f3n local vs. web"),
        bslib::card_body(
          tags$table(
            class = "table table-sm",
            tags$thead(
              tags$tr(
                tags$th("Funcionalidad"),
                tags$th(class = "text-center", "Web"),
                tags$th(class = "text-center", "Local")
              )
            ),
            tags$tbody(
              tags$tr(
                tags$td("Buscar sesiones"),
                tags$td(class = "text-center text-success", "\u2714"),
                tags$td(class = "text-center text-success", "\u2714")
              ),
              tags$tr(
                tags$td("Descargar PDFs (ZIP)"),
                tags$td(class = "text-center text-success", "\u2714"),
                tags$td(class = "text-center text-success", "\u2714")
              ),
              tags$tr(
                tags$td("An\u00e1lisis de menciones"),
                tags$td(class = "text-center text-muted", "\u2014"),
                tags$td(class = "text-center text-success", "\u2714")
              ),
              tags$tr(
                tags$td("Co-ocurrencias"),
                tags$td(class = "text-center text-muted", "\u2014"),
                tags$td(class = "text-center text-success", "\u2714")
              ),
              tags$tr(
                tags$td("Gr\u00e1ficos interactivos"),
                tags$td(class = "text-center text-muted", "\u2014"),
                tags$td(class = "text-center text-success", "\u2714")
              ),
              tags$tr(
                tags$td("Exportar a CSV"),
                tags$td(class = "text-center text-muted", "\u2014"),
                tags$td(class = "text-center text-success", "\u2714")
              ),
              tags$tr(
                tags$td("Sin l\u00edmite de uso"),
                tags$td(class = "text-center text-muted", "\u2014"),
                tags$td(class = "text-center text-success", "\u2714")
              )
            )
          )
        )
      )
    )
  )
}

# ══════════════════════════════════════════════════════════════════════════════
shinyApp(ui, server)
