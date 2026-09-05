# R/app_server.R
# Lógica de servidor de la app.

#' Servidor de la app
#'
#' @param input,output,session Objetos que provee Shiny.
#' @keywords internal
app_server <- function(input, output, session) {

  rv <- reactiveValues(
    resultados       = NULL,  # tibble: sesiones encontradas por el buscador
    analisis_res     = NULL,  # tibble: resultados del análisis de PDFs
    carpeta_analisis = NULL,  # ruta de la carpeta analizada (para abrir PDFs)
    terminos_ids     = c(),   # IDs de los términos de comparación activos
    contador         = 0,     # contador para generar IDs únicos
    ultimo_dl        = NULL,  # estadísticas del último ZIP descargado
    dir_pdfs         = NULL,  # directorio de PDFs subidos (solo en versión web)
    errores_busqueda = NULL,  # motivos de fallo de la última búsqueda (red/HTTP)
    origen_pdfs      = NULL   # carpeta o .zip elegido para analizar (modo local)
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
      # bind_rows descarta atributos: guardar los errores ANTES de combinar.
      rv$errores_busqueda <- unique(unlist(purrr::map(todos, ~ attr(.x, "errores"))))
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
      # Distinguir "el sitio no devolvió nada" de "no pudimos consultar el sitio".
      # El segundo caso es el habitual cuando la app corre fuera de Uruguay
      # (shinyapps.io está en EE.UU. y parlamento.gub.uy bloquea IPs no uruguayas).
      if (length(rv$errores_busqueda) > 0) {
        return(
          bslib::card(
            bslib::card_body(
              div(class = "py-3",
                p(class = "text-danger fw-bold mb-2",
                  "\u26d4 No se pudo consultar el sitio del Parlamento."),
                tags$ul(class = "text-muted small",
                        lapply(rv$errores_busqueda, function(e) tags$li(e))),
                p(class = "text-muted small mb-0",
                  "parlamento.gub.uy restringe el acceso desde IPs de fuera de Uruguay, ",
                  "y este servidor est\u00e1 en el exterior. Para buscar y descargar, ",
                  "correr la app en local desde una conexi\u00f3n uruguaya ",
                  "(ver README). La pesta\u00f1a Analizador s\u00ed funciona ac\u00e1: ",
                  "sub\u00ed los PDFs que ya tengas.")
              )
            )
          )
        )
      }
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
    # Raíces del selector de carpetas. USERPROFILE sólo existe en Windows, así
    # que partimos de "~" —que R resuelve en los tres sistemas— y nos quedamos
    # únicamente con las carpetas que existan: pasarle a shinyFiles una ruta
    # inexistente deja el selector inutilizable.
    inicio <- path.expand("~")
    candidatas <- c(
      `Carpeta actual`   = getwd(),
      `Carpeta personal` = inicio,
      Escritorio         = file.path(inicio, "Desktop"),
      Escritorio         = file.path(inicio, "Escritorio"),
      Documentos         = file.path(inicio, "Documents"),
      Documentos         = file.path(inicio, "Documentos"),
      Descargas          = file.path(inicio, "Downloads"),
      Descargas          = file.path(inicio, "Descargas")
    )
    candidatas     <- candidatas[dir.exists(candidatas)]
    candidatas     <- candidatas[!duplicated(candidatas)]
    roots_locales  <- stats::setNames(normalizePath(candidatas), names(candidatas))
    shinyFiles::shinyDirChoose(input, "btn_carpeta", roots = roots_locales,
                                allowDirCreate = FALSE)
    shinyFiles::shinyFileChoose(input, "btn_zip", roots = roots_locales,
                                 filetypes = c("zip"))

    # Las dos formas de elegir origen escriben en el mismo lugar, y gana la
    # última que se haya usado.
    observeEvent(input$btn_carpeta, {
      p <- shinyFiles::parseDirPath(roots_locales, input$btn_carpeta)
      if (length(p) == 1 && nzchar(p)) rv$origen_pdfs <- as.character(p)
    })

    observeEvent(input$btn_zip, {
      sel <- shinyFiles::parseFilePaths(roots_locales, input$btn_zip)
      if (nrow(sel) == 0) return()
      rv$origen_pdfs <- as.character(sel$datapath[1])
    })
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

  # Reactivo unificado: devuelve el origen de los PDFs sin importar el entorno.
  # En local puede ser una carpeta o un .zip; analizar_pdfs() acepta las dos.
  carpeta_pdfs <- reactive({
    if (ES_LOCAL) rv$origen_pdfs else rv$dir_pdfs
  })

  output$lbl_carpeta <- renderText({
    if (!ES_LOCAL) return("")
    p <- carpeta_pdfs()
    if (is.null(p)) "Nada seleccionado todav\u00eda" else p
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

    # Marcar duplicados: mismo día Y mismos fragmentos de texto.
    # Antes alcanzaba con coincidir en cantidad de páginas y score, pero eso
    # marcaba como duplicadas a sesiones distintas: con un término poco
    # frecuente casi todos los diarios tienen una sola mención y el mismo
    # score. Pedir que el texto encontrado sea idéntico exige evidencia real
    # de que se trata del mismo contenido publicado dos veces.
    df <- df %>%
      group_by(fecha, fragmentos_texto) %>%
      mutate(
        es_duplicado = n() > 1 &
          paginas_con_termino > 0 &
          nzchar(fragmentos_texto)
      ) %>%
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
          ruta
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
