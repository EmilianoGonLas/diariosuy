# R/ui_helpers.R
# Piezas de UI que el server inserta dinámicamente.

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
