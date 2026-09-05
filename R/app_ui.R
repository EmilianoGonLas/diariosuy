# R/app_ui.R
# Interfaz de la app. Se arma dentro de una función (y no como objeto de nivel
# superior) porque en un paquete el UI tiene que construirse en cada arranque.

#' Construye la interfaz de la app
#'
#' @return Un `shiny.tag.list` con la interfaz completa.
#' @keywords internal
app_ui <- function() {
  tagList(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "diariosuy/styles.css"),
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
        href   = "https://github.com/EmilianoGonLas/diariosuy",
        target = "_blank",
        style  = "font-size:.88rem;",
        icon("github", class = "me-1"), "GitHub"
      )
    )
  )) # cierre de page_navbar y tagList
}
