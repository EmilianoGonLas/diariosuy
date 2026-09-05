# R/abrir_app.R

#' Abrir la app de diarios del Parlamento
#'
#' Levanta la aplicación Shiny y la abre en el navegador. Todas las consultas
#' salen desde la máquina donde se ejecuta, que es lo que permite llegar al
#' sitio del Parlamento: `parlamento.gub.uy` responde 403 a las IP de fuera de
#' Uruguay, así que la app necesita correr desde una conexión uruguaya.
#'
#' @param puerto Puerto donde levantar la app. Por defecto Shiny elige uno libre.
#' @param navegador Si es `TRUE` (por defecto) abre el navegador automáticamente.
#'
#' @return Se invoca por su efecto: corre la app hasta que se la interrumpe.
#'
#' @examples
#' if (interactive()) {
#'   abrir_app()
#' }
#'
#' @export
abrir_app <- function(puerto = NULL, navegador = TRUE) {
  shiny::addResourcePath("diariosuy", system.file("www", package = "diariosuy"))

  app <- shiny::shinyApp(ui = app_ui(), server = app_server)

  shiny::runApp(
    app,
    port           = puerto,
    launch.browser = navegador
  )
}
