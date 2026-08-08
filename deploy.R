# deploy.R — publica la app en shinyapps.io
# Usa la cuenta rsconnect ya registrada localmente (el token NO vive en el repo).
# Uso:  Rscript deploy.R
#
# Requisito: haber corrido una vez rsconnect::setAccountInfo(...) en esta máquina
# (queda guardado en ~/.config/R/rsconnect y NO se versiona).

library(rsconnect)

deployApp(
  appDir     = ".",
  appName    = "diarios-parlamento-uy",
  account    = "emilianogonzalez",
  server     = "shinyapps.io",
  appFiles   = c("app.R", "global.R", "R", "www"),  # solo lo necesario para correr
  forceUpdate = TRUE,
  launch.browser = FALSE
)
