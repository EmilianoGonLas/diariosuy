# deploy.R — publica la página de instrucciones en shinyapps.io
#
# Ojo: lo que se publica es tutorial/, no el paquete. La app del buscador ya no
# se publica porque no puede funcionar en la nube: shinyapps.io corre en
# Estados Unidos y parlamento.gub.uy responde 403 a las IP de fuera de Uruguay.
# Lo que queda online es la página que explica cómo instalar y usar el paquete.
#
# Uso:  Rscript deploy.R
#
# Requisito: haber corrido una vez rsconnect::setAccountInfo(...) en esta
# máquina (queda en ~/.config/R/rsconnect y NO se versiona).

library(rsconnect)

deployApp(
  appDir      = "tutorial",
  appName     = "diariosuy",
  account     = "emilianogonzalez",
  server      = "shinyapps.io",
  appFiles    = c("app.R", "www"),
  forceUpdate = TRUE,
  launch.browser = FALSE
)
