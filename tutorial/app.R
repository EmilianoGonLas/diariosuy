# tutorial/app.R
# Página de instrucciones publicada en shinyapps.io.
#
# Esta app NO consulta al Parlamento: no podría. shinyapps.io corre en
# servidores de Estados Unidos y parlamento.gub.uy responde 403 a las IP de
# fuera de Uruguay. Su única función es explicar cómo instalar y usar el
# paquete diariosuy, que sí corre desde la máquina del usuario.

library(shiny)
library(bslib)

# ── Piezas reutilizables ──────────────────────────────────────────────────────

# .noWS = "inside" es imprescindible: htmltools indenta el HTML que genera, y
# dentro de un <pre> esos espacios se muestran tal cual, corriendo el código
# hacia la derecha.
#
# El <pre> va envuelto para poder posicionar el botón de copiar arriba a la
# derecha sin que quede dentro del texto que se copia.
bloque_codigo <- function(...) {
  div(
    class = "codigo-caja",
    tags$button(
      class        = "btn-copiar",
      type         = "button",
      `aria-label` = "Copiar el código al portapapeles",
      "Copiar"
    ),
    tags$pre(
      class = "bloque-codigo",
      .noWS = "inside",
      tags$code(.noWS = "inside", paste(c(...), collapse = "\n"))
    )
  )
}

paso <- function(numero, titulo, ...) {
  div(
    class = "paso",
    div(class = "paso-num", numero),
    div(
      class = "paso-cuerpo",
      h3(class = "paso-titulo", titulo),
      ...
    )
  )
}

captura <- function(archivo, epigrafe) {
  tags$figure(
    class = "captura",
    tags$img(src = archivo, alt = epigrafe),
    tags$figcaption(epigrafe)
  )
}

REPO <- "https://github.com/EmilianoGonLas/diariosuy"

# Copiado al portapapeles. La API moderna (navigator.clipboard) sólo existe en
# contextos seguros —https o localhost—; el fallback con execCommand cubre el
# resto y los navegadores viejos.
JS_COPIAR <- '
document.addEventListener("click", function (ev) {
  var boton = ev.target.closest(".btn-copiar");
  if (!boton) return;

  var codigo = boton.parentElement.querySelector("code");
  if (!codigo) return;
  var texto = codigo.innerText;

  function avisar(ok) {
    boton.textContent = ok ? "\u00a1Copiado!" : "No se pudo copiar";
    boton.classList.toggle("copiado", ok);
    setTimeout(function () {
      boton.textContent = "Copiar";
      boton.classList.remove("copiado");
    }, 1800);
  }

  if (navigator.clipboard && window.isSecureContext) {
    navigator.clipboard.writeText(texto).then(function () { avisar(true); },
                                              function () { avisar(false); });
  } else {
    var area = document.createElement("textarea");
    area.value = texto;
    area.style.position = "fixed";
    area.style.opacity = "0";
    document.body.appendChild(area);
    area.select();
    var ok = false;
    try { ok = document.execCommand("copy"); } catch (e) { ok = false; }
    document.body.removeChild(area);
    avisar(ok);
  }
});
'


# ── UI ────────────────────────────────────────────────────────────────────────

ui <- tagList(
  tags$head(
    tags$link(rel = "stylesheet", href = "tutorial.css"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$title("Diarios del Parlamento · Uruguay"),
    tags$script(HTML(JS_COPIAR))
  ),

  bslib::page_fluid(
    theme = bslib::bs_theme(
      bootswatch = "flatly",
      primary    = "#1a4971",
      base_font  = bslib::font_google("Source Sans 3")
    ),

    # ── Encabezado ────────────────────────────────────────────────────────────
    div(
      class = "hero",
      div(
        class = "hero-inner",
        div(class = "hero-icono", "\U0001f3db️"),
        h1(class = "hero-titulo", "Diarios del Parlamento"),
        p(class = "hero-bajada",
          "Buscá cualquier tema en los diarios de sesión del Parlamento uruguayo, ",
          "descargá los PDFs y analizá cuándo se habló de qué."),
        p(class = "hero-nota",
          "Es un paquete de R que corre en tu computadora y se abre en tu navegador."),
        div(
          class = "hero-acciones",
          tags$a(href = REPO, target = "_blank", class = "btn btn-light btn-lg",
                 "Ver el código en GitHub")
        )
      )
    ),

    div(
      class = "contenido",

      # ── Por qué se instala ──────────────────────────────────────────────────
      div(
        class = "aviso",
        div(class = "aviso-icono", "\U0001f1fa\U0001f1fe"),
        div(
          h2(class = "aviso-titulo", "Por qué esto se instala y no es una web"),
          p("El sitio del Parlamento ",
            tags$strong("rechaza las conexiones desde fuera de Uruguay"),
            ": responde ", tags$code("HTTP 403"), " a las IP extranjeras. Una app ",
            "publicada en la web consultar\u00eda el sitio desde el servidor donde ",
            "est\u00e1 alojada, no desde tu conexi\u00f3n, y por eso no obtiene ",
            "resultados."),
          p("Corriendo el paquete en tu propia máquina, la consulta sale desde tu ",
            "conexión y funciona. Por eso ",
            tags$strong("necesitás estar conectado desde Uruguay"),
            " para que el buscador traiga resultados.")
        )
      ),

      # ── Pasos ───────────────────────────────────────────────────────────────
      h2(class = "seccion", "Cómo instalarlo"),

      paso(
        "1", "Tené R instalado",
        p("Hace falta R 4.1 o superior. Si no lo tenés, se baja de ",
          tags$a(href = "https://cran.r-project.org/", target = "_blank", "CRAN"),
          ". Se recomienda usarlo con ",
          tags$a(href = "https://posit.co/download/rstudio-desktop/",
                 target = "_blank", "RStudio"), ".")
      ),

      paso(
        "2", "Sólo en Linux: instalá poppler",
        p("El paquete lee PDFs con ", tags$code("pdftools"), ", que en Linux ",
          "necesita una biblioteca del sistema. En Debian o Ubuntu:"),
        bloque_codigo("sudo apt install libpoppler-cpp-dev"),
        p(class = "nota", "En Windows y macOS no hace falta: ya viene resuelto.")
      ),

      paso(
        "3", "Instalá el paquete",
        p("Desde la consola de R:"),
        bloque_codigo(
          'install.packages("remotes")',
          'remotes::install_github("EmilianoGonLas/diariosuy")'
        ),
        p(class = "nota",
          "La primera vez puede tardar unos minutos: instala las dependencias.")
      ),

      paso(
        "4", "Abrí la app",
        bloque_codigo("library(diariosuy)", "abrir_app()"),
        p("Eso levanta la aplicación y abre tu navegador. Cada vez que la ",
          "quieras usar, alcanza con esas dos líneas.")
      ),

      # ── Uso ─────────────────────────────────────────────────────────────────
      h2(class = "seccion", "Cómo se usa"),

      div(
        class = "uso",
        h3("Buscador"),
        p("Escribís un término —o varios separados por comas—, elegís las ",
          "legislaturas y el cuerpo legislativo, y te devuelve las sesiones que ",
          "lo mencionan. Marcás las que te interesan y las descargás en un ZIP."),
        captura("02-resultados.png",
                "Búsqueda de “aborto” en las legislaturas L y XLIX: 30 sesiones.")
      ),

      div(
        class = "uso",
        h3("Analizador"),
        p("Elegís el ZIP que te dio el Buscador —tal cual, sin descomprimir— o ",
          "una carpeta con PDFs, y escribís el tema. Te devuelve las sesiones ",
          "ordenadas por relevancia, los fragmentos donde aparece el término, ",
          "un gráfico de evolución en el tiempo y otro de co-ocurrencia con ",
          "otros temas que agregues."),
        captura("04-analisis.png",
                "Análisis de seis diarios de sesión para el término “aborto”.")
      ),

      # ── Uso desde R ─────────────────────────────────────────────────────────
      h2(class = "seccion", "También se puede usar sin la app"),
      p("Las funciones están expuestas, así que se pueden llamar directamente ",
        "desde un script:"),
      bloque_codigo(
        "library(diariosuy)",
        "",
        'sesiones <- buscar_sesiones("aborto", camara = "All", lgl_ids = c(49, 50))',
        'descargar_sesiones(sesiones[1:5, ], "pdfs/")',
        'analizar_pdfs("pdfs/", "aborto")  # también acepta un .zip'
      ),

      # ── Advertencias ────────────────────────────────────────────────────────
      h2(class = "seccion", "Lo que conviene saber de la fuente"),
      tags$ul(
        class = "advertencias",
        tags$li(
          tags$strong("Algunos diarios son escaneos sin capa de texto."),
          " En esos casos el análisis va a encontrar menos menciones de las que ",
          "realmente hay, porque el texto no se puede extraer. La mayoría del ",
          "archivo, incluso el de 1990, sí tiene texto extraíble."
        ),
        tags$li(
          tags$strong("El buscador consulta el índice oficial en vivo."),
          " Los resultados son los que devuelve el buscador del Parlamento, con ",
          "sus propios criterios."
        )
      ),

      # ── Pie ─────────────────────────────────────────────────────────────────
      div(
        class = "pie",
        p(tags$a(href = REPO, target = "_blank", "Código y reporte de errores"),
          " · Datos: ",
          tags$a(href = "https://parlamento.gub.uy/documentosyleyes/documentos/diarios-de-sesion",
                 target = "_blank", "Parlamento del Uruguay")),
        p(class = "nota", "Licencia MIT.")
      )
    )
  )
)

server <- function(input, output, session) {
  # La página es estática: no hay nada que calcular del lado del servidor.
}

shinyApp(ui, server)
