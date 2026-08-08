# Diarios del Parlamento · Uruguay

Aplicación **Shiny** para **buscar, descargar y analizar** los diarios de sesión del
Parlamento uruguayo ([parlamento.gub.uy](https://parlamento.gub.uy)) por palabra clave.

Permite rastrear cómo y cuándo se discutió un tema en el recinto (por ejemplo
**forestación**, cannabis, aborto, vivienda…), a lo largo de las legislaturas
disponibles en línea (desde la XLII, 1985, hasta la actual).

> **Relación con [`speech`](https://github.com/Nicolas-Schmidt/speech)**
> El paquete `speech` (Schmidt, Luján y Moraes) convierte las actas parlamentarias
> en un dataset *tidy* a nivel de intervención por legislador. Esta app ataca la
> **misma fuente** pero desde un ángulo distinto y complementario: es una
> **herramienta interactiva** de descubrimiento (búsqueda a texto completo,
> descarga masiva de PDFs y análisis exploratorio de menciones y co-ocurrencias),
> pensada para quien no programa en R. Donde `speech` produce datos estructurados
> para modelar, esta app produce evidencia navegable para explorar.

---

## Qué hace

**1. Buscador** (funciona online y local)
- Busca uno o varios términos (separados por coma) en el buscador oficial del Parlamento.
- Filtra por cuerpo (Asamblea General, Comisión Permanente, Diputados, Senadores) y por legislatura.
- Devuelve las sesiones en una tabla interactiva y permite **descargar los PDFs seleccionados como ZIP**.

**2. Analizador** (recomendado en local — ver más abajo)
- Sobre una carpeta de PDFs descargados, busca un **tema principal** y **temas de comparación** opcionales.
- Extrae texto respetando el orden de lectura en documentos a **doble columna**.
- Calcula menciones por página, **co-ocurrencias** (misma página) y **proximidad** (≤500 caracteres).
- Visualiza la **evolución temporal** y las **co-ocurrencias**, muestra fragmentos de contexto y exporta a CSV.

## Web vs. local

| Funcionalidad            | Web (shinyapps.io) | Local |
|--------------------------|:------------------:|:-----:|
| Buscar sesiones          | ✔ | ✔ |
| Descargar PDFs (ZIP)     | ✔ | ✔ |
| Análisis de menciones    | ✔ (acotado) | ✔ |
| Co-ocurrencias / gráficos| ✔ (acotado) | ✔ |
| Exportar a CSV           | ✔ | ✔ |
| Sin límite de volumen    | — | ✔ |

El análisis carga y parsea los PDFs en memoria. En el servidor web (recursos
limitados) esto se **acota** a un número/tamaño máximo de archivos para no saturar
la instancia; para volúmenes grandes (cientos de MB) conviene correr la app en
tu propia computadora, donde no hay límites.

## Correr en local

Requisitos: **R ≥ 4.2** y la librería del sistema **poppler** (para `pdftools`).

```r
# Desde la carpeta del proyecto (abrí diarios-parlamento-uy.Rproj en RStudio)
# global.R instala automáticamente los paquetes que falten la primera vez.
shiny::runApp()
```

## Estructura

```
diarios-parlamento-uy/
├── app.R              # UI + server (Shiny)
├── global.R           # carga de paquetes, entorno, tabla de legislaturas
├── R/
│   ├── scraper.R      # búsqueda de sesiones + resolución del PDF
│   ├── downloader.R   # descarga de PDFs + empaquetado ZIP
│   ├── analizar.R     # extracción de texto y análisis de menciones/co-ocurrencias
│   └── visualizar.R   # gráficos (plotly)
├── www/styles.css     # estilos
└── deploy.R           # publicación a shinyapps.io (sin credenciales)
```

## Fuente / datos

Los PDFs no se versionan (se re-descargan desde el Parlamento con el Buscador).
Carpeta de trabajo original del proyecto en Drive:
`Proyectos academicos / Forestacion / diarios-parlamento-uy`.

## Licencia / uso

Uso académico. Los diarios de sesión son documentos públicos del Parlamento uruguayo.
