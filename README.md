# diariosuy

Buscador y analizador de los **diarios de sesión del Parlamento uruguayo**, como
paquete de R. Busca sesiones por palabra clave en el sitio oficial, descarga los
PDFs y analiza sus textos: en qué sesiones aparece un tema, cuándo se mencionó y
junto a qué otros temas.

La app corre en tu computadora y se abre en tu navegador.

## Por qué se instala en vez de usarse en una web

`parlamento.gub.uy` **rechaza las conexiones desde fuera de Uruguay**: responde
HTTP 403 a las IP extranjeras. Cualquier servidor de hosting gratuito está en el
exterior, así que una versión publicada en la web no puede consultar el sitio
oficial. Corriendo el paquete en tu propia máquina, la consulta sale desde tu
conexión y funciona.

O sea: **necesitás estar conectado desde Uruguay** para que el buscador traiga
resultados. Si lo corrés desde afuera, la app te lo dice explícitamente en vez de
mostrar una búsqueda vacía.

## Instalación

Requiere R 4.1 o superior.

```r
install.packages("remotes")
remotes::install_github("EmilianoGonLas/diariosuy")
```

En **Linux** hay que instalar antes la biblioteca que usa `pdftools` para leer
PDFs. En Debian/Ubuntu:

```bash
sudo apt install libpoppler-cpp-dev
```

En Windows y macOS no hace falta nada extra: viene en el binario de CRAN.

## Uso

```r
library(diariosuy)
abrir_app()
```

Eso levanta la app y abre el navegador.

### Buscador

Escribís un término —o varios separados por comas— elegís las legislaturas y el
cuerpo, y te devuelve las sesiones que lo mencionan. Seleccionás las que te
interesan y las descargás en un ZIP.

### Analizador

Apuntás a la carpeta con los PDFs descargados y elegís el tema a analizar. Te
devuelve una tabla con las sesiones ordenadas por relevancia, los fragmentos
donde aparece el término, un gráfico de evolución en el tiempo y otro de
co-ocurrencia con otros temas que agregues.

## Advertencias sobre la fuente

- **Algunos diarios son escaneos sin capa de texto.** El análisis de esos
  documentos va a encontrar menos menciones de las que realmente hay, porque el
  texto no se puede extraer. La mayoría del archivo, incluso el de 1990, sí
  tiene texto extraíble, pero hay excepciones.
- El buscador consulta el índice oficial del Parlamento en vivo, así que los
  resultados son los que devuelve ese buscador, con sus criterios.

## Uso desde R, sin la app

Las funciones se pueden usar directamente:

```r
library(diariosuy)

sesiones <- buscar_sesiones("aborto", camara = "All", lgl_ids = c(49, 50))
descargar_sesiones(sesiones[1:5, ], "pdfs/")
analizar_pdfs("pdfs/", "aborto")
```

## Licencia

MIT.
