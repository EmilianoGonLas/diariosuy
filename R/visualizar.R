# R/visualizar.R
# Funciones de visualización sobre los resultados del análisis.

#' Gráfico interactivo de menciones a lo largo del tiempo
#'
#' @param datos  tibble resultado de analizar_pdfs()
#' @param titulo Título del gráfico
#' @return objeto plotly
#' @export
grafico_timeline <- function(datos, titulo = "Menciones del término en el tiempo") {

  datos_ok <- datos %>%
    filter(estado == "OK", paginas_con_termino > 0)

  if (nrow(datos_ok) == 0) {
    return(plotly::plotly_empty() %>%
             plotly::layout(title = "Sin datos para mostrar"))
  }

  # Si hay fechas válidas usarlas; si no, usar el nombre del archivo como eje
  tiene_fechas <- sum(!is.na(datos_ok$fecha)) > 0

  if (tiene_fechas) {
    datos_ok <- datos_ok %>% filter(!is.na(fecha)) %>% arrange(fecha)
    eje_x  <- "fecha"
    x_lab  <- NULL
  } else {
    datos_ok <- datos_ok %>% arrange(desc(paginas_con_termino))
    datos_ok$eje <- seq_len(nrow(datos_ok))
    eje_x  <- "eje"
    x_lab  <- "Sesión (ordenadas por menciones)"
  }

  datos_ok <- datos_ok %>%
    mutate(
      tooltip = paste0(
        if (tiene_fechas) paste0("Fecha: ", fecha, "\n") else paste0("Archivo: ", archivo, "\n"),
        "Legislatura: ",    ifelse(is.na(legislatura), "desconocida", legislatura), "\n",
        "P\u00e1ginas con menci\u00f3n: ", paginas_con_termino,
        if ("total_cruces" %in% names(datos_ok))
          paste0("\nCruces con otros temas: ", total_cruces) else ""
      )
    )

  p <- ggplot2::ggplot(
    datos_ok,
    ggplot2::aes_string(
      x    = eje_x,
      y    = "paginas_con_termino",
      text = "tooltip"
    )
  ) +
    ggplot2::geom_col(fill = "#1a5276", alpha = 0.85, width = if (tiene_fechas) 20 else 0.7) +
    ggplot2::labs(title = titulo, x = x_lab, y = "P\u00e1ginas con menci\u00f3n") +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 14))

  # Línea de tendencia solo si hay suficientes puntos y fechas
  if (tiene_fechas && nrow(datos_ok) >= 5) {
    p <- p + ggplot2::geom_smooth(
      ggplot2::aes_string(x = eje_x, y = "paginas_con_termino"),
      method    = "loess",
      formula   = y ~ x,
      se        = FALSE,
      color     = "#e74c3c",
      linewidth = 0.9
    )
  }

  plotly::ggplotly(p, tooltip = "text")
}

#' Gráfico de co-ocurrencias entre el término principal y términos secundarios
#'
#' @param datos          tibble resultado de analizar_pdfs()
#' @param terminos_cruce lista nombrada usada en el análisis
#' @return objeto plotly o NULL si no hay términos de cruce
#' @export
grafico_coocurrencias <- function(datos, terminos_cruce) {
  if (length(terminos_cruce) == 0) return(NULL)

  cols_cruce      <- paste0("cruce_", names(terminos_cruce))
  cols_existentes <- intersect(cols_cruce, names(datos))
  if (length(cols_existentes) == 0) return(NULL)

  tiene_fechas <- sum(!is.na(datos$fecha)) > 0

  datos_base <- datos %>% filter(estado == "OK")

  if (tiene_fechas) {
    datos_base <- datos_base %>%
      filter(!is.na(fecha)) %>%
      mutate(eje = fecha)
  } else {
    datos_base <- datos_base %>%
      arrange(desc(total_cruces)) %>%
      mutate(eje = seq_len(n()))
  }

  datos_largo <- datos_base %>%
    select(eje, legislatura, archivo, all_of(cols_existentes)) %>%
    tidyr::pivot_longer(
      cols      = all_of(cols_existentes),
      names_to  = "termino",
      values_to = "paginas"
    ) %>%
    mutate(termino = str_remove(termino, "^cruce_") %>% str_to_title()) %>%
    filter(paginas > 0)

  if (nrow(datos_largo) == 0) {
    return(plotly::plotly_empty() %>%
             plotly::layout(title = "No se encontraron sesiones donde ambos temas aparezcan juntos"))
  }

  datos_largo <- datos_largo %>%
    mutate(
      tooltip = paste0(
        if (tiene_fechas) paste0("Fecha: ", eje, "\n") else paste0("Sesi\u00f3n #", eje, "\n"),
        "Tema comparado: ", termino, "\n",
        "P\u00e1ginas en com\u00fan: ", paginas
      )
    )

  p <- ggplot2::ggplot(
    datos_largo,
    ggplot2::aes_string(
      x    = "eje",
      y    = "paginas",
      fill = "termino",
      text = "tooltip"
    )
  ) +
    ggplot2::geom_col(position = "dodge", alpha = 0.85) +
    ggplot2::labs(
      title = "Sesiones donde el tema principal y los temas de comparaci\u00f3n aparecen juntos",
      x     = if (tiene_fechas) NULL else "Sesi\u00f3n",
      y     = "P\u00e1ginas en com\u00fan",
      fill  = "Tema"
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::scale_fill_brewer(palette = "Set2")

  plotly::ggplotly(p, tooltip = "text")
}
