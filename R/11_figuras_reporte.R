library(tidyverse)

# Figuras y resúmenes del reporte (reporte.qmd). Dependen de las funciones de
# R/08_tipos_error.R (`grupo_sustitucion()`, `nombre_caracter()`).

# Colores: azul de acento y dos paletas daltónico-seguras (Okabe-Ito)
azul_reporte <- "#1F4E79"
paleta_operacion <- c(
  "Sustitución" = "#0072B2", "Omisión" = "#D55E00", "Inserción" = "#009E73"
)
paleta_grupo <- c(
  "Confusión entre letras" = "#56B4E9", "Letra por dígito" = "#D55E00",
  "Puntuación" = "#CC79A7", "Tildes y diacríticos" = "#009E73",
  "Comillas y apóstrofos" = "#0072B2", "Otras" = "#999999"
)

#' Tema común de las figuras: fondo blanco, rejilla vertical suave, títulos de
#' panel en negrita alineados a la izquierda
tema_reporte <- function(base_size = 11) {
  theme_minimal(base_size = base_size, base_family = "Helvetica") +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = "grey92", linewidth = 0.4),
      strip.text = element_text(face = "bold", hjust = 0, size = base_size - 1),
      axis.title = element_text(size = base_size),
      axis.text = element_text(color = "grey25"),
      legend.position = "bottom",
      plot.margin = margin(8, 12, 8, 8)
    )
}

#' Resumen de la distribución de una tasa entre documentos, por motor y métrica
#'
#' @param tabla Tibble con `motor`, `doc_id`, `metrica` (CER/WER) y `valor`.
resumen_dispersion <- function(tabla) {
  tabla %>%
    group_by(motor, metrica) %>%
    summarise(
      documentos = n(),
      media = mean(valor),
      mediana = median(valor),
      q1 = quantile(valor, 0.25, names = FALSE),
      q3 = quantile(valor, 0.75, names = FALSE),
      iqr = q3 - q1,
      p90 = quantile(valor, 0.90, names = FALSE),
      maximo = max(valor),
      .groups = "drop"
    )
}

#' Figura: mediana y rango intercuartílico del CER y del WER por motor
grafico_cer_wer <- function(disp, etiquetas) {
  disp %>%
    mutate(motor = fct_rev(factor(etiquetas[motor], levels = unname(etiquetas)))) %>%
    ggplot(aes(y = motor)) +
    geom_linerange(aes(xmin = q1, xmax = q3), color = azul_reporte, linewidth = 0.8) +
    geom_point(aes(x = mediana), color = azul_reporte, size = 3.6) +
    facet_wrap(~metrica, scales = "free_x") +
    scale_x_continuous(labels = scales::label_percent(accuracy = 0.1, decimal.mark = ","),
                       limits = c(0, NA), expand = expansion(mult = c(0, 0.08))) +
    labs(x = "Mediana y rango intercuartílico entre documentos", y = NULL) +
    tema_reporte() +
    theme(panel.spacing.x = unit(1.6, "cm"))
}

#' Figura: composición del error (sustitución, omisión, inserción) por motor
#'
#' @param comp Tibble con `motor`, `operacion`, `n`.
grafico_composicion <- function(comp, etiquetas) {
  comp <- comp %>%
    group_by(motor) %>%
    mutate(prop = n / sum(n), total = sum(n)) %>%
    ungroup() %>%
    mutate(
      motor = fct_rev(factor(etiquetas[motor], levels = unname(etiquetas))),
      operacion = factor(operacion, levels = names(paleta_operacion))
    )
  totales <- distinct(comp, motor, total)

  ggplot(comp, aes(x = prop, y = motor, fill = operacion)) +
    geom_col(width = 0.55, position = position_stack(reverse = TRUE)) +
    geom_text(
      data = filter(comp, prop >= 0.08),
      aes(label = scales::percent(prop, accuracy = 1)),
      position = position_stack(vjust = 0.5, reverse = TRUE),
      color = "white", fontface = "bold", size = 3.6
    ) +
    geom_text(
      data = totales, aes(x = 1.02, y = motor, label = format(total, big.mark = ".")),
      inherit.aes = FALSE, hjust = 0, size = 3.6, color = "grey25"
    ) +
    scale_fill_manual(values = paleta_operacion) +
    scale_x_continuous(labels = scales::label_percent(), breaks = seq(0, 1, 0.25)) +
    coord_cartesian(xlim = c(0, 1), clip = "off") +
    labs(x = "Proporción de las operaciones de edición", y = NULL, fill = "Tipo de error") +
    tema_reporte() +
    theme(plot.margin = margin(8, 40, 8, 8))
}

#' Sustituciones más frecuentes, con su grupo y una etiqueta legible
#'
#' @param errores Tibble producido por `errores_por_tipo()`.
top_sustituciones <- function(errores, top = 8) {
  errores %>%
    filter(operacion == "sustitución") %>%
    mutate(
      grupo = grupo_sustitucion(ref_char, hip_char),
      referencia = nombre_caracter(ref_char),
      ocr = nombre_caracter(hip_char)
    ) %>%
    count(motor, referencia, ocr, grupo, name = "n") %>%
    group_by(motor) %>%
    slice_max(n, n = top, with_ties = FALSE) %>%
    ungroup() %>%
    arrange(motor, desc(n))
}

#' Figura: sustituciones más frecuentes por motor, coloreadas por grupo
grafico_sustituciones <- function(top, etiquetas) {
  top %>%
    mutate(
      motor = factor(etiquetas[motor], levels = unname(etiquetas)),
      par = paste(referencia, "→", ocr),
      grupo = factor(grupo, levels = names(paleta_grupo))
    ) %>%
    group_by(motor) %>%
    mutate(par = fct_reorder(par, n, .fun = max)) %>%
    ungroup() %>%
    ggplot(aes(x = n, y = par, fill = grupo)) +
    geom_col(width = 0.7) +
    geom_text(aes(label = n), hjust = -0.25, size = 3.4, color = "grey25") +
    facet_wrap(~motor, scales = "free") +
    scale_fill_manual(values = paleta_grupo, drop = TRUE) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
    labs(
      x = "Sustituciones (conteo sobre los documentos)",
      y = "Referencia → ICR", fill = "Tipo de sustitución"
    ) +
    tema_reporte() +
    theme(legend.title = element_text(size = 10))
}

categorias_entidad <- c(
  fecha = "Fechas", numero = "Números", nombre_propio = "Nombres propios",
  codigo_administrativo = "Códigos administrativos"
)

#' Figura: preservación de entidades por motor y categoría (mapa de calor)
#'
#' @param tabla Tibble de `preservacion_entidades()`.
grafico_preservacion <- function(tabla, etiquetas) {
  tabla <- tabla %>%
    mutate(
      categoria = factor(categorias_entidad[categoria], levels = unname(categorias_entidad)),
      motor = fct_rev(factor(etiquetas[motor], levels = unname(etiquetas)))
    )
  n_cat <- tabla %>%
    group_by(categoria) %>%
    summarise(n = max(tokens_totales), .groups = "drop") %>%
    mutate(nombre = paste0(categoria, "\n(n = ", n, ")"))
  tabla <- tabla %>%
    left_join(n_cat, by = "categoria") %>%
    mutate(nombre = factor(nombre, levels = n_cat$nombre))

  ggplot(tabla, aes(x = nombre, y = motor, fill = tasa_preservacion)) +
    geom_tile(color = "white", linewidth = 1.2) +
    geom_text(
      aes(
        label = paste0(round(100 * tasa_preservacion), "%\n", tokens_preservados, "/", tokens_totales),
        color = tasa_preservacion > 0.7
      ),
      size = 3.6, lineheight = 0.95
    ) +
    scale_color_manual(values = c("TRUE" = "white", "FALSE" = "grey15"), guide = "none") +
    scale_fill_gradient(
      low = "#E4ECF3", high = "#1F5B85", limits = c(0, 1),
      labels = scales::label_percent(), name = "Entidades\npreservadas"
    ) +
    scale_x_discrete(position = "top") +
    labs(x = NULL, y = NULL) +
    tema_reporte() +
    theme(
      panel.grid = element_blank(), panel.grid.major.x = element_blank(),
      legend.position = "right", axis.ticks = element_blank(),
      axis.text.x = element_text(size = 9.5, color = "grey25")
    )
}

#' Figura: distribución por documento del CER y del WER, con el peor rotulado
#'
#' @param tabla Tibble con `motor`, `doc_id`, `metrica`, `valor`.
grafico_dispersion <- function(tabla, etiquetas) {
  tabla <- tabla %>%
    mutate(motor = factor(etiquetas[motor], levels = unname(etiquetas)))
  peores <- tabla %>%
    group_by(motor, metrica) %>%
    slice_max(valor, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    mutate(etiqueta = paste0("Doc_", doc_id))

  ggplot(tabla, aes(x = motor, y = valor)) +
    geom_boxplot(outlier.shape = NA, fill = "white", color = azul_reporte, width = 0.3) +
    geom_point(position = position_jitter(width = 0.12, height = 0, seed = 2026), alpha = 0.6, size = 1.6, color = azul_reporte) +
    ggrepel::geom_text_repel(
      data = peores, aes(label = etiqueta), size = 3.3, color = "grey25",
      nudge_x = 0.28, segment.color = "grey60", min.segment.length = 0, seed = 2026
    ) +
    facet_wrap(~metrica, scales = "free_y") +
    scale_x_discrete(expand = expansion(add = 1)) +
    scale_y_continuous(labels = scales::label_percent(), limits = c(0, NA)) +
    labs(x = NULL, y = "Error por documento") +
    tema_reporte() +
    theme(panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(color = "grey92"))
}

#' Figura: flujo del análisis (documentos -> motores -> comparaciones -> análisis)
grafico_flujo <- function(n_docs, n_motores, nombres_motores) {
  cajas <- tibble(
    x = c(1, 3.3, 5.6, 7.9),
    cifra = c(n_docs, n_motores, n_docs * n_motores, 3),
    texto = c(
      "documentos\n(una línea por documento)",
      paste0("motor", if (n_motores > 1) "es", " ICR\n", paste(nombres_motores, collapse = " · ")),
      "comparaciones\nreferencia frente a\nsalida ICR",
      "análisis por motor\ncarácter · entidades\nestabilidad"
    )
  )
  ggplot(cajas) +
    geom_rect(
      aes(xmin = x - 1, xmax = x + 1, ymin = 0, ymax = 2),
      fill = "#F7F8F9", color = "#B9BDC3", linewidth = 0.5
    ) +
    geom_text(aes(x = x, y = 1.42, label = cifra), size = 9.5, fontface = "bold", family = "Helvetica") +
    geom_text(aes(x = x, y = 0.6, label = texto), size = 3.1, lineheight = 0.95, color = "grey25", family = "Helvetica") +
    geom_segment(
      data = tibble(x = cajas$x[-4] + 1.06, xend = cajas$x[-1] - 1.06),
      aes(x = x, xend = xend, y = 1, yend = 1),
      arrow = arrow(length = unit(0.12, "inches")), color = "grey40"
    ) +
    coord_cartesian(xlim = c(-0.1, 8.9), ylim = c(-0.1, 2.1)) +
    theme_void()
}

#' Símbolo legible de un carácter para los ejes de la matriz de confusión
#'
#' `NA` (omisión en la salida o inserción en la referencia) se muestra como
#' "∅" y el espacio como "␣".
simbolo_caracter <- function(x) {
  case_when(is.na(x) ~ "\u2205", x == " " ~ "\u2423", TRUE ~ x)
}

#' Matriz de confusión de caracteres de un motor, con omisiones e inserciones
#'
#' A diferencia de `matriz_confusion()`, que solo cuenta sustituciones, aquí
#' cada error es una celda: una sustitución es un par (referencia, salida), una
#' omisión es (carácter, ∅) y una inserción es (∅, carácter). Para que sea
#' legible se conservan los `top` caracteres con más errores de cada lado.
#'
#' @param errores Tibble producido por `errores_por_tipo()`.
#' @param motor_filtro Motor a graficar.
#' @param top Número de caracteres de referencia y de salida que se muestran.
matriz_confusion_completa <- function(errores, motor_filtro, top = 20) {
  e <- errores %>%
    filter(motor == motor_filtro) %>%
    mutate(ref = simbolo_caracter(ref_char), hip = simbolo_caracter(hip_char)) %>%
    count(ref, hip, name = "n")
  mantener <- function(col) {
    e %>% filter(.data[[col]] != "\u2205") %>%
      count(.data[[col]], wt = n, sort = TRUE) %>%
      slice_head(n = top) %>% pull(1)
  }
  e %>% filter(ref %in% c(mantener("ref"), "\u2205"), hip %in% c(mantener("hip"), "\u2205"))
}

#' Figura: matriz de confusión de caracteres (mapa de calor con el conteo)
grafico_matriz_confusion <- function(mc) {
  orden_ref <- mc %>% count(ref, wt = n, sort = TRUE) %>% pull(ref)
  orden_hip <- mc %>% count(hip, wt = n, sort = TRUE) %>% pull(hip)
  mc %>%
    mutate(
      ref = factor(ref, levels = rev(orden_ref)),
      hip = factor(hip, levels = orden_hip),
      operacion = case_when(
        ref == "\u2205" ~ "Inserción", hip == "\u2205" ~ "Omisión", TRUE ~ "Sustitución"
      ),
      operacion = factor(operacion, levels = names(paleta_operacion))
    ) %>%
    ggplot(aes(x = hip, y = ref, fill = n)) +
    geom_tile(color = "white", linewidth = 0.6) +
    geom_text(aes(label = n, color = n > 0.5 * max(n)), size = 3) +
    scale_color_manual(values = c("TRUE" = "white", "FALSE" = "grey15"), guide = "none") +
    scale_fill_gradient(low = "#E4ECF3", high = "#1F5B85", name = "Errores", trans = "sqrt") +
    scale_x_discrete(position = "top") +
    labs(x = "Carácter en la salida ICR (\u2205: omisión)", y = "Carácter en la referencia (\u2205: inserción)") +
    tema_reporte() +
    theme(
      panel.grid = element_blank(), legend.position = "right",
      axis.title.x = element_text(margin = margin(b = 6)), axis.ticks = element_blank()
    )
}

#' Caracteres más omitidos o insertados de un motor
#'
#' @param errores Tibble producido por `errores_por_tipo()`.
#' @return Tibble con `operacion` ("omisión" o "inserción"), `caracter` (su
#'   nombre legible) y `n`, los `top` más frecuentes de cada operación.
top_omisiones_inserciones <- function(errores, motor_filtro, top = 5) {
  errores %>%
    filter(motor == motor_filtro, operacion %in% c("omisión", "inserción")) %>%
    mutate(caracter = nombre_caracter(if_else(operacion == "omisión", ref_char, hip_char))) %>%
    count(operacion, caracter, name = "n") %>%
    group_by(operacion) %>%
    slice_max(n, n = top, with_ties = FALSE) %>%
    ungroup() %>%
    arrange(operacion, desc(n))
}

#' Figura: preservación de entidades en el corpus OCR y en el ICR, por categoría
#'
#' @param tabla Tibble con `categoria`, `corpus`, `preservadas` y `total`.
grafico_contraste_entidades <- function(tabla) {
  tabla %>%
    mutate(
      categoria = factor(categorias_entidad[categoria], levels = unname(categorias_entidad)),
      tasa = preservadas / total
    ) %>%
    ggplot(aes(x = tasa, y = fct_rev(categoria), fill = corpus)) +
    geom_col(position = position_dodge(width = 0.75), width = 0.68) +
    geom_text(
      aes(label = paste0(round(100 * tasa), "%  (", preservadas, "/", total, ")")),
      position = position_dodge(width = 0.75), hjust = -0.08, size = 3.3, color = "grey20"
    ) +
    scale_fill_manual(values = c(setNames(c("#8FB3D1", azul_reporte), levels(factor(tabla$corpus))))) +
    scale_x_continuous(labels = scales::label_percent(), limits = c(0, 1.32), breaks = seq(0, 1, 0.25)) +
    labs(x = "Entidades de la referencia presentes en la salida de OlmOCR", y = NULL, fill = NULL) +
    tema_reporte() +
    guides(fill = guide_legend(reverse = TRUE))
}

#' Guarda una figura del reporte como JPG en `Reporte/Figuras_reporte/ICR/` y la devuelve
#'
#' Se usa dentro de los bloques del reporte: la figura se dibuja en el
#' documento y además queda guardada como archivo.
guardar_figura <- function(p, nombre, ancho = 8, alto = 4.6, dir = "Reporte/Figuras_reporte/ICR") {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  ggsave(file.path(dir, nombre), p, width = ancho, height = alto, dpi = 200, device = ragg::agg_jpeg, quality = 95)
  p
}
