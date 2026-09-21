// Estilo Typst del reporte (formato PDF de artículo): fuente tipo LaTeX,
// cabecera con el título, número de página al pie, tablas de tres reglas y
// leyendas con la etiqueta "Figura N." / "Tabla N." en negrita.

#let titulo-corto = "Análisis lingüístico de los errores de ICR: motor OlmOCR"

#set page(
  header: context {
    {
      set text(size: 8.5pt, fill: luma(80))
      smallcaps(titulo-corto)
      h(1fr)
      [Bernal y Donato]
      v(-0.5em)
      line(length: 100%, stroke: 0.4pt + luma(140))
    }
  },
  footer: context align(center, text(size: 9pt, fill: luma(60))[#counter(page).display()]),
)

