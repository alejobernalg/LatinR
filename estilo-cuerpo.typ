// Reglas del cuerpo del artículo: se cargan después de la plantilla de Quarto para
// que no las pise.

#set text(font: "New Computer Modern", size: 10.5pt, lang: "es", hyphenate: true)
#set par(justify: true, leading: 0.64em, spacing: 0.9em)
#set enum(indent: 0.6em, body-indent: 0.6em)
#set list(indent: 0.6em, body-indent: 0.6em)

// Encabezados
#set heading(numbering: "1.1")
#show heading: set text(font: "New Computer Modern")
#show heading: set block(sticky: true)
#show heading.where(level: 1): set text(size: 13pt, weight: "bold")
#show heading.where(level: 1): set block(above: 2em, below: 0.9em)
#show heading.where(level: 2): set text(size: 11pt, weight: "bold")
#show heading.where(level: 2): set block(above: 1.6em, below: 0.7em)
#show heading.where(level: 3): set text(size: 10.5pt, weight: "bold", style: "italic")
#show heading.where(level: 3): set block(above: 1.3em, below: 0.5em)

// Código en línea
#show raw: set text(font: "DejaVu Sans Mono", size: 8.6pt)

// Figuras y leyendas
#set figure(gap: 0.9em)
#set figure.caption(separator: [. ])
#show figure.caption: it => align(left, block(width: 100%, inset: (x: 0.5em))[
  #set text(size: 9pt)
  #set par(justify: true)
  #strong[#it.supplement #context it.counter.display(it.numbering)#it.separator]#it.body
])
#show figure: set block(above: 1.6em, below: 1.6em)

// Tablas: tres reglas (arriba, bajo el encabezado y abajo), sin líneas verticales
#set table(
  stroke: (x, y) => (
    top: if y == 0 { 0.9pt } else if y == 1 { 0.5pt } else { 0pt },
  ),
  inset: (x: 7pt, y: 4pt),
)
#show table: set text(size: 8.6pt, hyphenate: false)
#show table: set par(justify: false, leading: 0.5em)
#show table.cell.where(y: 0): set text(weight: "bold")
#show table: it => block(stroke: (bottom: 0.9pt), inset: (bottom: 0pt), it)

// Enlaces de referencia cruzada en tinta oscura (papel)
#show link: set text(fill: rgb("#1f4e79"))
#show ref: set text(fill: rgb("#1f4e79"))

// Resumen enmarcado por filetes
#let resumen(cuerpo) = block(
  width: 100%, inset: (x: 1.2cm, y: 0.8em), above: 1.4em, below: 1.6em,
  stroke: (top: 0.9pt, bottom: 0.9pt),
)[
  #set text(size: 9.5pt)
  #set par(leading: 0.58em)
  #cuerpo
]
