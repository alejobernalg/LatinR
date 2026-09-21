

#let article(
  title: none,
  subtitle: none,
  authors: none,
  date: none,
  abstract: none,
  abstract-title: none,
  cols: 1,
  margin: (x: 1.25in, y: 1.25in),
  paper: "us-letter",
  lang: "en",
  region: "US",
  font: "libertinus serif",
  fontsize: 11pt,
  title-size: 1.5em,
  subtitle-size: 1.25em,
  heading-family: "libertinus serif",
  heading-weight: "bold",
  heading-style: "normal",
  heading-color: black,
  heading-line-height: 0.65em,
  sectionnumbering: none,
  pagenumbering: "1",
  toc: false,
  toc_title: none,
  toc_depth: none,
  toc_indent: 1.5em,
  doc,
) = {
  set page(
    paper: paper,
    margin: margin,
    numbering: pagenumbering,
  )
  set par(justify: true)
  set text(lang: lang,
           region: region,
           font: font,
           size: fontsize)
  set heading(numbering: sectionnumbering)
  // Portada: página aparte, sin cabecera, pie ni fecha. El texto empieza en la página 1.
  if title != none {
    page(header: none, footer: none, margin: (x: 3cm, y: 3cm))[
      #set text(font: font)
      #set par(justify: false)
      #align(center)[
        #v(1.2cm)
        #if authors != none {
          let afs = authors.fold((), (acc, a) => if acc.contains(a.affiliation) { acc } else { acc + (a.affiliation,) })
          text(size: 15pt, weight: "bold", tracking: 0.04em)[#smallcaps[#afs.join([ \ ])]]
        }
        #v(0.5cm)
        #line(length: 30%, stroke: 0.6pt)
        #v(1fr)
        #block(width: 100%)[
          #set par(leading: 0.55em)
          #text(size: 24pt, weight: "bold")[#title]
          #if subtitle != none {
            v(0.9em)
            text(size: 14pt, style: "italic")[#subtitle]
          }
        ]
        #v(1fr)
        #if authors != none {
          line(length: 30%, stroke: 0.6pt)
          v(0.6cm)
          for a in authors {
            text(size: 14pt)[#a.name]
            linebreak()
          }
        }
        #v(1.5cm)
      ]
      #counter(page).update(0)
    ]
    align(center)[#block(above: 0.4em, below: 0.6em)[
      #set par(leading: 0.55em)
      #text(weight: "bold", size: 1.45em)[#title]
    ]]
  }

  if abstract != none {
    block(inset: 2em)[
    #text(weight: "semibold")[#abstract-title] #h(1em) #abstract
    ]
  }

  if toc {
    let title = if toc_title == none {
      auto
    } else {
      toc_title
    }
    block(above: 0em, below: 2em)[
    #outline(
      title: toc_title,
      depth: toc_depth,
      indent: toc_indent
    );
    ]
  }

  if cols == 1 {
    doc
  } else {
    columns(cols, doc)
  }
}

#set table(
  inset: 6pt,
  stroke: none
)
