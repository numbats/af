// Monash University Presentation Theme for Quarto / Typst
// Uses Touying 0.6.3 presentation framework

#import "@preview/touying:0.6.3": *

// Colour palette
#let monash-blue   = rgb("#1969AA")
#let monash-orange = rgb("#C14b14")
#let dark-grey     = rgb("#646464")
#let light-grey    = rgb("#e7e7e7")

// Slide geometry (16:9)
#let slide-width   = 25.4cm
#let slide-height  = 14.29cm
#let margin-x      = 1.0cm
#let margin-top    = 1.0cm
#let margin-bottom = 0.1cm
#let title-bar-h   = 1.7cm

#let _parse-color(c) = {
  if c == "white" { white } else if c == "black" { black } else { rgb(c) }
}

// Draw the full-width blue title bar and push content below it.
#let _title-bar(label) = {
  place(top + left, dx: -margin-x, dy: -margin-top,
    rect(width: slide-width, height: title-bar-h, fill: monash-blue,
         inset: (x: margin-x - 0.2cm, y: 0.45cm))[
      #set text(fill: white, size: 1.3em, weight: "bold")
      #label
    ]
  )
  v(title-bar-h - margin-top + 0.75cm)
}

// Render TOC entries.  cur = none → all highlighted; cur = i → section i+1 highlighted.
#let _toc-entries(sections, fontsize, cur) = {
  for (i, sec) in sections.enumerate() {
    let n   = i + 1
    let col = if cur == none or n == cur + 1 { monash-orange } else { monash-orange.lighten(60%) }
    block(above: fontsize, below: fontsize, inset: (left: 1cm),
      link(sec.location(),
        text(size: fontsize * 1.1)[
          #box(fill: col, inset: (x: 8pt, y: 5pt), baseline: 20%,
            text(fill: white, weight: "bold")[#n])
          #h(0.4em)
          #text(fill: col, weight: "regular")[#sec.body]
        ]
      )
    )
  }
}

// Content slide: injects the blue title bar via the setting wrapper.
#let slide(
  config: (:), repeat: auto, setting: body => body, composer: auto, ..bodies,
) = touying-slide-wrapper(self => {
  let inner-setting(body) = {
    _title-bar(utils.display-current-heading(depth: self.slide-level))
    setting(body)
  }
  touying-slide(self: self, config: config, repeat: repeat,
                setting: inner-setting, composer: composer, ..bodies)
})

// Title slide with full-page background image.
#let title-slide(config: (:), ..args) = touying-slide-wrapper(self => {
  let info          = self.info + args.named()
  let title-color   = _parse-color(self.store.titlecolor)
  let titlefontsize = self.store.titlefontsize
  let fontsize      = self.store.fontsize

  let self = utils.merge-dicts(self,
    config-common(freeze-slide-counter: true),
    config-page(margin: 0em, header: none, footer: none),
    config,
  )

  touying-slide(self: self, {
    place(top + left,
      image(self.store.titlegraphic, width: slide-width, height: slide-height, fit: "cover"))
    place(top + left, dx: 1.0cm, dy: 4.8cm,
      block(width: slide-width * 0.65)[
        #set text(fill: title-color, size: titlefontsize, weight: "bold")
        #set par(leading: 0.8em)
        #info.title
        #if info.subtitle != none {
          linebreak()
          text(size: titlefontsize * 0.65, weight: "regular")[#info.subtitle]
        }
      ]
    )
    place(top + left, dx: 1.0cm, dy: slide-height - margin-top - margin-bottom - 2.2cm,
      block(width: slide-width * 0.6)[
        #set text(fill: title-color, size: fontsize * 1.1)
        #if info.author != none { info.author }
        #if info.date   != none { v(0em); utils.display-info-date(self) }
      ]
    )
  })
})

// Section / TOC slide (shown for each H1 when toc: true).
#let new-section-slide(config: (:), level: 1, numbered: true, body) = touying-slide-wrapper(self => {
  let self = utils.merge-dicts(self,
    config-common(freeze-slide-counter: true),
    config-page(footer: none),
    config,
  )
  let fontsize = self.store.fontsize
  touying-slide(self: self, context {
    _title-bar(self.store.toc-title)
    v(0.5cm)
    let sections = query(heading.where(level: 1))
    let loc = here()
    // Current section = last level-1 heading whose page <= this slide's page
    let cur = {
      let idx = none
      for (i, s) in sections.enumerate() {
        if s.location().page() <= loc.page() { idx = i }
      }
      idx
    }
    _toc-entries(sections, fontsize, cur)
  })
})

// Entry point called from typst-show.typ.
#let presentation(
  title:         none,
  subtitle:      none,
  authors:       none,
  date:          none,
  titlegraphic:  none,
  titlecolor:    "black",
  titlefontsize: 36pt,
  toc:           false,
  toc-title:     "Outline",
  fontsize:      22pt,
  bg-path:       "_extensions/quarto-monash/presentation/_images/background/",
  doc,
) = {
  let bg-path = bg-path.replace("\\_", "_")
  let img-path = if titlegraphic == none { bg-path + "bg-02.png" }
                 else if titlegraphic.contains("/") { titlegraphic }
                 else { bg-path + titlegraphic }

  let footer-fn(self) = {
    place(bottom + left, rect(width: 100%, height: 2pt, fill: light-grey))
    place(bottom + left, components.progress-bar(height: 2pt, monash-orange, light-grey))
    place(bottom + right,
      move(dx: -1.5mm, dy: -2.0mm,
        context text(fill: rgb("#c7c7c7"), size: 0.5em)[#utils.slide-counter.display()]
      )
    )
  }

  show: touying-slides.with(
    config-page(
      width: slide-width, height: slide-height,
      margin: (x: margin-x, top: margin-top, bottom: margin-bottom),
      footer: footer-fn, footer-descent: 0%,
    ),
    config-common(
      slide-fn: slide,
      new-section-slide-fn: if toc { new-section-slide } else { none },
      slide-level: 2,
      show-strong-with-alert: false,
      zero-margin-footer: true,
    ),
    config-info(title: title, subtitle: subtitle, author: authors, date: date),
    config-store(
      titlegraphic: img-path, titlecolor: titlecolor,
      titlefontsize: titlefontsize, toc-title: toc-title, fontsize: fontsize,
    ),
    config-methods(
      init: (self: none, body) => {
        set text(font: ("Fira Sans", "Liberation Sans"), size: fontsize)
        set par(justify: false, leading: 0.65em)
        set figure(placement: none)
        show raw: it => { set text(font: ("DejaVu Sans Mono", "Noto Sans Mono", "Liberation Mono")); it }
        show block.where(fill: rgb("#f1f3f5")): it => { set text(size: fontsize * 0.85); it }
        set list(
          marker: (
            box(width: 0.55em, height: 0.55em, baseline: -0.05em, fill: monash-orange),
            text(fill: monash-orange.lighten(20%), size: 0.75em)[#sym.triangle.filled.r],
            text(fill: monash-orange.lighten(20%), size: 0.75em)[#sym.star.filled],
          ),
          indent: 1.2em, body-indent: 0.6em,
        )
        set enum(
          full: true,
          numbering: (..nums) => box(fill: monash-orange, inset: (x: 4pt, y: 2pt),
            text(fill: white, size: 0.72em, weight: "bold")[#nums.pos().last()]),
          indent: 1.2em, body-indent: 0.8em,
        )
        show strong: it => text(fill: monash-orange, weight: "bold", it.body)
        set table(inset: (x: 6pt, y: 8pt), stroke: none)
        set table.hline(stroke: 1pt + dark-grey)
        show table: it => context {
          let w = measure(it).width
          set align(center)
          line(length: w, stroke: 2pt + dark-grey); v(-1em); it; v(-1em)
          line(length: w, stroke: 2pt + dark-grey)
        }
        show heading.where(level: 3): h => {
          v(1em, weak: true)
          text(weight: "bold", fill: monash-blue)[#h.body]
          v(0.7em, weak: true)
        }
        body
      },
    ),
  )

  title-slide()

  // Initial TOC slide (all sections highlighted equally)
  if toc {
    touying-slide-wrapper(self => {
      let self2 = utils.merge-dicts(self,
        config-common(freeze-slide-counter: true),
        config-page(footer: none),
      )
      touying-slide(self: self2, context {
        _title-bar(toc-title)
        v(0.5cm)
        _toc-entries(query(heading.where(level: 1)), fontsize, none)
      })
    })
  }

  doc
}
