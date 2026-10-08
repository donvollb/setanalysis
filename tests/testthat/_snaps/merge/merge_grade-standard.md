
```{=typst}
#show figure: set block(breakable: false)
#figure( // start preamble figure
  
  kind: table, // end preamble figure

block[ // start block

  #let style-dict = (
    // tinytable style-dict after
    "1_1": 0, "1_2": 0, "1_3": 0, "1_4": 0, "1_5": 0, "1_6": 0, "1_0": 1, "0_0": 2, "0_1": 3, "0_2": 3, "0_3": 3, "0_4": 3, "0_5": 3, "0_6": 3
  )

  #let style-array = ( 
    // tinytable cell style after
    (align: center,),
    (align: left,),
    (background: rgb("#6DACDC1A"), align: left,),
    (bold: true, background: rgb("#6DACDC1A"), align: center,),
  )

  // Helper function to get cell style
  #let get-style(x, y) = {
    let key = str(y) + "_" + str(x)
    if key in style-dict { style-array.at(style-dict.at(key)) } else { none }
  }

  #show table.cell: it => {
    if style-array.len() == 0 { return it }
    
    let style = get-style(it.x, it.y)
    if style == none { return it }
    
    let tmp = it
    if ("fontsize" in style) { tmp = text(size: style.fontsize, tmp) }
    if ("color" in style) { tmp = text(fill: style.color, tmp) }
    if ("indent" in style) { tmp = pad(left: style.indent, tmp) }
    if ("underline" in style) { tmp = underline(tmp) }
    if ("italic" in style) { tmp = emph(tmp) }
    if ("bold" in style) { tmp = strong(tmp) }
    if ("mono" in style) { tmp = math.mono(tmp) }
    if ("strikeout" in style) { tmp = strike(tmp) }
    if ("smallcaps" in style) { tmp = smallcaps(tmp) }
    tmp
  }

  // tinytable align-figure before

  #table( // tinytable table start
    columns: (53.00%, 9.00%, 8.00%, 8.00%, 8.00%, 7.00%, 7.00%),
    stroke: none,
    rows: auto,
    align: (x, y) => {
      let style = get-style(x, y)
      if style != none and "align" in style { style.align } else { left }
    },
    fill: (x, y) => {
      let style = get-style(x, y)
      if style != none and "background" in style { style.background }
    },
 table.hline(y: 1, start: 0, end: 7, stroke: 0.05em),
 table.hline(y: 2, start: 0, end: 7, stroke: 0.08em),
 table.hline(y: 0, start: 0, end: 7, stroke: 0.08em),
    // tinytable lines before

    // tinytable header start
    table.header(
      repeat: true,
[#text(weight: "bold")[Item] _[Skala: Schulnoten]_], [n], [M], [SD], [MD], [Min], [Max],
    ),
    // tinytable header end

    // tinytable cell content after
[Welche Gesamtnote (Schulnote) geben Sie der Veranstaltung insgesamt?], [449], [2.09], [0.50], [2], [1], [3.78],

    // tinytable footer after

  ) // end table

  // tinytable align-figure after

] // end block
) // end figure
```<img src="figure/sub_chunk_2-1.svg" alt="" width="100%" style="display: block; margin: auto;" />
