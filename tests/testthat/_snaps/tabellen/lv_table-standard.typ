#show figure: set block(breakable: false)
#figure( // start preamble figure
  
  kind: table, // end preamble figure

block[ // start block

  #let style-dict = (
    // tinytable style-dict after
    "1_0": 0, "3_0": 0, "5_0": 0, "1_1": 1, "3_1": 1, "5_1": 1, "1_2": 1, "3_2": 1, "5_2": 1, "1_3": 1, "3_3": 1, "5_3": 1, "1_4": 1, "3_4": 1, "5_4": 1, "1_5": 1, "3_5": 1, "5_5": 1, "1_6": 1, "3_6": 1, "5_6": 1, "1_7": 1, "3_7": 1, "5_7": 1, "1_8": 1, "3_8": 1, "5_8": 1, "1_9": 1, "3_9": 1, "5_9": 1, "1_10": 1, "3_10": 1, "5_10": 1, "2_0": 2, "4_0": 2, "2_1": 3, "4_1": 3, "2_2": 3, "4_2": 3, "2_3": 3, "4_3": 3, "2_4": 3, "4_4": 3, "2_5": 3, "4_5": 3, "2_6": 3, "4_6": 3, "2_7": 3, "4_7": 3, "2_8": 3, "4_8": 3, "2_9": 3, "4_9": 3, "2_10": 3, "4_10": 3, "0_0": 4, "0_1": 5, "0_2": 5, "0_3": 5, "0_4": 5, "0_5": 5, "0_6": 5, "0_7": 5, "0_8": 5, "0_9": 5, "0_10": 5
  )

  #let style-array = ( 
    // tinytable cell style after
    (align: left,),
    (align: right,),
    (background: rgb("#6DACDC1A"), align: left,),
    (background: rgb("#6DACDC1A"), align: right,),
    (bold: true, background: rgb("#6DACDC1A"), align: left,),
    (bold: true, background: rgb("#6DACDC1A"), align: right,),
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
    columns: (9.09%, 9.09%, 9.09%, 9.09%, 9.09%, 9.09%, 9.09%, 9.09%, 9.09%, 9.09%, 9.09%),
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
 table.hline(y: 1, start: 0, end: 11, stroke: 0.05em),
 table.hline(y: 6, start: 0, end: 11, stroke: 0.08em),
 table.hline(y: 0, start: 0, end: 11, stroke: 0.08em),
    // tinytable lines before

    // tinytable header start
    table.header(
      repeat: true,
[mpg], [cyl], [disp], [hp], [drat], [wt], [qsec], [vs], [am], [gear], [carb],
    ),
    // tinytable header end

    // tinytable cell content after
[21.00], [6], [160], [110], [3.90], [2.62], [16.46], [0], [1], [4], [4],
[21.00], [6], [160], [110], [3.90], [2.88], [17.02], [0], [1], [4], [4],
[22.80], [4], [108], [93], [3.85], [2.32], [18.61], [1], [1], [4], [1],
[21.40], [6], [258], [110], [3.08], [3.21], [19.44], [1], [0], [3], [1],
[18.70], [8], [360], [175], [3.15], [3.44], [17.02], [0], [0], [3], [2],

    // tinytable footer after

  ) // end table

  // tinytable align-figure after

] // end block
) // end figure
