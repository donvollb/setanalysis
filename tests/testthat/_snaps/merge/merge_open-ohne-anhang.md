###  [FILTER_123] Welche weiteren Informationen hätten Sie gerne gehabt, damit Sie sich vor Beginn Ihres Studiums ausreichend über den Studiengang informiert gefühlt hätten? 
 

```{=typst}
#show figure: set block(breakable: false)
#figure( // start preamble figure
  
  kind: table, // end preamble figure

block[ // start block

  #let style-dict = (
    // tinytable style-dict after
    "1_0": 0, "2_0": 0, "3_0": 0, "4_0": 0, "5_0": 0, "6_0": 0, "7_0": 0, "8_0": 0, "9_0": 0, "10_0": 0, "11_0": 0, "12_0": 0, "13_0": 0, "14_0": 0, "15_0": 0, "16_0": 0, "17_0": 0, "18_0": 0, "19_0": 0, "20_0": 0, "21_0": 0, "22_0": 0, "0_1": 1, "0_0": 2
  )

  #let style-array = ( 
    // tinytable cell style after
    (align: left,),
    (bold: true,),
    (bold: true, align: left,),
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
    columns: (88.00%, 12.00%),
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
 table.hline(y: 1, start: 0, end: 2, stroke: 0.05em),
 table.hline(y: 23, start: 0, end: 2, stroke: 0.08em),
 table.hline(y: 0, start: 0, end: 2, stroke: 0.08em),
    // tinytable lines before

    // tinytable header start
    table.header(
      repeat: true,
[Antwort], [Häufigkeit],
    ),
    // tinytable header end

    // tinytable cell content after
[Lorem ipsum dolor sit amet.], [4],
[Sed do eiusmod tempor.], [2],
[Aliqua laborum anim cillum esse non consectetur nostrud. Fugiat nostrud anim laborum excepteur elit laborum.], [1],
[Cillum elit ullamco reprehenderit aliqua voluptate consequat officia enim eiusmod sed aliquip.], [1],
[Ea nulla non esse consequat ullamco elit aute magna.], [1],
[Elit consequat aute dolore. Velit reprehenderit enim culpa sint qui ullamco ullamco esse eiusmod ut excepteur et.], [1],
[Elit lorem labore eiusmod ea nostrud culpa. Non sint occaecat aute sint.], [1],
[Exercitation id excepteur non.], [1],
[In cupidatat dolore eiusmod nostrud ut elit aliqua.], [1],
[Incididunt quis sed sunt in. Quis voluptate officia sed dolore aliquip culpa deserunt. Pariatur occaecat nisi incididunt fugiat cupidatat minim incididunt consectetur excepteur occaecat et ipsum laborum.], [1],
[Laboris id sunt cillum excepteur.], [1],
[Laborum ipsum adipiscing ea aute ad incididunt cillum enim nostrud aliqua ut velit labore. Nostrud sint officia nostrud id sit irure commodo sint fugiat. Consequat irure elit enim minim veniam eiusmod enim reprehenderit labore cillum.], [1],
[Mollit lorem et tempor. Duis nulla reprehenderit sit veniam esse ex. Reprehenderit irure cillum sunt ullamco.], [1],
[Nisi aliqua ea culpa dolore sint. Laborum nisi non sed officia nisi voluptate duis consequat nostrud. Qui consequat minim lorem dolor.], [1],
[Non adipiscing in dolore ad proident ad est veniam mollit enim quis.], [1],
[Nostrud ipsum cillum exercitation mollit labore ad aute. Excepteur proident ullamco dolore esse mollit excepteur sunt do. Reprehenderit in reprehenderit sint culpa elit et.], [1],
[Nulla elit culpa anim irure aute culpa do fugiat fugiat laborum excepteur dolore tempor. Nulla lorem commodo pariatur fugiat velit amet velit proident nisi exercitation consequat qui minim.], [1],
[Proident in do in. Elit sit minim ut ex enim aute ex ullamco fugiat consectetur quis elit culpa.], [1],
[Quis et minim qui commodo lorem sed sint sit exercitation id officia aliqua mollit. Aute mollit esse consectetur do officia aliqua qui in ad minim officia sit.], [1],
[Sed id consequat velit lorem sit id duis.], [1],
[Sint officia laborum exercitation exercitation in sed est dolor nulla proident culpa.], [1],
[Sit ex in sunt deserunt sunt.], [1],

    // tinytable footer after

  ) // end table

  // tinytable align-figure after

] // end block
) // end figure
``` 

