# Einstellungen für den gesamten Testlauf ---------------------------------
# (wird nur von testthat ausgeführt, nicht bei devtools::load_all())

# Text in Abbildungen nicht über showtext rendern, damit er in den
# SVG-Snapshots als lesbarer Text erhalten bleibt
showtext::showtext_auto(FALSE)
withr::defer(showtext::showtext_auto(TRUE), teardown_env())

# Sub-Chunks wie im Quarto-Bericht rendern: Tabellen als Typst,
# Abbildungen als SVG (textbasiert und damit vergleichbar)
opts_chunk_alt <- knitr::opts_chunk$get()
opts_knit_alt <- knitr::opts_knit$get()
knitr::opts_chunk$set(dev = "svglite")
knitr::opts_knit$set(rmarkdown.pandoc.to = "typst")

withr::defer(
  {
    knitr::opts_chunk$restore(opts_chunk_alt)
    knitr::opts_knit$restore(opts_knit_alt)
  },
  teardown_env()
)
