# Erzeugt die Vorschaubilder für README und Vignette -----------------------
#
# Rendert data-raw/readme_vorschau.qmd mit Quarto (Typst) und speichert jede
# Seite als PNG in man/figures/. Die Seiten sind so hoch wie ihr Inhalt.
#
# Ausführen aus dem Paketverzeichnis: source("data-raw/readme_vorschau.R")
# Benötigt Quarto (>= 1.7) und die Schrift „Red Hat Text“.

ordner <- "data-raw"
qmd <- file.path(ordner, "readme_vorschau.qmd")
typ <- file.path(ordner, "readme_vorschau.typ")

system2("quarto", c("render", shQuote(qmd)))

dir.create("man/figures", showWarnings = FALSE, recursive = TRUE)
system2("quarto", c(
  "typst", "compile", shQuote(typ),
  shQuote("man/figures/vorschau-{p}.png"), "--ppi", "150"
))

# Seite 1 auch für die Vignette (Bilder müssen im Ordner vignettes/ liegen,
# damit sie im Paket und auf der pkgdown-Seite gefunden werden)
file.copy("man/figures/vorschau-1.png", "vignettes/vorschau-bericht.png", overwrite = TRUE)

# Zwischenergebnisse entfernen
unlink(c(
  typ, file.path(ordner, "readme_vorschau.pdf"),
  file.path(ordner, "readme_vorschau_files"), file.path(ordner, "figure")
), recursive = TRUE)
