# Code, der beim Laden des Pakets ausgeführt wird -----------------------

## Bei Start des Pakets Schriftart laden ----------------------------------

.onAttach <- function(libname, pkgname) {
  
 try(silent = TRUE, { # Fehlermeldungen ignorieren (ist für Installation nötig)
  
  if (!"Red Hat Text" %in% sysfonts::font_families()) {
   
  sysfonts::font_add("Red Hat Text", 
    regular = system.file("fonts/RHMixed-Regular.ttf", package = "setanalysis"),
       bold = system.file("fonts/RHMixed-Bold.ttf",    package = "setanalysis"),
     italic = system.file("fonts/RHMixed-Light.ttf",   package = "setanalysis"))
  
  showtext::showtext_auto()
  }}
)
}
