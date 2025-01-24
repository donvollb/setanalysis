#' Erzeugt Subchunks für die Berichte
#'
#' @param g Code (kann auch mit Aufzählung ("c(...)") benutzt werden)
#' @param fig_height A number.
#' @param fig_width A number.
#' @param hide description
#' @returns Einen Subchunk
#' @examples
#' add(1, 1)
#' add(10, 1)



# Erzeugen von Sub-Chunks
subchunkify <- function(g, # Code (kann auch mit Aufzählung ("c(...)") benutzt werden)
                        fig_height = 7, # figure height des Sub-Chunks
                        fig_width  = 5, # figure width des Sub-Chunks
                        hide = FALSE) # "hide" für den Sub-Chunk
{
  g_deparsed <- paste0(deparse(
    function() {g}
  ), collapse = '')

  if(hide == FALSE) {head.end <- ", echo=FALSE, results = \"asis\", fig.align = \"center\", out.width = \"100%\"}"}
  else {head.end <- ", echo=FALSE, results = \"hide\", fig.keep = \"all\", fig.align = \"center\", out.width = \"100%\"}" }

  if(!exists("sub.nr")) {assign("sub.nr", 0, envir = globalenv())}
  assign("sub.nr", sub.nr+1, envir = globalenv())

  sub_chunk <- paste0("```{r sub_chunk_", sub.nr, ", fig.height=", fig_height, ", fig.width=", fig_width, head.end,
                      "  \npar(family = \"", font.family, "\")  \n",
                      "  \n",
                      "\n(",
                      "  \n",
                      g_deparsed
                      , ")()",

                      "\n```")

  cat(knitr::knit(text = knitr::knit_expand(text = sub_chunk), quiet = TRUE))
}

# Eigene Funktion zum Runden
true_round <- function(number, digits) {
  posneg <- sign(number)
  number <- abs(number) * 10 ^ digits
  number <- number + 0.5 + sqrt(.Machine$double.eps)
  number <- trunc(number)
  number <- number / 10 ^ digits
  number * posneg
}
