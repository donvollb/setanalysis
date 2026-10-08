# Anhang mit den offenen Antworten ausgeben

Gibt den Anhang „Fragen mit offenem Antwortformat“ aus: alle offenen
Fragen, die zuvor mit `merge_open(..., appendix = TRUE)` aufgerufen
wurden, jeweils mit Link zurück zur Stelle im Bericht.

`appendix_open()` gehört ans Ende jedes Berichts mit Anhang. Danach wird
der Speicher der offenen Antworten
([list_open_answers](https://donvollb.github.io/setanalysis/reference/list_open_answers.md))
geleert, damit ein weiterer Bericht in derselben R-Sitzung neu beginnt.

## Usage

``` r
appendix_open(freq = "auto")
```

## Arguments

- freq:

  Sollen gleiche Antworten zusammengefasst werden? Siehe
  [`merge_open()`](https://donvollb.github.io/setanalysis/reference/merge_open.md).

## Value

Nichts (unsichtbar `NULL`). Der Code für den Bericht wird mit
[`cat()`](https://rdrr.io/r/base/cat.html) ausgegeben. Wurde vorher
keine offene Frage mit Anhang aufgerufen, wird nichts ausgegeben.

## See also

Weitere Auswertungsfunktionen:
[`merge_aggr_sk()`](https://donvollb.github.io/setanalysis/reference/merge_aggr_sk.md),
[`merge_auto()`](https://donvollb.github.io/setanalysis/reference/merge_auto.md),
[`merge_fachsem()`](https://donvollb.github.io/setanalysis/reference/merge_fachsem.md),
[`merge_grade()`](https://donvollb.github.io/setanalysis/reference/merge_grade.md),
[`merge_many()`](https://donvollb.github.io/setanalysis/reference/merge_many.md),
[`merge_mc()`](https://donvollb.github.io/setanalysis/reference/merge_mc.md),
[`merge_num()`](https://donvollb.github.io/setanalysis/reference/merge_num.md),
[`merge_open()`](https://donvollb.github.io/setanalysis/reference/merge_open.md),
[`merge_rueck()`](https://donvollb.github.io/setanalysis/reference/merge_rueck.md),
[`merge_sc()`](https://donvollb.github.io/setanalysis/reference/merge_sc.md),
[`merge_sk()`](https://donvollb.github.io/setanalysis/reference/merge_sk.md),
[`merge_subj()`](https://donvollb.github.io/setanalysis/reference/merge_subj.md),
[`merge_wl()`](https://donvollb.github.io/setanalysis/reference/merge_wl.md)

## Examples

``` r
merge_open(BspDaten$dataSHOWUP$offen, appendix = TRUE)
#> ###  [FILTER_123] Welche weiteren Informationen hätten Sie gerne gehabt, damit Sie sich vor Beginn Ihres Studiums ausreichend über den Studiengang informiert gefühlt hätten? {#sec-1.top} 
#> 
#> *Die offenen Antworten zu dieser Frage finden sich* [im Anhang](#sec-1.bottom).  
#> 
#> \
#> 

# … weitere Fragen des Berichts …

appendix_open()
#> # Anhang: Fragen mit offenem Antwortformat  
#>   
#> ###  [FILTER_123] Welche weiteren Informationen hätten Sie gerne gehabt, damit Sie sich vor Beginn Ihres Studiums ausreichend über den Studiengang informiert gefühlt hätten? {#sec-1.bottom} 
#>  
#> [zurück nach oben](#sec-1.top) 
#> 
#> 
#> +--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+----------------+
#> | **Antwort**                                                                                                                                                                                                                                | **Häufigkeit** |
#> +============================================================================================================================================================================================================================================+================+
#> | Lorem ipsum dolor sit amet.                                                                                                                                                                                                                | 4              |
#> +--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+----------------+
#> | Sed do eiusmod tempor.                                                                                                                                                                                                                     | 2              |
#> +--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+----------------+
#> | Aliqua laborum anim cillum esse non consectetur nostrud. Fugiat nostrud anim laborum excepteur elit laborum.                                                                                                                               | 1              |
#> +--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+----------------+
#> | Cillum elit ullamco reprehenderit aliqua voluptate consequat officia enim eiusmod sed aliquip.                                                                                                                                             | 1              |
#> +--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+----------------+
#> | Ea nulla non esse consequat ullamco elit aute magna.                                                                                                                                                                                       | 1              |
#> +--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+----------------+
#> | Elit consequat aute dolore. Velit reprehenderit enim culpa sint qui ullamco ullamco esse eiusmod ut excepteur et.                                                                                                                          | 1              |
#> +--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+----------------+
#> | Elit lorem labore eiusmod ea nostrud culpa. Non sint occaecat aute sint.                                                                                                                                                                   | 1              |
#> +--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+----------------+
#> | Exercitation id excepteur non.                                                                                                                                                                                                             | 1              |
#> +--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+----------------+
#> | In cupidatat dolore eiusmod nostrud ut elit aliqua.                                                                                                                                                                                        | 1              |
#> +--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+----------------+
#> | Incididunt quis sed sunt in. Quis voluptate officia sed dolore aliquip culpa deserunt. Pariatur occaecat nisi incididunt fugiat cupidatat minim incididunt consectetur excepteur occaecat et ipsum laborum.                                | 1              |
#> +--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+----------------+
#> | Laboris id sunt cillum excepteur.                                                                                                                                                                                                          | 1              |
#> +--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+----------------+
#> | Laborum ipsum adipiscing ea aute ad incididunt cillum enim nostrud aliqua ut velit labore. Nostrud sint officia nostrud id sit irure commodo sint fugiat. Consequat irure elit enim minim veniam eiusmod enim reprehenderit labore cillum. | 1              |
#> +--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+----------------+
#> | Mollit lorem et tempor. Duis nulla reprehenderit sit veniam esse ex. Reprehenderit irure cillum sunt ullamco.                                                                                                                              | 1              |
#> +--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+----------------+
#> | Nisi aliqua ea culpa dolore sint. Laborum nisi non sed officia nisi voluptate duis consequat nostrud. Qui consequat minim lorem dolor.                                                                                                     | 1              |
#> +--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+----------------+
#> | Non adipiscing in dolore ad proident ad est veniam mollit enim quis.                                                                                                                                                                       | 1              |
#> +--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+----------------+
#> | Nostrud ipsum cillum exercitation mollit labore ad aute. Excepteur proident ullamco dolore esse mollit excepteur sunt do. Reprehenderit in reprehenderit sint culpa elit et.                                                               | 1              |
#> +--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+----------------+
#> | Nulla elit culpa anim irure aute culpa do fugiat fugiat laborum excepteur dolore tempor. Nulla lorem commodo pariatur fugiat velit amet velit proident nisi exercitation consequat qui minim.                                              | 1              |
#> +--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+----------------+
#> | Proident in do in. Elit sit minim ut ex enim aute ex ullamco fugiat consectetur quis elit culpa.                                                                                                                                           | 1              |
#> +--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+----------------+
#> | Quis et minim qui commodo lorem sed sint sit exercitation id officia aliqua mollit. Aute mollit esse consectetur do officia aliqua qui in ad minim officia sit.                                                                            | 1              |
#> +--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+----------------+
#> | Sed id consequat velit lorem sit id duis.                                                                                                                                                                                                  | 1              |
#> +--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+----------------+
#> | Sint officia laborum exercitation exercitation in sed est dolor nulla proident culpa.                                                                                                                                                      | 1              |
#> +--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+----------------+
#> | Sit ex in sunt deserunt sunt.                                                                                                                                                                                                              | 1              |
#> +--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+----------------+ 
#> 
```
