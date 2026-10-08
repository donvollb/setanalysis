-- Entfernt die Hülle, die Quarto um jede Chunk-Ausgabe legt (in Typst ein
-- #block[...]), wie der Filter chunk-ausgabe.lua in set-template. Erst ohne
-- diese Hülle lassen sich die Tabellen zentrieren.

function Div(el)
  if el.classes:includes("cell") then
    return el.content
  end
end
