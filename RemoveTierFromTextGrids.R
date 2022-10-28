# Tom Offrede
# Remove one tier from all textgrids in a folder

library(rPraat)
library(tidyverse)
`%!in%` <- Negate(`%in%`)

folder <- "C:/Users/offredet/Documents/1HU/ExperimentEyes/Data/All/"
folderTurns <- "C:/Users/offredet/Documents/1HU/ExperimentEyes/Data/TextGridsTurns/"

files <- list.files(folder, "TextGrid")

for(f in files){
  tg0 <- tg.read(paste0(folder, f), encoding = detectEncoding(paste0(folder, f)))
  tg <- tg.removeTier(tg0, "IPU")
  tg.write(tg, paste0(folderTurns, gsub("\\.TextGrid", "_turns.TextGrid", f)))
}