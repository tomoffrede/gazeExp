# join TextGrid with pause annotations (created through a Praat script) and one with turn annotations (created manually)

library(rPraat)
library(tidyverse)
`%!in%` <- Negate(`%in%`)

folder <- "C:/Users/tomof/Documents/1HU/ExperimentEyes/Data/test2/"

f <- list.files(folder, "TextGrid")
fP <- folder[grepl("pauses", folder)] # pause annotations
fT <- folder[!grepl("pause", folder)] # turn annotations

files <- data.frame(cbind(fT, fP))

for(i in 1:nrow(files)){
  p <- tg.read(paste0(folder, files$fP[i]), encoding=detectEncoding(paste0(folder, files$fP[i])))
  t <- tg.read(paste0(folder, files$fT[i]), encoding=detectEncoding(paste0(folder, files$fT[i])))
  
  
}
