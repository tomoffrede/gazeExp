# Tom Offrede
# Get textgrids I was working with before, remove the "IPU" tier. Then, get the "IPU" tier from new textgrids and put them in the old files, which contain turn annotations

library(rPraat)
library(tidyverse)
`%!in%` <- Negate(`%in%`)

folder <- "C:/Users/offredet/Documents/1HU/ExperimentEyes/Data/temp/"
folderIPUs <- "C:/Users/offredet/Documents/1HU/ExperimentEyes/Data/OneChannelFiltered/IPU/"
folderNew <- "C:/Users/offredet/Documents/1HU/ExperimentEyes/Data/OneChannelFiltered/All/"

f <- list.files(folder, "TextGrid")
fipu <- list.files(folderIPUs, "TextGrid")

files <- data.frame(cbind(f, fipu)) %>%
  mutate(worked = ifelse(substr(f, 1, 9)==substr(fipu, 1, 9), "worked!", "NO!!!!"))
table(files$worked)

for(i in 1:nrow(files)){
  p <- tg.read(paste0(folder, files$f[i]), encoding=detectEncoding(paste0(folder, files$f[i])))
  t <- tg.read(paste0(folderIPUs, files$fipu[i]), encoding=detectEncoding(paste0(folderIPUs, files$fipu[i])))
  
  p <- tg.removeTier(p, "IPU")
  p <- tg.insertNewIntervalTier(p, newInd=Inf, "IPU", tMin=0, tMax=tg.getEndTime(t))
  for(n in 1:(tg.getNumberOfIntervals(t, "IPU"))){
    p <- tg.insertInterval(p,
                           "IPU",
                           tStart=tg.getIntervalStartTime(t, "IPU", n),
                           tEnd=tg.getIntervalEndTime(t, "IPU", n),
                           label=tg.getLabel(t, "IPU", n))
  }
  tg.write(p, paste0(folderNew, files$f[i]))
}
