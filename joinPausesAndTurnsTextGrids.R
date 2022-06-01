# join TextGrid with pause annotations (created through a Praat script) and one with turn annotations (created manually)

library(rPraat)
library(tidyverse)
`%!in%` <- Negate(`%in%`)

folder <- "C:/Users/tomof/Documents/1HU/ExperimentEyes/Data/"

f <- list.files(folder, "TextGrid")
fP <- f[grepl("pauses", f)] # pause annotations
fT <- f[!grepl("pauses", f)] # turn annotations

files <- data.frame(cbind(fT, fP)) %>%
  mutate(worked = ifelse(substr(fT, 1, 9)==substr(fP, 1, 9), "worked!", "NO!!!!"))
table(files$worked)

for(i in 1:nrow(files)){
  p <- tg.read(paste0(folder, files$fP[i]), encoding=detectEncoding(paste0(folder, files$fP[i])))
  t <- tg.read(paste0(folder, files$fT[i]), encoding=detectEncoding(paste0(folder, files$fT[i])))
  
  tg.getTotalDuration(p)
  tg.getTotalDuration(t)
  
  t <- tg.insertNewIntervalTier(t, newInd=Inf, "IPU", tMin=0, tMax=tg.getEndTime(t))
  for(n in 1:(tg.getNumberOfIntervals(p, "sentence")-1)){ # there was a weird issue with the last interval,
    #but since the last interval is usually gonna be outside of the last turn anyway (even for baseline),
    #it's probably ok not to use the last annotated "IPU"
    t <- tg.insertInterval(t,
                           "IPU",
                           tStart=tg.getIntervalStartTime(p, "sentence", n),
                           tEnd=tg.getIntervalEndTime(p, "sentence", n),
                           label=tg.getLabel(p, "sentence", n))
  }
  name <- gsub(".TextGrid", "_withPauses.TextGrid", files$fT[i])
  tg.write(t, paste0(folder, name))
}

# new <- tg.read(paste0(folder, "GA-CO-DMT_withPauses.TextGrid"))
# tg.plot(new)
