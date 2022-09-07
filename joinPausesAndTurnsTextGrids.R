# join TextGrid with pause annotations (created through a Praat script) and one with turn annotations (created manually)

library(rPraat)
library(tidyverse)
`%!in%` <- Negate(`%in%`)

folder <- "C:/Users/offredet/Documents/1HU/ExperimentEyes/Data/All/"
folderPauses <- "C:/Users/offredet/Documents/1HU/ExperimentEyes/Data/TextGridsPauses/"
folderTurns <- "C:/Users/offredet/Documents/1HU/ExperimentEyes/Data/TextGridsTurns/"

f <- list.files(folder, "TextGrid")
# fP <- f[grepl("pauses", f)] # pause annotations
# fT <- f[!grepl("pauses", f)] # turn annotations
fT0 <- f # turn annotations also containing old pause measurements
fP <- list.files(folderPauses, "TextGrid")

# since the files in fT0 contain a tier with old pause measurements, let's first remove this tier
# and then we'll save these textgrids as the "turns" textgrids in a different folder

# for(i in fT0){
#   tg <- tg.read(paste0(folder, i), encoding = detectEncoding(paste0(folder, i)))
#   tg <- tg.removeTier(tg, "IPU")
#   newName <- gsub("\\.TextGrid", "_turns.TextGrid", i)
#   tg.write(tg, paste0(folderTurns, newName), format = "text")
# }
# done

fT <- list.files(folderTurns, "TextGrid")

files <- data.frame(cbind(fT, fP)) %>%
  mutate(worked = ifelse(substr(fT, 1, 9)==substr(fP, 1, 9), "worked!", "NO!!!!"))
table(files$worked)

for(i in 1:nrow(files)){
  p <- tg.read(paste0(folderPauses, files$fP[i]), encoding=detectEncoding(paste0(folderPauses, files$fP[i])))
  t <- tg.read(paste0(folderTurns, files$fT[i]), encoding=detectEncoding(paste0(folderTurns, files$fT[i])))
  
  tg.getTotalDuration(p)
  tg.getTotalDuration(t)
  
  t <- tg.insertNewIntervalTier(t, newInd=Inf, "IPU", tMin=0, tMax=tg.getEndTime(t))
  for(n in 1:(tg.getNumberOfIntervals(p, "silences")-1)){ # there was a weird issue with the last interval,
    # but since the last interval is usually gonna be outside of the last turn anyway (even for baseline),
    # it's probably ok not to use the last annotated "IPU"
    t <- tg.insertInterval(t,
                           "IPU",
                           tStart=tg.getIntervalStartTime(p, "silences", n),
                           tEnd=tg.getIntervalEndTime(p, "silences", n),
                           label=tg.getLabel(p, "silences", n))
  }
  name <- gsub("_turns", "", files$fT[i])
  tg.write(t, paste0(folder, name))
}

# new <- tg.read(paste0(folder, "GA-CO-DMT_withPauses.TextGrid"))
# tg.plot(new)
