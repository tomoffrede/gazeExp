# Tom Offrede
# in textgrids containing VUV (voiced, unvoiced) sections, create a new tier that joins everything in between U sections of at least x miliseconds
# try different x values: 150, 200, 250, 300

library(tidyverse)
library(rPraat)

folder <- "C:/Users/offredet/Documents/1HU/ExperimentEyes/Data/AllVUV/"
folderIPU <- "C:/Users/offredet/Documents/1HU/ExperimentEyes/Data/AllIPU300/"
files <- list.files(folder, "VUV")

pause <- 0.3

for(f in files){
  t <- tg.read(paste0(folder, f), encoding=detectEncoding(paste0(folder, f)))
  
  t <- tg.insertNewIntervalTier(t, newInd = Inf, "IPU", tMin = tg.getStartTime(t), tMax = tg.getEndTime(t))
  
  for(i in 1:tg.getNumberOfIntervals(t, "vuv")){
    if(tg.getLabel(t, "vuv", i) == "U"){ # if the interval is an unvoiced section
      if(tg.getIntervalDuration(t, "vuv", i) >= pause){ # if this unvoiced interval is larger than x (define above)
        if(i != tg.getNumberOfIntervals(t, "vuv")){
          t <- tg.insertInterval(t, "IPU", tg.getIntervalStartTime(t, "vuv", i), tg.getIntervalEndTime(t, "vuv", i), label = "pause")  
        } else if(i == tg.getNumberOfIntervals(t, "vuv")){
          t <- tg.insertInterval(t, "IPU", tg.getIntervalStartTime(t, "vuv", i), tg.getEndTime(t) - 0.0001, label = "pause")
        }
        
      }
    }
  }
  
  name <- gsub("VUV", "IPU", f)
  tg.write(t, paste0(folderIPU, name))
}