# Tom Offrede
# in textgrids containing VUV (voiced, unvoiced) sections, create a new tier that joins everything in between U sections of at least x miliseconds
# try different x values: 150, 200, 250, 300

library(tidyverse)
library(rPraat)

folder <- "C:/Users/offredet/Documents/1HU/ExperimentEyes/Data/AllVUV/"
folderIPU <- "C:/Users/offredet/Documents/1HU/ExperimentEyes/Data/IPU350/"
files <- list.files(folder, "VUV")

pause <- 0.35

for(f in files){
  t <- tg.read(paste0(folder, f), encoding=detectEncoding(paste0(folder, f)))
  
  t <- tg.insertNewIntervalTier(t, newInd = Inf, "IPU0", tMin = tg.getStartTime(t), tMax = tg.getEndTime(t))
  
  for(i in 1:tg.getNumberOfIntervals(t, "vuv")){
    if(tg.getLabel(t, "vuv", i) == "U"){ # if the interval is an unvoiced section
      if(tg.getIntervalDuration(t, "vuv", i) >= pause){ # if this unvoiced interval is larger than x (define above)
        if(i != tg.getNumberOfIntervals(t, "vuv")){
          t <- tg.insertInterval(t, "IPU0", tg.getIntervalStartTime(t, "vuv", i), tg.getIntervalEndTime(t, "vuv", i), label = "pause")
        } else if(i == tg.getNumberOfIntervals(t, "vuv")){
          t <- tg.insertInterval(t, "IPU0", tg.getIntervalStartTime(t, "vuv", i), tg.getEndTime(t) - 0.0001, label = "pause")
        }
      }
    }
  }
  
  t <- tg.duplicateTier(t, "IPU0", newInd = Inf, newTierName = "IPU")
  
  for(i in 2:(tg.getNumberOfIntervals(t, "IPU0")-1)){
    if(tg.getLabel(t, "IPU0", i) == ""){
      if(tg.getIntervalDuration(t, "IPU0", i) < 0.1){
        index <- tg.getIntervalIndexAtTime(t, "IPU", (tg.getIntervalStartTime(t, "IPU0", i) + 0.0000000000001))
        t <- tg.setLabel(t, "IPU", index+1, "") # remove the label from the pause following this short voiced period (because the next line will concatenate the labels of three intervals and we want to avoid something getting labeled "pausepause")
        t <- tg.removeIntervalBothBoundaries(t, "IPU", index)
      }
    }
  }
  
  for(i in 1:tg.getNumberOfIntervals(t, "IPU")){ # rename empty intervals as "IPU"
    if(tg.getLabel(t, "IPU", i) == ""){
      t <- tg.setLabel(t, "IPU", i, "IPU")
    }
  }
  
  t <- tg.removeTier(t, "IPU0")
  
  name <- gsub("VUV", "IPU", f)
  tg.write(t, paste0(folderIPU, name))
}
