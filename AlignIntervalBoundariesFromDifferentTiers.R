# Tom Offrede
# if the boundaries of an interval is within X distance from a boundary on another tier, change the time so that both intervals match

library(rPraat)
library(tidyverse)

# read TGs
# in each one, read all intervals from "IPU" tier
# if the initial boundary is within 0.005 s before the initial boundary of a turn or the final boundary 0.005 s after the final boundary of a turn,
# change the "IPU" interval#s boundaries to match those of the turn's interval's boundaries

folder <- "C:/Users/offredet/Documents/1HU/ExperimentEyes/Data/All/"
folder2 <- "C:/Users/offredet/Documents/1HU/ExperimentEyes/Data/All/Aligned/"

files <- list.files(folder, "\\.TextGrid")

for(f in files){
  tg <- tg.read(paste0(folder, f), encoding = detectEncoding(paste0(folder, f)))
  tg <- tg.boundaryMagnet(tg, targetTier = "speech", patternTier = "IPU", boundaryTolerance = 1, verbose=FALSE)
  name <- gsub("\\.TextGrid", "_aligned.TextGrid", f)
  tg.write(tg, paste0(folder2, name))
}

# for(f in files){
#   tg <- tg.read(paste0(folder, f), encoding = detectEncoding(paste0(folder, f)))
#   
#   if(substr(f, 4, 5) == "BL"){ # first for BL files
#     for(s in 1:tg.getNumberOfIntervals(tg, "speech")){ # for each interval in the "speech" tier
#       if(tg.getLabel(tg, "speech", "baseline")){ # if the interval is the interval of interest, i.e. labeled as "baseline"
#         for(i in 1:tg.getNumberOfIntervals(tg, "IPU")){ # for each interval in the "IPU" tier
#           diff <- tg.getIntervalStartTime(tg, "speech", s) - tg.getIntervalStartTime(tg, "IPU", i)
#           if(diff <= 0.005 & diff >= 0){ # if the difference between start of "speech" and "IPU" intervals is between 0 and 0.005
#             tg.boundaryMagnet(tg, targetTier = "IPU", patternTier = "speech", boundaryTolerance = 0.005)
#           }
#         }
#       }
#     }
#   } 
# }