# Tom Offrede
# Rename intervals in textgrids:
# each interval in tier `robot` immediately preceding an interval `sQ` in tier `participant`, rename the interval in `robot` as `sQ`

library(rPraat)
library(tidyverse)
`%!in%` <- Negate(`%in%`)

folder <- "C:/Users/offredet/Documents/1HU/ExperimentEyes/Data/All/"
folderCorr <- "C:/Users/offredet/Documents/1HU/ExperimentEyes/Data/corrected/"

files <- list.files(folder, "TextGrid")
files <- files[substr(files, 4, 5) == "CO"]

# annotate all turns of each one: onset, offset, label, turn number?
# each part's sQ turn: find immediately preceding robot's turn
# change robot's turn into sQ

for(f in files){
  tg <- tg.read(paste0(folder, f), encoding = detectEncoding(paste0(folder, f)))
  
  turnCountHuman <- 0
  turnCountRobot <- 0
  
  robot <- data.frame(matrix(ncol=6, nrow=0))
  names(robot) <- c("speakerR", "turnR", "onsetR", "offsetR", "labelR", "intervalIndexR")
  for(i in 1:tg.getNumberOfIntervals(tg, "robot")){
    if(tg.getLabel(tg, "robot", i) != ""){
      turnCountRobot <- turnCountRobot + 1
      robot[nrow(robot)+1,] <- c(paste0(substr(f, 7, 9), "-Robot"), # speaker name
                                 turnCountRobot, # turn number
                                 as.numeric(tg.getIntervalStartTime(tg, "robot", i)), # onset
                                 as.numeric(tg.getIntervalEndTime(tg, "robot", i)), # offset
                                 tg.getLabel(tg, "robot", i), # label
                                 i) # interval index
    }
  }
  
  part <- data.frame(matrix(ncol=6, nrow=0))
  names(part) <- c("speakerP", "turnP", "onsetP", "offsetP", "labelP", "intervalIndexP")
  for(i in 1:tg.getNumberOfIntervals(tg, "participant")){
    if(tg.getLabel(tg, "participant", i) == "sQ"){
      turnCountHuman <- turnCountHuman + 1
      part[nrow(part)+1,] <- c(substr(f, 7, 9), # speaker name
                               turnCountHuman, # turn number
                               as.numeric(tg.getIntervalStartTime(tg, "participant", i)), # onset
                               as.numeric(tg.getIntervalEndTime(tg, "participant", i)), # offset
                               tg.getLabel(tg, "participant", i), # label
                               i) # interval index
    }
  }
  
  intervalsToChange <- c()
  for(r in 1:nrow(part)){
    p <- part[rep(r, nrow(robot)),]
    p <- cbind(p, robot)
    p <- p %>% 
      mutate_at(c("onsetP", "offsetP", "onsetR", "offsetR"), as.numeric) %>% 
      mutate(gap = onsetP - offsetR) %>% 
      filter(gap >= 0) %>% 
      filter(gap == min(gap))
    intervalsToChange <- c(intervalsToChange, p$intervalIndexR)
  }
  
  intervalsToChange <- as.integer(intervalsToChange)
  for(i in intervalsToChange){
    tg <- tg.setLabel(tg, "robot", i, "sQ")
  }
  
  tg.write(tg, paste0(folderCorr, f))
}