# Extract data and create dataset for speech (and eye gaze?) data of HRI experiment

library(rPraat)
library(tidyverse)
`%!in%` <- Negate(`%in%`)

folder <- "C:/Users/tomof/Documents/1HU/ExperimentEyes/Data/"

# TURNS

# Get duration of each turn

fTG <- list.files(folder, "TextGrid")
fTGc <- fTG[substr(fTG, 4, 5) == "CO"] # only conversations! not baseline

tD <- data.frame(matrix(nrow=0, ncol=4))
names(tD) <- c("file", "speaker", "turn", "turnDur")

# i=fTGc[[1]]

for(i in fTGc){
  tg <- tg.read(paste0(folder, i), encoding=detectEncoding(paste0(folder, i)))
  for(n in 1:tg.getNumberOfIntervals(tg, "participant")){
    tD[nrow(tD)+1,] <- c(gsub(".TextGrid", "", i), # file name
                         substr(i, 7, 9), # participant's name
                         n, # number of turn
                         tg.getIntervalDuration(tg, "participant", n)) # duration of turn
  }
  for(n in 1:tg.getNumberOfIntervals(tg, "robot")){
    tD[nrow(tD)+1,] <- c(gsub(".TextGrid", "", i), # file name
                         paste0("R-", substr(i, 7, 9)), # robot's name
                         n, # number of turn
                         tg.getIntervalDuration(tg, "robot", n)) # duration of turn
  }
}

tD <- tD %>%
  mutate_at(c("file", "speaker", "turn"), as.factor) %>%
  mutate_at("turnDur", as.numeric)

tD$condition <- substr(tD$file, 1, 2)
tD$speaker <- substr(tD$file, 7, 9)
summary(lmer(turnDur ~ condition + (1 | speaker), tD))

# Get time between robot's and human's turns

tG <- data.frame(matrix(nrow=0, ncol=4))
names(tG) <- c("file", "turnPairType", "turnPair", "gap")


# only non-overlapping turns

for(i in fTGc){
  tg <- tg.read(paste0(folder, i), encoding=detectEncoding(paste0(folder, i)))
  
  for(p in 1:tg.getNumberOfIntervals(tg, "participant")){ # first, remove all the participant's turns that overlap with the robot's turn
    for(r in 1:tg.getNumberOfIntervals(tg, "robot")){
      if(tg.getLabel(tg, "participant", p) == "s" & tg.getLabel(tg, "robot", r) == "s"){
        if(tg.getIntervalStartTime(tg, "participant", p) > tg.getIntervalStartTime(tg, "robot", r) & tg.getIntervalEndTime(tg, "participant", p) < tg.getIntervalEndTime(tg, "robot", r)){
          tg.setLabel(tg, "participant", p, "")
          tg.removeIntervalBothBoundaries(tg, "participant", p)
        }
      }
    }
  }
  
  gaps1 <- data.frame(matrix(nrow=0, ncol=4))
  names(gaps1) <- c("file", "turnPairType", "turnPair", "gap")
  
  for(p in 1:tg.getNumberOfIntervals(tg, "participant")){
    if(tg.getLabel(tg, "participant", p) == "s"){
      gaps1[nrow(gaps1)+1,] <- c(gsub(".TextGrid", "", i), # file name
                               "Robot-Human", # turn pair type (Robot-Human or Human-Robot)
                               p, # number of RH turn pair
                               as.numeric(tg.getIntervalStartTime(tg, "participant", p)) - as.numeric(tg.getIntervalEndTime(tg, "robot", p))) # start of participant's turn minus end robot's previous turn
    }
  }
  
  gaps1 <- gaps1 %>% mutate(turnPair = 1:nrow(gaps1))

  gaps2 <- data.frame(matrix(nrow=0, ncol=4))
  names(gaps2) <- c("file", "turnPairType", "turnPair", "gap")
  
  for(r in 4:tg.getNumberOfIntervals(tg, "robot")){
    if(tg.getLabel(tg, "robot", r) == "s"){
      gaps2[nrow(gaps2)+1,] <- c(gsub(".TextGrid", "", i), # file name
                               "Human-Robot", # turn pair type (Robot-Human or Human-Robot)
                               p, # number of HR turn pair
                               as.numeric(tg.getIntervalStartTime(tg, "robot", r)) - as.numeric(tg.getIntervalEndTime(tg, "participant", r-2))) # start of participant's turn minus end robot's previous turn
    }
  }
  
  gaps2 <- gaps2 %>% mutate(turnPair = 1:nrow(gaps2))
  gaps <- rbind(gaps1, gaps2)
  
  tG <- rbind(tG, gaps)
}

tG <- tG %>%
  mutate_at(c("file", "turnPairType", "turnPair"), as.factor) %>%
  mutate_at("gap", as.numeric)












