# Extract data and create dataset for speech (and eye gaze?) data of HRI experiment

library(rPraat)
library(tidyverse)
library(lme4)
`%!in%` <- Negate(`%in%`)

folder <- "C:/Users/offredet/Documents/1HU/ExperimentEyes/Data/All/"

# Objects so far in the code below containing data (if you want to combine them, you'll have to change some things in them so they look the same):
# tD, tG, f0

# TURNS

# Get duration of each turn

fTG <- list.files(folder, "\\.TextGrid")
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

summary(lmer(turnDur ~ condition + (1 | speaker), tD %>% filter(substr(speaker, 1, 2) != "R-")))
# doing this regression with only the humans' data shows no significant difference in turn durations (t = -1.627)
# but if we include robots too, then t = -2.706. If the robots didn't contribute to the effect,
# I'd expect them to vary in duration randomly, and either maintain the same |t| value or reduce it.
# But if it increased, it could be that the experimenter was behaving differently in each condition.

# Get duration of gap between robot's and human's turns

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

tG$condition <- substr(tG$file, 1, 2)

summary(lmer(gap ~ condition + (1 | file), tG))
# gap duration didn't vary per condition


# ACOUSTICS

## (taken from AudioData.R)

filesTG <- list.files(folder, "\\.TextGrid")
filesTG <- filesTG[!grepl("VUV", filesTG)]
filesTXT <- list.files(folder, "txt")
filesTXT <- filesTXT[!grepl("Register", filesTXT)]

files <- data.frame(cbind(filesTG, filesTXT))
files <- files %>%
  mutate(worked = ifelse(substr(files$filesTG, 1, 9) == substr(files$filesTXT, 1, 9), "worked!", "NO!!!!"))

f0 <- data.frame(matrix(nrow=0, ncol=11))
names(f0) <- c("file", "speaker", "turn", "turnOnset", "turnOffset", "turnDur", "IPU", "IPUOnset", "IPUOffset", "IPUDur", "f0mean")

# do something like the following, but also calculating f0 mean per IPU (not entire turn)

for(i in 1:nrow(files)){
  tg <- tg.read(paste0(folder, files$filesTG[[i]]), encoding=detectEncoding(paste0(folder, files$filesTG[[i]])))
  txt <- read.table(paste0(folder, files$filesTXT[[i]]), header=TRUE, na.strings = "--undefined--")
  turnCount <- 0
  
  if(substr(files$filesTG[i], 4, 5) == "BL"){ # baseline speech!
    ipuCount <- 0
    startBL <- as.numeric(tg.getIntervalStartTime(tg, "speech", as.numeric(tg.findLabels(tg, "speech", "baseline"))))
    endBL <- as.numeric(tg.getIntervalEndTime(tg, "speech", as.numeric(tg.findLabels(tg, "speech", "baseline"))))
    for(p in 1:tg.getNumberOfIntervals(tg, "IPU")){
      if(tg.getLabel(tg, "IPU", p)==""){ # if the interval is labeled as empty (vs. as "xxx"), it means it's an IPU
        ipuCount <- ipuCount + 1
        startIPU <- as.numeric(tg.getIntervalStartTime(tg, "IPU", p))
        endIPU <- as.numeric(tg.getIntervalEndTime(tg, "IPU", p))
        f <- (txt %>%
                filter(onset >= startIPU & offset <= endIPU) %>%
                summarize(f = mean(f0mean, na.rm=TRUE)))[1,1] # "[1,1]" because `f` is a 1x1 matrix data frame
        f0[nrow(f0)+1,] <- c(substr(files$filesTG[i], 1, 9),
                             substr(files$filesTG[i], 7, 9),
                             "baseline",
                             startBL,
                             endBL,
                             as.numeric(endBL - startBL), # duration of entire baseline
                             ipuCount,
                             startIPU,
                             endIPU,
                             as.numeric(endIPU - startIPU), # duration of IPU
                             as.numeric(f))
      }
    }
  } else if(substr(files$filesTG[i], 4, 5) == "CO"){ # conversation
    for(n in 1:tg.getNumberOfIntervals(tg, "participant")){
      if(tg.getLabel(tg, "participant", n) == "s"){
        ipuCount <- 0
        turnCount <- turnCount + 1
        turnOnset <- as.numeric(tg.getIntervalStartTime(tg, "participant", n))
        turnOffset <- as.numeric(tg.getIntervalEndTime(tg, "participant", n))
        for(p in 1:tg.getNumberOfIntervals(tg, "IPU")){
          if(tg.getLabel(tg, "IPU", p)==""){ # if the interval is labeled as empty (vs. as "xxx"), it means it's an IPU
            ipuCount <- ipuCount + 1
            startIPU <- as.numeric(tg.getIntervalStartTime(tg, "IPU", p))
            endIPU <- as.numeric(tg.getIntervalEndTime(tg, "IPU", p))
            f <- (txt %>%
                    filter(onset >= startIPU & offset <= endIPU) %>%
                    summarize(f = mean(f0mean, na.rm=TRUE)))[1,1] # "[1,1]" because `f` is a 1x1 matrix data frame
            f0[nrow(f0)+1,] <- c(substr(files$filesTG[i], 1, 9),
                                 substr(files$filesTG[i], 7, 9),
                                 turnCount,
                                 turnOnset,
                                 turnOffset,
                                 as.numeric(turnOffset - turnOnset), # duration of turn
                                 ipuCount,
                                 startIPU,
                                 endIPU,
                                 as.numeric(endIPU - startIPU), # duration of IPU
                                 as.numeric(f))
          }
        }
      }
    }
    for(n in 1:tg.getNumberOfIntervals(tg, "robot")){
      if(tg.getLabel(tg, "robot", n) == "s"){
        ipuCount <- 0
        turnCount <- turnCount + 1
        turnOnset <- as.numeric(tg.getIntervalStartTime(tg, "robot", n))
        turnOffset <- as.numeric(tg.getIntervalEndTime(tg, "robot", n))
        for(p in 1:tg.getNumberOfIntervals(tg, "IPU")){
          if(tg.getLabel(tg, "IPU", p)==""){ # if the interval is labeled as empty (vs. as "xxx"), it means it's an IPU
            ipuCount <- ipuCount + 1
            startIPU <- as.numeric(tg.getIntervalStartTime(tg, "IPU", p))
            endIPU <- as.numeric(tg.getIntervalEndTime(tg, "IPU", p))
            f <- (txt %>%
                    filter(onset >= startIPU & offset <= endIPU) %>%
                    summarize(f = mean(f0mean, na.rm=TRUE)))[1,1] # "[1,1]" because `f` is a 1x1 matrix data frame
            f0[nrow(f0)+1,] <- c(substr(files$filesTG[i], 1, 9),
                                 paste0(substr(files$filesTG[i], 7, 9), "-Robot"),
                                 turnCount,
                                 turnOnset,
                                 turnOffset,
                                 as.numeric(turnOffset - turnOnset), # duration of turn
                                 ipuCount,
                                 startIPU,
                                 endIPU,
                                 as.numeric(endIPU - startIPU), # duration of IPU
                                 as.numeric(f))
          }
        }
      }
    }
  }
}

f0 <- f0 %>% 
  mutate(condition = substr(file, 1, 2),
         task = ifelse(substr(file, 4, 5) == "BL", "Baseline", "Conversation"))

###############
###############
###############

# check if f0 was extracted correctly
# also show in the plot if there are NAs

f0 <- fo %>% 
  mutate(participant = substr(file, 7, 9), # variable with name of speaker including for the robot subsets
         groupings = paste0(speaker, condition, task))

f0$participant <- substr(f0$file, 7, 9)
f0$groupings <- paste0(f0$speaker, f0$condition, f0$task)

for(i in unique(f0$participant)){
  dat <- f0 %>% filter(participant == i)
  ggplot(dat, aes(IPU, f0mean))+
    geom_line()+
    facet_wrap(~groupings)
  readline("Enter to continue")
}














