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

## (adapted from AudioData.R)

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
startTime <- Sys.time()
for(i in 1:nrow(files)){
  tg <- tg.read(paste0(folder, files$filesTG[[i]]), encoding=detectEncoding(paste0(folder, files$filesTG[[i]])))
  txt <- read.table(paste0(folder, files$filesTXT[[i]]), header=TRUE, na.strings = "--undefined--")
  
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
                             "baseline", # "turn"
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
    turnCount <- 0
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
            if(txt$onset >= startIPU & txt$offset <= endIPU){
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
            if(txt$onset >= startIPU & txt$offset <= endIPU){
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
}

f0 <- f0 %>% 
  mutate(condition = substr(file, 1, 2),
         task = ifelse(substr(file, 4, 5) == "BL", "Baseline", "Conversation"),
         f0mean = ifelse(f0mean == "NaN", NA, f0mean)) %>% 
  mutate_at("f0mean", as.numeric)

endTime <- Sys.time()
endTime-startTime

# save this because this for loop takes literal hours!
save(f0, file=paste0(folder, "f0.RData"))

###############
###############
###############

# check if f0 was extracted correctly
# also show in the plot if there are NAs

load(paste0(folder, "f0.RData"))

####check

ch <- f0 %>% filter(speaker=="AMO", condition=="NG", task=="Conversation")
a <- ch %>% filter(turn==1)
b <- ch %>% filter(turn==2)
c <- ch %>% filter(turn==3)
d <- ch %>% filter(turn==4)

#### end of check

f0 <- f0 %>% 
  mutate(participant = substr(file, 7, 9), # variable with name of speaker including for the robot subsets
         groupings = paste(speaker, condition, task, sep = "."))

# for(i in unique(f0$participant)){
#   dat0 <- f0 %>% filter(participant == i)
#   par(mfrow=c(3, 2))
#   for(g in unique(dat0$groupings)){
#     dat <- dat0 %>% filter(groupings == g)
#     plot(dat$IPU, dat$f0mean, type="l", main=g)
#   }
#   readline("Enter to continue")
# }

# lots of weird data!

# calculate proportion of NA

f0 <- f0 %>%
  # group_by(speaker, condition, task) %>%
  group_by(groupings) %>%
  mutate(NAprop = sum(is.na(f0mean)) / n()) %>%
  ungroup()

# length(unique(f0$NAprop))
# length(unique(f0$groupings))

# in the following i think the idea was to get the amount of NA also grouping per turn of each speaker
# but i don't see the point in that anymore, so forget it.
# 
# c <- data.frame(matrix(nrow=0, ncol=4))
# names(c) <- c("turn", "f0mean", "groupings", "NAprop")
# 
# for(g in unique(f0$groupings)){
#   c0 <- f0 %>% 
#     filter(groupings == g) %>% 
#     select(turn, f0mean, groupings) %>%
#     mutate(NAprop = sum(is.na(f0mean)) / n())
#   c <- rbind(c, c0)
# }
# 
# f0a <- full_join(f0, c, by=c("groupings", "turn", "f0mean"), all=TRUE)
# f0a <- f0a %>% distinct()
# # na <- f0a[!duplicated(f0a$groupings)]
# na <- f0a %>% distinct(groupings, .keep_all=TRUE)
# hist(na$NAprop)

# end of unnecessarily complicated code

# hist(f0$NAprop)

# I'm not sure how to deal with it. why is there so much NA?
# should we exclude files with too many NAs? what would be the cutoff point?

# exclude f0 higher than 2.5 SD

n <- data.frame(matrix(nrow=0, ncol=4))
names(n) <- c("turn", "f0mean", "groupings", "f0z")

for(g in unique(f0$groupings)){
  n0 <- f0 %>% 
    filter(groupings == g) %>% 
    select(turn, f0mean, groupings) %>%
    mutate(f0z = (f0mean - mean(f0mean, na.rm=TRUE))/sd(f0mean, na.rm=TRUE))
  n <- rbind(n, n0)
}

f0b <- full_join(f0, n, by=c("groupings", "turn", "f0mean"), all=TRUE)
f0b <- f0b %>% distinct()
f0b <- f0b %>% filter(abs(f0z) < 2.5)

# from here: create new variable with the f0 of partner's previous turn (f0 mean I guess)

dat <- f0b

# calculate mean f0 per turn
# (mean f0 of entire baseline for BL files)
# save that as the f0 of the interlocutor's following turn

dat$tgroup <- paste(dat$groupings, dat$turn, sep=".")

# dt <- dat %>% # trying to get the mean per turn (per speaker per task per condition), but this code keeps grouping the dataset by file, not by `tgroup`
#   group_by(tgroup) %>% 
#   summarize(f0turn = mean(f0mean, na.rm=TRUE)) %>% # i've also tried with mutate()
#   ungroup() #%>% 
  # select(-c("f0mean", "f0z", "IPU", "IPUOnset", "IPUOffset", "IPUDur")) %>% 
  # distinct()

dt <- data.frame(matrix(nrow=0, ncol=2))
names(dt) <- c("tgroup", "f0turn")

t=unique(dat$tgroup[!grepl("baseline", dat$tgroup)])[[5]]

for(t in unique(dat$tgroup)){
  d <- dat %>% 
    filter(tgroup == t)
  dt[nrow(dt)+1,] <- c(t, mean(d$f0mean, na.rm=TRUE))
}

dat <- dat %>% 
  select(-c("f0mean", "f0z", "IPU", "IPUOnset", "IPUOffset", "IPUDur")) %>% 
  distinct()
 
dat <- merge(dat, dt, by="tgroup")

# why does even this group it by file and not tgroup??????

  
  
  
  
  
  
  
  
  







ch <- dat[,c(1:3, 18,19)]

dat$prevf0 <- NA

for(i in 2:nrow(dat)){
  dat$prevf0[i] <- NA
}

