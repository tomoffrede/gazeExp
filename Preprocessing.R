# Extract data and create dataset for speech (and eye gaze?) data of HRI experiment

library(rPraat)
library(tidyverse)
library(lme4)
`%!in%` <- Negate(`%in%`)

folder <- "C:/Users/offredet/Documents/1HU/ExperimentEyes/Data/All/"

# (referring to a previously written regression with turnDuration ~ Condition:)
# doing this regression with only the humans' data shows no significant difference in turn durations (t = -1.627)
# but if we include robots too, then t = -2.706. If the robots didn't contribute to the effect,
# I'd expect them to vary in duration randomly, and either maintain the same |t| value or reduce it.
# But if it increased, it could be that the experimenter was behaving differently in each condition.
# voices: one was slower and one faster. we tried to counterbalance them across orders but it wasn't a perfect balance (something like 20-30)
# this could be the source for the effect

## (part of it adapted from AudioData.R)

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
        startIPU <- as.numeric(tg.getIntervalStartTime(tg, "IPU", p))
        endIPU <- as.numeric(tg.getIntervalEndTime(tg, "IPU", p))
        if(startBL <= startIPU){ # doing two separate if() statements because there's some confusion with the use of &(&)
          if(endBL >= endIPU){
            ipuCount <- ipuCount + 1
            f <- data.frame(matrix(nrow=0, ncol=1))
            names(f) <- c("f0")
            for(l in 1:nrow(txt)){
              if(txt$onset[l] >= startIPU){
                if(txt$offset[l] <= endIPU){
                  f[nrow(f)+1,] <- as.numeric(txt$f0mean[l])
                }
              }
            }
            f0mean <- mean(f$f0, na.rm=TRUE)  
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
                                 as.numeric(f0mean))
          }
        } 
      }
    }
  } else if(substr(files$filesTG[i], 4, 5) == "CO"){ # conversation
    turnCountHuman <- 0
    turnCountRobot <- 0
    for(n in 1:tg.getNumberOfIntervals(tg, "participant")){
      if(tg.getLabel(tg, "participant", n) == "s"){
        ipuCount <- 0
        turnCountHuman <- turnCountHuman + 1
        turnOnset <- as.numeric(tg.getIntervalStartTime(tg, "participant", n))
        turnOffset <- as.numeric(tg.getIntervalEndTime(tg, "participant", n))
        for(p in 1:tg.getNumberOfIntervals(tg, "IPU")){
          if(tg.getLabel(tg, "IPU", p)==""){ # if the interval is labeled as empty (vs. as "xxx"), it means it's an IPU
            startIPU <- as.numeric(tg.getIntervalStartTime(tg, "IPU", p))
            endIPU <- as.numeric(tg.getIntervalEndTime(tg, "IPU", p))
            if(turnOnset <= startIPU){ # doing two separate if() statements because there's some confusion with the use of &(&)
              if(turnOffset >= endIPU){
                ipuCount <- ipuCount + 1
                f <- data.frame(matrix(nrow=0, ncol=1))
                names(f) <- c("f0")
                for(l in 1:nrow(txt)){
                  if(txt$onset[l] >= startIPU){
                    if(txt$offset[l] <= endIPU){
                      f[nrow(f)+1,] <- as.numeric(txt$f0mean[l])
                    }
                  }
                }
                f0mean <- mean(f$f0, na.rm=TRUE)
                f0[nrow(f0)+1,] <- c(substr(files$filesTG[i], 1, 9),
                                     substr(files$filesTG[i], 7, 9),
                                     turnCountHuman,
                                     turnOnset,
                                     turnOffset,
                                     as.numeric(turnOffset - turnOnset), # duration of turn
                                     ipuCount,
                                     startIPU,
                                     endIPU,
                                     as.numeric(endIPU - startIPU), # duration of IPU
                                     as.numeric(f0mean))
              }
            }
          }
        }
      }
    }
    for(n in 1:tg.getNumberOfIntervals(tg, "robot")){
      if(tg.getLabel(tg, "robot", n) == "s"){
        ipuCount <- 0
        turnCountRobot <- turnCountRobot + 1
        turnOnset <- as.numeric(tg.getIntervalStartTime(tg, "robot", n))
        turnOffset <- as.numeric(tg.getIntervalEndTime(tg, "robot", n))
        for(p in 1:tg.getNumberOfIntervals(tg, "IPU")){
          if(tg.getLabel(tg, "IPU", p)==""){ # if the interval is labeled as empty (vs. as "xxx"), it means it's an IPU
            startIPU <- as.numeric(tg.getIntervalStartTime(tg, "IPU", p))
            endIPU <- as.numeric(tg.getIntervalEndTime(tg, "IPU", p))
            if(turnOnset <= startIPU){
              if(turnOffset >= endIPU){
                ipuCount <- ipuCount + 1
                f <- data.frame(matrix(nrow=0, ncol=1))
                names(f) <- c("f0")
                for(l in 1:nrow(txt)){
                  if(txt$onset[l] >= startIPU){ # doing two separate if() statements because there's some confusion with the use of &(&)
                    if(txt$offset[l] <= endIPU){
                      f[nrow(f)+1,] <- as.numeric(txt$f0mean[l])
                    }
                  }
                }
                f0mean <- mean(f$f0, na.rm=TRUE)
                f0[nrow(f0)+1,] <- c(substr(files$filesTG[i], 1, 9),
                                     paste0(substr(files$filesTG[i], 7, 9), "-Robot"),
                                     turnCountRobot,
                                     turnOnset,
                                     turnOffset,
                                     as.numeric(turnOffset - turnOnset), # duration of turn
                                     ipuCount,
                                     startIPU,
                                     endIPU,
                                     as.numeric(endIPU - startIPU), # duration of IPU
                                     as.numeric(f0mean))
              }
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

save(f0, file=paste0(folder, "f0.RData"))

###############
###############
###############

# check if f0 was extracted correctly
# also show in the plot if there are NAs

load(paste0(folder, "f0.RData"))

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

# f0 <- f0 %>%
#   # group_by(speaker, condition, task) %>%
#   group_by(groupings) %>%
#   mutate(NAprop = sum(is.na(f0mean)) / n()) %>%
#   ungroup()

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

dat <- dat %>% 
  mutate(interlocutor = ifelse(nchar(speaker)==3, paste0(speaker, "-Robot"), substr(speaker, 1, 3)),
         prevf0 = NA,
         gapDur = NA)

d0 <- dat
# dat <- d0

# add metadata

folder2 <- "C:/Users/offredet/Documents/1HU/ExperimentEyes/Data/"
file <- list.files(folder2, "\\.csv")
file <- file[grepl("metadata", file)]

m <- read.csv(paste0(folder2, file)) %>% 
  rename(participant = Participant)

dam <- merge(dat, m, by="participant")

# calculate prevf0, i.e. the average f0 of the interlocutor's previous turn
# for the baseline, prevf0 is the average of all the first robot's turns (prevf0 in `baseline` only exists for the second baseline)
# also calculate gapDur, i.e. the gap (in seconds) between the interlocutor's previous turn and speaker's current turn

t <- dam %>%
  mutate_at("f0turn", as.numeric) %>% 
  filter(grepl("Robot", speaker)) %>% 
  group_by(speaker, condition) %>% 
  summarize(mean = mean(f0turn, na.rm=TRUE)) %>% 
  ungroup()

dam <- dam %>% 
  mutate(prevCond = substr(Order, 1, 2)) %>% 
  mutate(prevCond = ifelse(condition == prevCond, NA, prevCond))

for(i in 1:nrow(dam)){
  if(nchar(dam$speaker[i]) == 3){ # if the speaker is human
    if(dam$task[i] == "Conversation"){ # if it's during the conversation (vs baseline)
      dam$prevf0[i] <- dam$f0turn[dam$speaker == dam$interlocutor[i] &
                                    dam$turn == dam$turn[i] &
                                    dam$condition == dam$condition[i]]
      prevEnd <- as.numeric(dam$turnOffset[dam$speaker == dam$interlocutor[i] &
                                  dam$turn == dam$turn[i] &
                                  dam$condition == dam$condition[i]])
      dam$gapDur[i] <- as.numeric(dam$turnOffset[i]) - prevEnd
    }
    if(dam$task[i] == "Baseline"){
      dam$prevf0[i] <- t$mean[t$speaker == dam$interlocutor[i] &
                                t$condition == dam$prevCond[i]]
        
    }
  }
}
# we don't need to calculate `prevf0` for the robot, because the robot's speech wasn't influeced by the human anyway

dam <- dam %>% 
  select(-c("tgroup", "groupings"))

# still have to do something with the questionnaire data

save(dam, file = paste0(folder, "data.RData"))