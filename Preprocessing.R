# Extract data and create dataset for speech (and eye gaze?) data of HRI experiment

library(rPraat)
library(tidyverse)
library(lme4)
`%!in%` <- Negate(`%in%`)

folder <- "C:/Users/offredet/Documents/1HU/ExperimentEyes/Data/All/"
folder2 <- "C:/Users/offredet/Documents/1HU/ExperimentEyes/Data/"

# (referring to a previously written regression with turnDuration ~ Condition:)
# doing this regression with only the humans' data shows no significant difference in turn durations (t = -1.627)
# but if we include robots too, then t = -2.706. If the robots didn't contribute to the effect,
# I'd expect them to vary in duration randomly, and either maintain the same |t| value or reduce it.
# But if it increased, it could be that the experimenter was behaving differently in each condition.
# voices: one was slower and one faster. we tried to counterbalance them across orders but it wasn't a perfect balance (something like 20-30)
# this could be the source of the effect

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
      if(tg.getLabel(tg, "participant", n) == "sQ"){
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
      if(tg.getLabel(tg, "robot", n) == "sQ"){
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
  mutate_at("f0mean", as.numeric) %>% 
  filter(!is.na(f0mean)) %>% 
  group_by(speaker, condition, task, turn) %>% 
  mutate(IPU = 1:n()) %>% 
  ungroup()

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

{# for(i in unique(f0$participant)){
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
  }

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

dat <- f0b

# for each turn of the human, save `robPrevf0`, i.e. the f0 of the robot's previous turn
# for the first IPU of the human, robPrevf0 is the f0 of the robot's last IPU in the previous turn. human's second IPU is robot's second-to-last IPU u.s.w

# the following loop transforms the robot's IPUs into invIPUs, to match the IPUs of the human later


r <- data.frame(matrix(nrow=0, ncol=5))
names(r) <- c("speaker", "turn", "IPU", "condition", "invIPU")

for(s in unique(dat$speaker[grepl("Robot", dat$speaker)])){
  for(c in unique(dat$condition)){
    for(t in unique(dat$turn[dat$task=="Conversation" & dat$speaker==s & dat$condition==c])){
      r0 <- dat %>% 
        filter(speaker == s, 
               condition == c,
               turn == t) %>%
        select(c(speaker, turn, IPU, condition)) %>% 
        mutate_at("IPU", as.numeric) %>%
        mutate(invIPU = n():1)
      r <- rbind(r, r0)
    }
  }
}


dat <- full_join(dat, r, by=c("speaker", "turn", "IPU", "condition"))

# now do a different dataset and create column `overallIPUs`, which are all the IPUs of the robot without dividing them by turn. this will serve for the `robPrevf0` of the baseline

b <- data.frame(matrix(nrow=0, ncol=5))
names(b) <- c("speaker", "IPU", "f0mean", "condition", "overallIPU")

for(s in unique(dat$speaker[grepl("Robot", dat$speaker)])){
  for(c in unique(dat$condition)){
      b0 <- dat %>% 
        filter(speaker == s, 
               condition == c) %>%
        select(c(speaker, IPU, f0mean, condition)) %>% 
        mutate_at("IPU", as.numeric) %>%
        mutate(overallIPU = n():1,
               continuousIPU = 1:n())
      b <- rbind(b, b0)
  }
}

# datsave <- dat
# dat <- datsave


# the following calculates the average f0 of each turn to then use it to calculate the difference between robot's and human's subsequent turns
 
dat$tgroup <- paste(dat$groupings, dat$turn, sep=".")

dt <- data.frame(matrix(nrow=0, ncol=2))
names(dt) <- c("tgroup", "f0turn")

# t=unique(dat$tgroup[!grepl("baseline", dat$tgroup)])[[5]]

for(t in unique(dat$tgroup)){
  d <- dat %>%
    filter(tgroup == t)
  dt[nrow(dt)+1,] <- c(t, mean(d$f0mean, na.rm=TRUE))
}

dat <- merge(dat, dt, by="tgroup")

# # to get the `robPrevf0` for the baseline, turn all the previous robot's IPUs into one long list of IPUs (not divided by turn),
# # then do invIPU like for the conversation
# 
dat <- dat %>%
  mutate(interlocutor = ifelse(nchar(speaker)==3, paste0(speaker, "-Robot"), substr(speaker, 1, 3)),
         robPrevf0 = NA,
         robPrevf0Mock = NA,
         gapDur = NA,
         f0Diff = NA)

# add metadata

folder2 <- "C:/Users/offredet/Documents/1HU/ExperimentEyes/Data/"
file <- list.files(folder2, "\\.csv")
file <- file[grepl("metadata", file)]

m <- read.csv(paste0(folder2, file)) %>% 
  rename(participant = Participant)

dam <- merge(dat, m, by="participant")
  

# calculate robPrevf0, i.e. the average f0 of the interlocutor's previous turn
# for the baseline, robPrevf0 is the average of all the first robot's turns (robPrevf0 in `baseline` only exists for the second baseline)
# also calculate gapDur, i.e. the gap (in seconds) between the interlocutor's previous turn and speaker's current turn

# t <- dam %>%
#   filter(grepl("Robot", speaker)) %>%
#   group_by(speaker, condition) %>% 
#   summarize(mean = mean(f0mean, na.rm=TRUE)) %>%
#   ungroup()

dam <- dam %>% 
  mutate(prevCond = substr(Order, 1, 2)) %>% 
  mutate(prevCond = ifelse(condition == prevCond, NA, prevCond))

dab <- dam %>% filter(task == "Baseline") %>% # Baseline dataset
  select(-c("turn", "invIPU", "gapDur", "f0Diff"))
dac <- dam %>% filter(task == "Conversation") %>%  # Conversation dataset
  mutate_at("turn", as.numeric) %>% 
  group_by(speaker) %>% 
  mutate(turnNormal = (turn - min(turn)) / (max(turn) - min(turn))) %>% 
  ungroup()

# dac <- dac[!grepl("TMF|BFI|GA|NG|Impairment|Dyslexia|Gender|Education|L1|Age", names(dac))]
# dab <- dab[!grepl("TMF|BFI|GA|NG|Impairment|Dyslexia|Gender|Education|L1|Age", names(dab))]

for(i in 1:nrow(dac)){ # getting `robPrevf0` for the Conversation dataset
  if(nchar(dac$speaker[i]) == 3){ # if the speaker is human
    if(dac$task[i] == "Conversation"){ # if it's during the conversation (vs baseline)
      previousf0 <- dac$f0mean[dac$speaker == dac$interlocutor[i] &
                                 dac$turn == dac$turn[i] &
                                 dac$condition == dac$condition[i] &
                                 dac$IPU == dac$IPU[i]] # tried also with dac$invIPU == dac$IPU[i]
      previousf0Mock <- dac$f0mean[dac$speaker == dac$interlocutor[i] &
                                 dac$turn == dac$turn[i] &
                                 dac$condition != dac$condition[i] & # here use the other condition
                                 dac$IPU == dac$IPU[i]] # tried also with dac$invIPU == dac$IPU[i]
      if(!purrr::is_empty(previousf0)){
        dac$robPrevf0[i] <- previousf0
      }
      if(!purrr::is_empty(previousf0Mock)){
        dac$robPrevf0Mock[i] <- previousf0Mock
      }
      previousf0Turn <- as.numeric(unique(dac$f0turn[dac$speaker == dac$interlocutor[i] &
                                     dac$turn == dac$turn[i] &
                                     dac$condition == dac$condition[i]]))
      dac$f0Diff[i] <- abs(as.numeric(dac$f0turn[i]) - previousf0Turn)
      prevEnd <- as.numeric(unique(dac$turnOffset[dac$speaker == dac$interlocutor[i] &
                                             dac$turn == dac$turn[i] &
                                             dac$condition == dac$condition[i]]))
      dac$gapDur[i] <- as.numeric(dac$turnOnset[i]) - prevEnd
    }
  }
}

for(i in 1:nrow(dab)){
  previousf0 <- b$f0mean[b$speaker == dab$interlocutor[i] &
                           b$condition == dab$prevCond[i] &
                           b$continuousIPU == dab$IPU[i]] # also tried with b$overallIPU == dab$IPU[i]
  if(!purrr::is_empty(previousf0)){
    if(!any(is.na(previousf0))){
      dab$robPrevf0[i] <- previousf0
    }
  }
  if(dab$condition[i] == substr(dab$Order[i], 1, 2)){
    previousf0Mock <- as.numeric(b$f0mean[b$speaker == dab$interlocutor[i] &
                                 b$condition == dab$condition[i] &
                                 b$continuousIPU == dab$IPU[i]]) # tried also with b$overallIPU == dab$IPU[i]
    if(!purrr::is_empty(previousf0Mock)){
      if(!any(is.na(previousf0Mock))){
        dab$robPrevf0Mock[i] <- previousf0Mock
      }
    }
  }
}

# we don't need to calculate `robPrevf0` for the robot, because the robot's speech wasn't influenced by the human anyway

dL <- list(dab, dac)
for(d in 1:length(dL)){
  dL[[d]] <- dL[[d]] %>% 
    select(-"groupings")
}
dab <- dL[[1]]
dac <- dL[[2]]

# add intimacy rating for each question

# first we have to figure out which participant saw which question in each condition

# dac <- dac[!grepl("TMF|BFI|GA|NG|Impairment|Dyslexia|Gender|Education|L1|Age", names(dac))]

q <- read.csv(paste0(folder2, "questions.csv"), header=TRUE, dec=".", sep=";")

d <- data.frame(matrix(nrow=0, ncol=3))
names(d) <- c("tgroup","turnDur", "QTurn")

### here: match participant to question text + rating based on `q`

dac <- dac %>% 
  group_by(tgroup) %>% 
  mutate(QTurn = ifelse(turnDur > 1,)) %>% 
  ungroup()

dac$QIntimacy <- NA


# still have to do something with the questionnaire data

save(dac, file = paste0(folder, "dataConversation.RData"))
save(dab, file = paste0(folder, "dataBaseline.RData"))

