# Extract data and create dataset for speech (and eye gaze?) data of HRI experiment

entireScriptStartTime <- Sys.time()

library(rPraat)
library(tidyverse)
library(lme4)
library(parameters) # for PCA
`%!in%` <- Negate(`%in%`)

folder <- "C:/Users/offredet/Documents/1HU/ExperimentEyes/Data/All-NotFiltered/"
folder2 <- "C:/Users/offredet/Documents/1HU/ExperimentEyes/Data/"

# (referring to a previously written regression with turnDuration ~ Condition:)
# doing this regression with only the humans' data shows no significant difference in turn durations (t = -1.627)
# but if we include robots too, then t = -2.706. If the robots didn't contribute to the effect,
# I'd expect them to vary in duration randomly, and either maintain the same |t| value or reduce it.
# But if it increased, it could be that the experimenter was behaving differently in each condition.
# voices: one was slower and one faster. we tried to counterbalance them across orders but it wasn't a perfect balance (something like 20-30)
# this could be the source of the effect

## (part of this code was adapted from AudioData.R)

filesTG <- list.files(folder, "\\.TextGrid")
filesTG <- filesTG[!grepl("VUV", filesTG)]
filesTXT <- list.files(folder, "txt")
filesTXT <- filesTXT[!grepl("Register", filesTXT)]

files <- data.frame(cbind(filesTG, filesTXT))
files <- files %>%
  mutate(worked = ifelse(substr(files$filesTG, 1, 9) == substr(files$filesTXT, 1, 9), "worked!", "NO!!!!"))

f0 <- data.frame("file"=NA, "speaker"=NA, "turn"=NA, "turnOnset"=NA, "turnOffset"=NA, "turnDur"=NA, "IPU"=NA, "IPUOnset"=NA, "IPUOffset"=NA, "IPUDur"=NA, "f0mean"=NA, "f0sd"=NA, "intensMean"=NA, "intensMax"=NA) %>% 
  filter(!is.na(file))

# do something like the following, but also calculating f0 mean per IPU (not entire turn)
startTime <- Sys.time()
for(i in 1:nrow(files)){
  tg <- tg.read(paste0(folder, files$filesTG[[i]]), encoding=detectEncoding(paste0(folder, files$filesTG[[i]])))
  txt <- read.table(paste0(folder, files$filesTXT[[i]]), header=TRUE, na.strings = "--undefined--") %>% 
    mutate(f0meanz = (f0mean - mean(f0mean, na.rm=TRUE))/sd(f0mean, na.rm=TRUE),
           f0sdz = (f0sd - mean(f0sd, na.rm=TRUE))/sd(f0sd, na.rm=TRUE),
           f0mean = ifelse(abs(f0meanz) > 2.5, NA, f0mean),
           f0sd = ifelse(abs(f0sdz) > 2.5, NA, f0sd))
  
  if(substr(files$filesTG[i], 4, 5) == "BL"){ # baseline speech recordings
    ipuCount <- 0
    startBL <- as.numeric(tg.getIntervalStartTime(tg, "speech", as.numeric(tg.findLabels(tg, "speech", "baseline"))))
    endBL <- as.numeric(tg.getIntervalEndTime(tg, "speech", as.numeric(tg.findLabels(tg, "speech", "baseline"))))
    for(p in 1:tg.getNumberOfIntervals(tg, "IPU")){
      if(tg.getLabel(tg, "IPU", p)!="pause"){
        startIPU <- as.numeric(tg.getIntervalStartTime(tg, "IPU", p))
        endIPU <- as.numeric(tg.getIntervalEndTime(tg, "IPU", p))
        if(startBL <= startIPU){ # doing two separate if() statements because there's some confusion with the use of &(&)
          if(endBL >= endIPU){
            ipuCount <- ipuCount + 1
            f <- data.frame(matrix(nrow=0, ncol=4))
            names(f) <- c("f0mean", "f0sd", "intMean", "intMax")
            for(l in 1:nrow(txt)){
              if(txt$onset[l] >= startIPU){
                if(txt$offset[l] <= endIPU){
                  f[nrow(f)+1,] <- c(as.numeric(txt$f0mean[l]),
                                     as.numeric(txt$f0sd[l]),
                                     as.numeric(txt$intensityMean[l]),
                                     as.numeric(txt$intensityMax[l]))
                }
              }
            }
            if(any(!is.na(f))){
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
                                     mean(f$f0mean, na.rm=TRUE),
                                     mean(f$f0sd, na.rm=TRUE),
                                     mean(f$intMean, na.rm=TRUE),
                                     max(f$intMax, na.rm=TRUE))
            }
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
          if(tg.getLabel(tg, "IPU", p)!="pause"){
            startIPU <- as.numeric(tg.getIntervalStartTime(tg, "IPU", p))
            endIPU <- as.numeric(tg.getIntervalEndTime(tg, "IPU", p))
            if(turnOnset <= startIPU){ # doing two separate if() statements because there's some confusion with the use of &(&)
              if(turnOffset >= endIPU){
                ipuCount <- ipuCount + 1
                f <- data.frame(matrix(nrow=0, ncol=4))
                names(f) <- c("f0mean", "f0sd", "intMean", "intMax")
                for(l in 1:nrow(txt)){
                  if(txt$onset[l] >= startIPU){
                    if(txt$offset[l] <= endIPU){
                      if(!is.na(txt$f0mean[l])){
                        f[nrow(f)+1,] <- c(as.numeric(txt$f0mean[l]),
                                           as.numeric(txt$f0sd[l]),
                                           as.numeric(txt$intensityMean[l]),
                                           as.numeric(txt$intensityMax[l]))
                      }
                    }
                  }
                }
                if(any(!is.na(f))){
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
                                       mean(f$f0mean, na.rm=TRUE),
                                       mean(f$f0sd, na.rm=TRUE),
                                       mean(f$intMean, na.rm=TRUE),
                                       max(f$intMax, na.rm=TRUE))
                }
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
          if(tg.getLabel(tg, "IPU", p)!="pause"){
            startIPU <- as.numeric(tg.getIntervalStartTime(tg, "IPU", p))
            endIPU <- as.numeric(tg.getIntervalEndTime(tg, "IPU", p))
            if(turnOnset <= startIPU){
              if(turnOffset >= endIPU){
                ipuCount <- ipuCount + 1
                f <- data.frame(matrix(nrow=0, ncol=4))
                names(f) <- c("f0mean", "f0sd", "intMean", "intMax")
                for(l in 1:nrow(txt)){
                  if(txt$onset[l] >= startIPU){ # doing two separate if() statements because there's some confusion with the use of &(&)
                    if(txt$offset[l] <= endIPU){
                      if(!is.na(txt$f0mean[l])){
                        f[nrow(f)+1,] <- c(as.numeric(txt$f0mean[l]),
                                           as.numeric(txt$f0sd[l]),
                                           as.numeric(txt$intensityMean[l]),
                                           as.numeric(txt$intensityMax[l]))
                      }
                    }
                  }
                }
                if(any(!is.na(f))){
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
                                       mean(f$f0mean, na.rm=TRUE),
                                       mean(f$f0sd, na.rm=TRUE),
                                       mean(f$intMean, na.rm=TRUE),
                                       max(f$intMax, na.rm=TRUE))
                }
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

load(paste0(folder, "f0.RData"))

dat <- f0 %>% 
  mutate(participant = substr(file, 7, 9), # variable with name of speaker including for the robot subsets
         groupings = paste(speaker, condition, task, sep = "."),
         tgroup = paste(groupings, turn, sep=".")) %>% 
  mutate_at(c("IPUOnset", "IPUOffset", "IPUDur", "turnOnset", "turnOffset", "turnDur"), as.numeric)

# for each turn of the human, save `robPrevf0`, i.e. the f0 of the robot's previous turn
# for the first IPU of the human, robPrevf0 is the f0 of the robot's first IPU in the previous turn. human's second IPU is robot's second IPU u.s.w

# datsave <- dat
# dat <- datsave

# the following calculates the average f0 of each turn to then use it to calculate the difference between robot's and human's subsequent turns

dt <- data.frame(matrix(nrow=0, ncol=2))
names(dt) <- c("tgroup", "f0turn")

for(t in unique(dat$tgroup)){
  d <- dat %>%
    filter(tgroup == t)
  dt[nrow(dt)+1,] <- c(t, mean(d$f0mean, na.rm=TRUE))
}

dat <- merge(dat, dt, by="tgroup")

# to get the `robPrevf0` for the baseline, turn all the previous robot's IPUs into one long list of IPUs (not divided by turn),
# then do invIPU like for the conversation

dat <- dat %>%
  mutate(interlocutor = ifelse(nchar(speaker)==3, paste0(speaker, "-Robot"), substr(speaker, 1, 3)),
         robPrevf0mean = NA,
         robPrevf0meanMock = NA,
         gapDur = NA,
         f0Diff = NA)

# add metadata

folder2 <- "C:/Users/offredet/Documents/1HU/ExperimentEyes/Data/"
file <- list.files(folder2, "\\.csv")
file <- file[grepl("metadata", file)]

m <- read.csv(paste0(folder2, file), sep=";") %>% 
  rename(participant = Participant)

dam <- merge(dat, m, by="participant")
  

# calculate robPrevf0, i.e. the average f0 of the interlocutor's previous turn
# for the baseline, robPrevf0 is the average of all the first robot's turns (robPrevf0 in `baseline` only exists for the second baseline)
# also calculate gapDur, i.e. the gap (in seconds) between the interlocutor's previous turn and speaker's current turn

# for baseline, do a different dataset and create column `overallIPU`, which are all the IPUs of the robot without dividing them by turn. this will serve for the `robPrevf0` of the baseline
# also, in baseline, save the person's 

b <- data.frame(matrix(nrow=0, ncol=6))
names(b) <- c("speaker", "IPU", "f0mean", "f0sd", "condition", "overallIPU")

for(s in unique(dat$speaker[grepl("Robot", dat$speaker)])){
  for(c in unique(dat$condition)){
    b0 <- dat %>% 
      filter(speaker == s, 
             condition == c) %>%
      dplyr::select(speaker, IPU, f0mean, f0sd, condition) %>% 
      mutate_at("IPU", as.numeric) %>%
      mutate(overallIPU = 1:n())
    b <- rbind(b, b0)
  }
}

dam <- dam %>% 
  mutate(prevCond = substr(Order, 1, 2)) %>% 
  mutate(prevCond = ifelse(condition == prevCond, NA, prevCond))

dab <- dam %>% filter(task == "Baseline") %>% # Baseline dataset
  dplyr::select(-c("turn", "gapDur", "f0Diff")) %>% 
  mutate_at(c("f0mean", "f0sd", "intensMean", "intensMax"), as.numeric) %>% 
  mutate("robPrevf0mean"=NA, "robPrevf0meanMock"=NA, "robPrevf0sd"=NA, "robPrevf0sdMock"=NA, "humanPrevf0mean"=NA, "humanPrevf0sd"=NA)
dac <- dam %>% filter(task == "Conversation") %>%  # Conversation dataset
  mutate_at(c("turn", "f0mean", "f0sd", "intensMean", "intensMax"), as.numeric) %>% 
  mutate("robPrevf0mean"=NA, "robPrevf0meanMock"=NA, "robPrevf0sd"=NA, "robPrevf0sdMock"=NA, "robPrevIntMean"=NA, "robPrevIntMeanMock"=NA, "robPrevIntMax"=NA, "robPrevIntMaxMock"=NA) %>% 
  group_by(speaker) %>% 
  mutate(turnNormal = (turn - min(turn)) / (max(turn) - min(turn))) %>% 
  ungroup()

# dac <- dac[!grepl("TMF|BFI|GA|NG|Impairment|Dyslexia|Gender|Education|L1|Age", names(dac))]
# dab <- dab[!grepl("TMF|BFI|GA|NG|Impairment|Dyslexia|Gender|Education|L1|Age", names(dab))]

for(i in 1:nrow(dac)){ # getting `robPrevf0` for the Conversation dataset
  if(nchar(dac$speaker[i]) == 3){ # if the speaker is human
    if(dac$task[i] == "Conversation"){ # if it's during the conversation (vs baseline)
      previousf0mean <- dac$f0mean[dac$speaker == dac$interlocutor[i] &
                                     dac$turn == dac$turn[i] &
                                     dac$condition == dac$condition[i] &
                                     dac$IPU == dac$IPU[i]]
      previousf0meanMock <- dac$f0mean[dac$speaker == dac$interlocutor[i] &
                                         dac$turn == dac$turn[i] &
                                         dac$condition != dac$condition[i] & # here use the other condition
                                         dac$IPU == dac$IPU[i]]
      previousf0sd <- dac$f0sd[dac$speaker == dac$interlocutor[i] &
                                 dac$turn == dac$turn[i] &
                                 dac$condition == dac$condition[i] &
                                 dac$IPU == dac$IPU[i]]
      previousf0sdMock <- dac$f0sd[dac$speaker == dac$interlocutor[i] &
                                     dac$turn == dac$turn[i] &
                                     dac$condition != dac$condition[i] & # here use the other condition
                                     dac$IPU == dac$IPU[i]]
      previousIntMean <- dac$intensMean[dac$speaker == dac$interlocutor[i] &
                                          dac$turn == dac$turn[i] &
                                          dac$condition == dac$condition[i] &
                                          dac$IPU == dac$IPU[i]]
      previousIntMeanMock <- dac$intensMean[dac$speaker == dac$interlocutor[i] &
                                              dac$turn == dac$turn[i] &
                                              dac$condition != dac$condition[i] & # here use the other condition
                                              dac$IPU == dac$IPU[i]]
      previousIntMax <- dac$intensMax[dac$speaker == dac$interlocutor[i] &
                                        dac$turn == dac$turn[i] &
                                        dac$condition == dac$condition[i] &
                                        dac$IPU == dac$IPU[i]]
      previousIntMaxMock <- dac$intensMax[dac$speaker == dac$interlocutor[i] &
                                            dac$turn == dac$turn[i] &
                                            dac$condition != dac$condition[i] & # here use the other condition
                                            dac$IPU == dac$IPU[i]]
      if(!purrr::is_empty(previousf0mean)){
        dac$robPrevf0mean[i] <- previousf0mean
      }
      if(!purrr::is_empty(previousf0meanMock)){
        dac$robPrevf0meanMock[i] <- previousf0meanMock
      }
      if(!purrr::is_empty(previousf0sd)){
        dac$robPrevf0sd[i] <- previousf0sd
      }
      if(!purrr::is_empty(previousf0sdMock)){
        dac$robPrevf0sdMock[i] <- previousf0sdMock
      }
      if(!purrr::is_empty(previousIntMean)){
        dac$robPrevIntMean[i] <- previousIntMean
      }
      if(!purrr::is_empty(previousIntMeanMock)){
        dac$robPrevIntMeanMock[i] <- previousIntMeanMock
      }
      if(!purrr::is_empty(previousIntMax)){
        dac$robPrevIntMax[i] <- previousIntMax
      }
      if(!purrr::is_empty(previousIntMax)){
        dac$robPrevIntMaxMock[i] <- previousIntMax
      }
      previousf0meanTurn <- unique(as.numeric(dac$f0turn[dac$speaker == dac$interlocutor[i] &
                                     dac$turn == dac$turn[i] &
                                     dac$condition == dac$condition[i]]))
      dac$f0Diff[i] <- abs(as.numeric(dac$f0turn[i]) - previousf0meanTurn)
      prevEnd <- as.numeric(unique(dac$turnOffset[dac$speaker == dac$interlocutor[i] &
                                             dac$turn == dac$turn[i] &
                                             dac$condition == dac$condition[i]]))
      dac$gapDur[i] <- as.numeric(dac$turnOnset[i]) - prevEnd
    }
  }
}

for(i in 1:nrow(dab)){
  previousf0mean <- as.numeric(b$f0mean[b$speaker == dab$interlocutor[i] &
                                          b$condition == dab$prevCond[i] &
                                          b$overallIPU == dab$IPU[i]])
  previousf0sd <- as.numeric(b$f0sd[b$speaker == dab$interlocutor[i] &
                                      b$condition == dab$prevCond[i] &
                                      b$overallIPU == dab$IPU[i]])
  if(!purrr::is_empty(previousf0mean)){
    if(!any(is.na(previousf0mean))){
      dab$robPrevf0mean[i] <- previousf0mean
    }
  }
  if(!purrr::is_empty(previousf0sd)){
    if(!any(is.na(previousf0sd))){
      dab$robPrevf0sd[i] <- previousf0sd
    }
  }
  if(dab$condition[i] == substr(dab$Order[i], 1, 2)){
    previousf0meanMock <- as.numeric(b$f0mean[b$speaker == dab$interlocutor[i] &
                                              b$condition == dab$condition[i] &
                                              b$overallIPU == dab$IPU[i]])
    previousf0sdMock <- as.numeric(b$f0sd[b$speaker == dab$interlocutor[i] &
                                            b$condition == dab$condition[i] &
                                            b$overallIPU == dab$IPU[i]])
    Hpreviousf0mean <- as.numeric(dab$f0mean[dab$speaker == dab$speaker[i] &
                                               dab$condition != dab$condition[i] &
                                               dab$IPU == dab$IPU[i]])
    Hpreviousf0sd <- as.numeric(dab$f0sd[dab$speaker == dab$speaker[i] &
                                           dab$condition != dab$condition[i] &
                                           dab$IPU == dab$IPU[i]])
    if(!purrr::is_empty(previousf0meanMock)){
      if(!any(is.na(previousf0meanMock))){
        dab$robPrevf0meanMock[i] <- previousf0meanMock
      }
    }
    if(!purrr::is_empty(previousf0sdMock)){
      if(!any(is.na(previousf0sdMock))){
        dab$robPrevf0sdMock[i] <- previousf0sdMock
      }
    }
    if(!purrr::is_empty(Hpreviousf0mean)){
      if(!any(is.na(Hpreviousf0mean))){
        dab$humanPrevf0mean[i] <- Hpreviousf0mean
      }
    }
    if(!purrr::is_empty(Hpreviousf0sd)){
      if(!any(is.na(Hpreviousf0sd))){
        dab$humanPrevf0sd[i] <- Hpreviousf0sd
      }
    }
  }
}

dL <- list(dab, dac)
for(d in 1:length(dL)){
  dL[[d]] <- dL[[d]] %>% 
    dplyr::select(-"groupings")
}
dab <- dL[[1]]
dac <- dL[[2]]

# add intimacy rating for each question

# first we have to figure out which participant saw which question in each condition

# dac <- dac[!grepl("TMF|BFI|GA|NG|Impairment|Dyslexia|Gender|Education|L1|Age", names(dac))]

q <- read.csv(paste0(folder2, "questions.csv"), header=TRUE, dec=".", sep=";") %>% 
  dplyr::select(-Q) %>% 
  group_by(scenario) %>% 
  mutate(turn = 1:n()) %>% 
  ungroup()
    
# match participant to question text + rating based on `q`

dac <- dac %>% 
  mutate(scenario = ifelse(condition == substr(Order, 1, 2), 1, 2))

dac <- full_join(dac, q, by=c("scenario", "turn")) %>% 
  rename("intimMean"="mean", "intimMed"="median", "intimSD"="SD")

##############
## PCA

dpG <- dac %>% 
  filter(!grepl("Robot", speaker)) %>%
  dplyr::select(speaker, GA.1:GA.12) %>% 
  distinct()
names(dpG) <- c("speaker", paste0("I", 1:12))
dpN <- dac %>% 
  filter(!grepl("Robot", speaker)) %>%
  dplyr::select(speaker, NG.1:NG.12) %>% 
  distinct()
names(dpN) <- c("speaker", paste0("I", 1:12))
dp <- rbind(dpN, dpG)

print(pc <- principal_components(dp[,2:13], n = 3, threshold = 0.4, standardize = TRUE, rotation="varimax"))
summary(pc)
# write.csv(pc, paste0(folder, "loadings2.csv"))

# rename questionnaire columns so they make sense
# also get rid of all the TMF columns: transform them into on TMF.F and one TMF.M

# the following questionnaire datasets are organized around the principal components identified in the PCA above

questionnaire1 <- data.frame(questionNumber = c(1, 2, 3, 9, 10, 11, 12),
                            question = c("conversationFlow",
                                         "floorYield",
                                         "floorHold",
                                         "enjoyTalkingToRob",
                                         "positiveAboutRob",
                                         "positiveAboutConv",
                                         "comfortTalkingToRob"),
                            dimension = c(rep("PCConvQuality", 7)))

questionnaire2 <- data.frame(questionNumber = c(4, 5, 6, 7, 12),
                            question = c("robTiming",
                                         "faceHumanlike",
                                         "voiceHumanlike",
                                         "behaviorHumanlike",
                                         "comfortTalkingToRob"),
                            dimension = c(rep("PCEvalRobot", 5)))


dacsave <- dac
dac <- dacsave

daq <- dac %>%
  filter(!grepl("Robot", speaker)) %>% 
  dplyr::select(speaker, GA.1:NG.12) %>% 
  distinct() %>%
  gather(key=condition, value=rating, -speaker) %>% 
  mutate(questionNumber = substr(condition, 4, nchar(condition)),
         condition = substr(condition, 1, 2))

daqsave <- daq
# daq <- daqsave

daq1 <- merge(daq, questionnaire1, by="questionNumber") %>%
  group_by(speaker, condition) %>% 
  mutate(dimensionRating = mean(rating, na.rm=TRUE)) %>%
  ungroup() %>% 
  dplyr::select(-questionNumber) %>% 
  spread(key=dimension, value=dimensionRating) %>% 
  spread(key=question, value = rating) %>% 
  dplyr::select(1:3)

daq2 <- merge(daq, questionnaire2, by="questionNumber") %>%
  group_by(speaker, condition) %>% 
  mutate(dimensionRating = mean(rating, na.rm=TRUE)) %>%
  ungroup() %>% 
  dplyr::select(-questionNumber) %>% 
  spread(key=dimension, value=dimensionRating) %>% 
  spread(key=question, value = rating) %>% 
  dplyr::select(1:3)

daq <- full_join(daq1, daq2, by=c("speaker", "condition"))

dac <- dac %>%
  dplyr::select(-c(GA.1:NG.12))

# join dac with the questionnaire data
# and then also reduce the dataset by calculating the TMF scores

dac <- full_join(dac, daq, by=c("speaker", "condition")) %>% 
  group_by(speaker) %>% 
  mutate(TMF.F = (TMF.F1+TMF.F2+TMF.F3+TMF.F4+TMF.F5+TMF.F6)/6,
         TMF.M = (TMF.M1+TMF.M2+TMF.M3+TMF.M4+TMF.M5+TMF.M6)/6) %>% 
  ungroup() %>% 
  dplyr::select(-c(TMF.M1:TMF.F6, scenario))

# calculate the BFI scores (average of each dimension)

bfi <- read.csv(paste0(folder2, "adjectivesBFI.csv"), sep = ";")

db <- dac %>% 
  filter(!grepl("Robot", speaker)) %>% 
  dplyr::select(speaker, BFI1:BFI40) %>% 
  distinct() %>%
  gather(key=adjNumber, value=rating, -speaker) %>% 
  mutate(adjNumber = substr(adjNumber, 4, nchar(adjNumber)))

db <- merge(db, bfi, by="adjNumber")

for(r in 1:nrow(db)){
  if(db$inverted[r] == "True"){
    db$rating[r] <- 6 - db$rating[r] # 6 because the highest possible rating was 5
  }
}

db <- db %>% 
  group_by(speaker, dimensionBFI) %>% 
  mutate(dimensionBFIRating = mean(rating, na.rm=TRUE)) %>% 
  ungroup() %>% 
  spread(key=dimensionBFI, value=dimensionBFIRating) %>% 
  dplyr::select(-c("adjNumber", "rating", "adjectiveBFI", "inverted"))

# the following is to collapse all the rows that should be together (but are right now containing one (correct) value and many NAs)

coalesce_by_column <- function(db) {
  return(dplyr::coalesce(!!! as.list(db)))
}

db <- db %>%
  group_by(speaker) %>%
  summarise_all(coalesce_by_column)

dac <- merge(dac, db, by="speaker", all=TRUE) %>% 
  dplyr::select(-c(BFI1:BFI40, X))

# also transform the questionnaire data in the dab dataset into something meaningful like you did for dac

info <- dac %>% 
  dplyr::select(speaker, condition, PCConvQuality, PCEvalRobot, TMF.F, TMF.M, Agreeableness, Conscientiousness, EmotionalStability, Extraversion, IntellectOpenness) %>% 
  distinct() %>% 
  filter(!grepl("Robot", speaker))

dab <- dab %>% 
  dplyr::select(-c("X", "GA.1":"NG.12", "TMF.M1":"BFI40"))

dab <- full_join(dab, info, by=c("speaker", "condition"))

# create dataset that can compare the baseline and the conversation data more directly

dac <- dac %>% 
  group_by(speaker, condition) %>% 
  mutate(overallIPU = 1:n()) %>% 
  ungroup() %>% 
  mutate("baselinef0mean"=NA, "baselinef0sd"=NA)

for(i in 1:nrow(dac)){
  bf0mean <- as.numeric(dab$f0mean[dab$speaker == dac$speaker[i] &
                                     dab$condition == dac$condition[i] &
                                     dab$IPU  == dac$overallIPU[i]])
  bf0sd <- as.numeric(dab$f0sd[dab$speaker == dac$speaker[i] &
                                 dab$condition == dac$condition[i] &
                                 dab$IPU  == dac$overallIPU[i]])
  if(!purrr::is_empty(bf0mean)){
    if(!any(is.na(bf0mean))){
      dac$baselinef0mean[i] <- bf0mean
    }
  }
  if(!purrr::is_empty(bf0sd)){
    if(!any(is.na(bf0sd))){
      dac$baselinef0sd[i] <- bf0sd
    }
  }
}

# save files

save(dac, file = paste0(folder, "dataConversation.RData"))
save(dab, file = paste0(folder, "dataBaseline.RData"))

entireScriptEndTime <- Sys.time()
entireScriptEndTime-entireScriptStartTime
