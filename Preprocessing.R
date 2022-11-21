# Extract data and create dataset for speech (and eye gaze?) data of HRI experiment

library(rPraat)
library(tidyverse)
library(lme4)
library(parameters) # for PCA
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

f0 <- data.frame("file"=NA, "speaker"=NA, "turn"=NA, "turnOnset"=NA, "turnOffset"=NA, "turnDur"=NA, "IPU"=NA, "IPUOnset"=NA, "IPUOffset"=NA, "IPUDur"=NA, "timeIndexOverall"=NA, "timeIndexInTurn"=NA, "f0mean"=NA) %>% 
  filter(!is.na(file))

# do something like the following, but also calculating f0 mean per IPU (not entire turn)
startTime <- Sys.time()
for(i in 1:nrow(files)){
  tg <- tg.read(paste0(folder, files$filesTG[[i]]), encoding=detectEncoding(paste0(folder, files$filesTG[[i]])))
  txt <- read.table(paste0(folder, files$filesTXT[[i]]), header=TRUE, na.strings = "--undefined--") %>% 
    mutate(f0z = (f0mean - mean(f0mean, na.rm=TRUE))/sd(f0mean, na.rm=TRUE)) %>% 
    filter(abs(f0z) < 2.5)
  
  if(substr(files$filesTG[i], 4, 5) == "BL"){ # baseline speech recordings
    ipuCount <- 0
    timeIndexOverall <- 0
    startBL <- as.numeric(tg.getIntervalStartTime(tg, "speech", as.numeric(tg.findLabels(tg, "speech", "baseline"))))
    endBL <- as.numeric(tg.getIntervalEndTime(tg, "speech", as.numeric(tg.findLabels(tg, "speech", "baseline"))))
    for(p in 1:tg.getNumberOfIntervals(tg, "IPU")){
      if(tg.getLabel(tg, "IPU", p)==""){ # if the interval is labeled as empty (vs. as "xxx"), it means it's an IPU
        startIPU <- as.numeric(tg.getIntervalStartTime(tg, "IPU", p))
        endIPU <- as.numeric(tg.getIntervalEndTime(tg, "IPU", p))
        if(startBL <= startIPU){ # doing two separate if() statements because there's some confusion with the use of &(&)
          if(endBL >= endIPU){
            ipuCount <- ipuCount + 1
            for(l in 1:nrow(txt)){
              if(txt$onset[l] >= startIPU){
                if(txt$offset[l] <= endIPU){
                  if(!is.na(txt$f0mean[l])){
                    timeIndexOverall <- timeIndexOverall + 1
                    timeIndexInTurn <- timeIndexOverall
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
                                         timeIndexOverall,
                                         timeIndexInTurn,
                                         as.numeric(txt$f0mean[l]))
                  }
                }
              }
            }  
          }
        } 
      }
    }
  } else if(substr(files$filesTG[i], 4, 5) == "CO"){ # conversation
    turnCountHuman <- 0
    turnCountRobot <- 0
    timeIndexOverallHuman <- 0
    timeIndexOverallRobot <- 0
    for(n in 1:tg.getNumberOfIntervals(tg, "participant")){
      if(tg.getLabel(tg, "participant", n) == "sQ"){
        ipuCount <- 0
        turnCountHuman <- turnCountHuman + 1
        timeIndexInTurnHuman <- 0
        turnOnset <- as.numeric(tg.getIntervalStartTime(tg, "participant", n))
        turnOffset <- as.numeric(tg.getIntervalEndTime(tg, "participant", n))
        for(p in 1:tg.getNumberOfIntervals(tg, "IPU")){
          if(tg.getLabel(tg, "IPU", p)==""){ # if the interval is labeled as empty (vs. as "xxx"), it means it's an IPU
            startIPU <- as.numeric(tg.getIntervalStartTime(tg, "IPU", p))
            endIPU <- as.numeric(tg.getIntervalEndTime(tg, "IPU", p))
            if(turnOnset <= startIPU){ # doing two separate if() statements because there's some confusion with the use of &(&)
              if(turnOffset >= endIPU){
                ipuCount <- ipuCount + 1
                for(l in 1:nrow(txt)){
                  if(txt$onset[l] >= startIPU){
                    if(txt$offset[l] <= endIPU){
                      if(!is.na(txt$f0mean[l])){
                        timeIndexOverallHuman <- timeIndexOverallHuman + 1
                        timeIndexInTurnHuman <- timeIndexInTurnHuman + 1
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
                                             timeIndexOverallHuman,
                                             timeIndexInTurnHuman,
                                             as.numeric(txt$f0mean[l]))
                      }
                    }
                  }
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
        timeIndexInTurnRobot <- 0
        turnOnset <- as.numeric(tg.getIntervalStartTime(tg, "robot", n))
        turnOffset <- as.numeric(tg.getIntervalEndTime(tg, "robot", n))
        for(p in 1:tg.getNumberOfIntervals(tg, "IPU")){
          if(tg.getLabel(tg, "IPU", p)==""){ # if the interval is labeled as empty (vs. as "xxx"), it means it's an IPU
            startIPU <- as.numeric(tg.getIntervalStartTime(tg, "IPU", p))
            endIPU <- as.numeric(tg.getIntervalEndTime(tg, "IPU", p))
            if(turnOnset <= startIPU){
              if(turnOffset >= endIPU){
                ipuCount <- ipuCount + 1
                for(l in 1:nrow(txt)){
                  if(txt$onset[l] >= startIPU){ # doing two separate if() statements because there's some confusion with the use of &(&)
                    if(txt$offset[l] <= endIPU){
                      if(!is.na(txt$f0mean[l])){
                        timeIndexOverallRobot <- timeIndexOverallRobot + 1
                        timeIndexInTurnRobot <- timeIndexInTurnRobot + 1
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
                                             timeIndexOverallRobot,
                                             timeIndexInTurnRobot,
                                             as.numeric(txt$f0mean[l]))
                      }
                    }
                  }
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

# check if f0 was extracted correctly
# also show in the plot if there are NAs

load(paste0(folder, "f0.RData"))

dat <- f0 %>% 
  mutate(participant = substr(file, 7, 9), # variable with name of speaker including for the robot subsets
         groupings = paste(speaker, condition, task, sep = "."),
         tgroup = paste(groupings, turn, sep="."))

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

# for each turn of the human, save `robPrevf0`, i.e. the f0 of the robot's previous turn
# for the first timeInfex of the human, robPrevf0 is the f0 of the robot's first timeIndex in the previous turn. human's second timeIndex is robot's second timeIndex u.s.w

# datsave <- dat
# dat <- datsave

# the following calculates the average f0 of each turn to then use it to calculate the difference between robot's and human's subsequent turns

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

m <- read.csv(paste0(folder2, file), sep=";") %>% 
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
  select(-c("turn", "gapDur", "f0Diff"))
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
                                 dac$timeIndexInTurn == dac$timeIndexInTurn[i]]
      previousf0Mock <- dac$f0mean[dac$speaker == dac$interlocutor[i] &
                                 dac$turn == dac$turn[i] &
                                 dac$condition != dac$condition[i] & # here use the other condition
                                 dac$timeIndexInTurn == dac$timeIndexInTurn[i]]
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
  previousf0 <- dac$f0mean[dac$speaker == dab$interlocutor[i] &
                             dac$condition == dab$prevCond[i] &
                             dac$timeIndexOverall == dab$timeIndexOverall[i]]
  if(!purrr::is_empty(previousf0)){
    if(!any(is.na(previousf0))){
      dab$robPrevf0[i] <- previousf0
    }
  }
  if(dab$condition[i] == substr(dab$Order[i], 1, 2)){
    previousf0Mock <- as.numeric(dac$f0mean[dac$speaker == dab$interlocutor[i] &
                                              dac$condition == dab$condition[i] &
                                              dac$timeIndexOverall == dab$timeIndexOverall[i]])
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

q <- read.csv(paste0(folder2, "questions.csv"), header=TRUE, dec=".", sep=";") %>% 
  select(-Q) %>% 
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
  select(speaker, GA.1:GA.12) %>% 
  distinct()
names(dpG) <- c("speaker", paste0("I", 1:12))
dpN <- dac %>% 
  filter(!grepl("Robot", speaker)) %>%
  select(speaker, NG.1:NG.12) %>% 
  distinct()
names(dpN) <- c("speaker", paste0("I", 1:12))
dp <- rbind(dpN, dpG)

pc <- principal_components(dp[,2:13], n = 3, threshold = 0., standardize = TRUE, rotation="varimax")
summary(pc)
# write.csv(pc, paste0(folder, "loadings2.csv"))

# rename questionnaire columns so they make sense
# also get rid of all the TMF columns: transform them into on TMF.F and one TMF.M

questionnaire <- data.frame(questionNumber = c(1:12),
                            question = c("conversationFlow",
                                         "floorYield",
                                         "floorHold",
                                         "robTiming",
                                         "faceHumanlike",
                                         "voiceHumanlike",
                                         "behaviorHumanlike",
                                         "generalHumanlike",
                                         "enjoyTalkingToRob",
                                         "positiveAboutRob",
                                         "positiveAboutConv",
                                         "comfortTalkingToRob"),
                            dimension = c(rep("PCConvQuality", 3), rep("PCRobotQuality", 4), NA, rep("PCConvQuality", 4))) # these components are based on the PCA above

dacsave <- dac
dac <- dacsave

daq <- dac %>%
  filter(!grepl("Robot", speaker)) %>% 
  select(speaker, GA.1:NG.12) %>% 
  distinct() %>%
  gather(key=condition, value=rating, -speaker) %>% 
  mutate(questionNumber = substr(condition, 4, nchar(condition)),
         condition = substr(condition, 1, 2))

daq <- merge(daq, questionnaire, by="questionNumber") %>% 
  group_by(speaker, dimension, condition) %>% 
  mutate(dimensionRating = mean(rating, na.rm=TRUE)) %>%
  ungroup() %>% 
  select(-questionNumber) %>% 
  spread(key=dimension, value=dimensionRating) %>% 
  spread(key=question, value = rating)

# this produces three rows per speaker/condition, each with a bunch of NAs and a couple of values.
# to put these rows together into one row per speaker/condition, do the following
# (taken from https://stackoverflow.com/questions/45515218/combine-rows-in-data-frame-containing-na-to-make-complete-row)

coalesce_by_column <- function(daq) {
  return(dplyr::coalesce(!!! as.list(daq)))
}

daq <- daq %>%
  group_by(speaker, condition) %>%
  summarise_all(coalesce_by_column)

dac <- dac %>%
  select(-c(GA.1:NG.12))

# join dac with the questionnaire data
# and then also reduce the dataset by calculating the TMF scores

dac <- full_join(dac, daq, by=c("speaker", "condition")) %>% 
  group_by(speaker) %>% 
  mutate(TMF.F = (TMF.F1+TMF.F2+TMF.F3+TMF.F4+TMF.F5+TMF.F6)/6,
         TMF.M = (TMF.M1+TMF.M2+TMF.M3+TMF.M4+TMF.M5+TMF.M6)/6) %>% 
  ungroup() %>% 
  select(-c(TMF.M1:TMF.F6, scenario))

# calculate the BFI scores (average of each dimension)

bfi <- read.csv(paste0(folder2, "adjectivesBFI.csv"), sep = ";")

db <- dac %>% 
  filter(!grepl("Robot", speaker)) %>% 
  select(speaker, BFI1:BFI40) %>% 
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
  select(-c("adjNumber", "rating", "adjectiveBFI", "inverted"))

# again, the rows with NAs...

coalesce_by_column <- function(db) {
  return(dplyr::coalesce(!!! as.list(db)))
}

db <- db %>%
  group_by(speaker) %>%
  summarise_all(coalesce_by_column)

dac <- merge(dac, db, by="speaker", all=TRUE) %>% 
  select(-c(BFI1:BFI40))

# save files

save(dac, file = paste0(folder, "dataConversation.RData"))
save(dab, file = paste0(folder, "dataBaseline.RData"))
