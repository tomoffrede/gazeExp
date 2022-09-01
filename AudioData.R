# play around with audio data, try to extract features
# try to extract / understand MFCCs

library(tuneR)
library(tidyverse)
library(rPraat)

`%!in%` <- Negate(`%in%`) 

folder <- "C:/Users/offredet/Documents/1HU/ExperimentEyes/Data/test/"

filesWAV <- list.files(folder, "WAV")

#### MFCCs

# s <- readWave(paste0(folder, files[[1]]))
# 
# m <- melfcc(s, numcep=13, frames_in_rows = FALSE)
# 
# d <- deltas(m)
# dd <- deltas(d)
# a <- rbind(m, d)
# b<- rbind(a, dd)

####

filesTG <- list.files(folder, "TextGrid")
filesTG <- filesTG[!grepl("VUV", filesTG)]
filesTXT <- list.files(folder, "txt")
filesTXT <- filesTXT[!grepl("Register", filesTXT)]

files <- data.frame(cbind(filesTG, filesTXT))
files <- files %>%
  mutate(worked = ifelse(substr(files$filesTG, 1, 9) == substr(files$filesTXT, 1, 9), "worked!", "NO!!!!"))

f0 <- data.frame(matrix(nrow=0, ncol=6))
names(f0) <- c("file", "speaker", "turn", "onset", "offset", "f0mean")

# do something like the following, but also calculating f0 mean per IPU (not entire turn)

for(i in 1:nrow(files)){
  tg <- tg.read(paste0(folder, files$filesTG[[i]]), encoding=detectEncoding(paste0(folder, files$filesTG[[i]])))
  txt <- read.table(paste0(folder, files$filesTXT[[i]]), header=TRUE, na.strings = "--undefined--")
  turnCount <- 0

  if(substr(files$filesTG[i], 4, 5) == "BL"){ # baseline speech!
    # get f0 mean for entire period? for time windows? get transcription?
    # for now just get the mean for the entire period
    start <- as.numeric(tg.getIntervalStartTime(tg, "speech", as.numeric(tg.findLabels(tg, "speech", "baseline"))))
    end <- as.numeric(tg.getIntervalEndTime(tg, "speech", as.numeric(tg.findLabels(tg, "speech", "baseline"))))
    f <- (txt %>%
            filter(onset >= start & offset <= end) %>%
            summarize(f = mean(f0mean, na.rm=TRUE)))[1,1] # "[1,1]" because `f` is a 1x1 matrix data frame
    f0[nrow(f0)+1,] <- c(substr(files$filesTG[i], 1, 9),
                         substr(files$filesTG[i], 7, 9),
                         "baseline",
                         start,
                         end,
                         as.numeric(f))
    
  } else if(substr(files$filesTG[i], 4, 5) == "CO"){ # conversation
    for(n in 1:tg.getNumberOfIntervals(tg, "participant")){
      if(tg.getLabel(tg, "participant", n) == "s"){
        turnCount <- turnCount + 1
        turnOnset <- as.numeric(tg.getIntervalStartTime(tg, "participant", n))
        turnOffset <- as.numeric(tg.getIntervalEndTime(tg, "participant", n))
        f <- (txt %>%
          filter(onset >= turnOnset & offset <= turnOffset) %>%
          summarize(f = mean(f0mean, na.rm=TRUE)))[1,1] # "[1,1]" because `f` is a 1x1 matrix data frame
        f0[nrow(f0)+1,] <- c(substr(files$filesTG[i], 1, 9),
                             substr(files$filesTG[i], 7, 9),
                             turnCount,
                             turnOnset,
                             turnOffset,
                             as.numeric(f))
      }
    }
    for(n in 1:tg.getNumberOfIntervals(tg, "robot")){
        if(tg.getLabel(tg, "robot", n) == "s"){
          turnCount <- turnCount + 1
          turnOnset <- as.numeric(tg.getIntervalStartTime(tg, "robot", n))
          turnOffset <- as.numeric(tg.getIntervalEndTime(tg, "robot", n))
          f <- (txt %>%
                  filter(onset >= turnOnset & offset <= turnOffset) %>%
                  summarize(f = mean(f0mean, na.rm=TRUE)))[1,1] # "[1,1]" because `f` is a 1x1 matrix data frame
          f0[nrow(f0)+1,] <- c(substr(files$filesTG[i], 1, 9),
                               paste0(substr(files$filesTG[i], 7, 9), "-Robot"),
                               turnCount,
                               turnOnset,
                               turnOffset,
                               as.numeric(f))
        }
    }
  }
}

f0$condition <- substr(f0$file, 1, 2)
f0$task <- substr(f0$file, 4, 5)


######################

# check if all files are in the folder

folder <- "C:/Users/offredet/Documents/1HU/ExperimentEyes/Data/"

files <- list.files(folder, "\\.WAV|\\.TextGrid")

ftg <- files[grepl("TextGrid", files)]
fwv <- files[grepl("WAV", files)]

fwv[substr(fwv, 1, 9) %!in% substr(ftg, 1, 9)]
fwv[substr(ftg, 1, 9) %!in% substr(fwv, 1, 9)] # both of these should be empty -- it means all the WAVs have texgrids and vice-versa

all <- data.frame(cbind(ftg, fwv))
all$worked[substr(fwv, 1, 9) == substr(ftg, 1, 9)] <- "worked!"
all$worked[substr(fwv, 1, 9) != substr(ftg, 1, 9)] <- "NO!!!"
unique(all$worked)

all$speaker <- substr(all$ftg, 7, 9)
length(unique(all$speaker)) # 33 speakers
table(all$speaker) # all with 4 files (2 baselines and 2 conversations)

# all files here :)

################