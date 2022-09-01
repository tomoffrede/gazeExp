# Tom Offrede
# Create a new folder for each speaker in the dataset
# A large part of this was taken and adapted from AudioData.R

library(tidyverse)

folder <- "C:/Users/offredet/Documents/1HU/ExperimentEyes/Data/"
folderAllSpeakers <- "C:/Users/offredet/Documents/1HU/ExperimentEyes/Data/PerSpeaker/"

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
sp <- unique(all$speaker)

for(s in sp){
  dir.create(paste0(folderAllSpeakers, s))
}