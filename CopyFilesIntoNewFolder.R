# Tom Offrede
# Copy files into new folder

# first get a list of all the speakers (taken from CreateNewFolder.R)

folder <- "C:/Users/offredet/Documents/1HU/ExperimentEyes/Data/All/"
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

# now copy the .txt files from the individual folders into the All folder

# folderSpeakers <- "C:/Users/offredet/Documents/1HU/ExperimentEyes/Data/PerSpeaker/"
# folderAll <- "C:/Users/offredet/Documents/1HU/ExperimentEyes/Data/All/"
# 
# for(s in sp){
#   folderCurrentSpeaker <- paste0(folderSpeakers, s, "/")
#   files <- list.files(folderCurrentSpeaker, "\\.txt")
#   files <- files[!grepl("Register", files)]
#   files <- paste0(folderCurrentSpeaker, files)
#   file.copy(files, folderAll)
# }

# copy only WAV files from All folder to another AllWAV folder

folderAll <- "C:/Users/offredet/Documents/1HU/ExperimentEyes/Data/All/"
folderAllWAV <- "C:/Users/offredet/Documents/1HU/ExperimentEyes/Data/AllWAV/"

files <- list.files(folderAll, "\\.WAV")
files <- paste0(folderAll, files)
file.copy(files, folderAllWAV)
