# Tom Offrede
# plots both channels of each stereo WAV file in a folder and lets you decide which channel to extract
# then creates a new WAV file only containing the channel you chose

library(tuneR)
library(tidyverse)

folder <- "C:/Users/offredet/Documents/1HU/ExperimentEyes/Data/All/" # folder containing original stereo WAV files
folderNew <- "C:/Users/offredet/Documents/1HU/ExperimentEyes/Data/OneChannel/" # folder where new WAV files will be saved

files <- list.files(folder, "\\.WAV")

for(i in files){
  s <- readWave(paste0(folder, i))
  sForViz <- downsample(s, 2500) # make a downsampled object to speed up the plotting (you can reduce this number even further)
  par(mfrow=c(2, 1))
  plot(sForViz@left, type="l", main=i)
  plot(sForViz@right, type="l")
  myInput <- readline("Channel to save:") # choose `1` for left channel (upper plot) or `2` for right channel (lower plot)
  ch <- ifelse(myInput=="1", "left", "right") # (actually if you input anything that's not `1` you'll also be choosing the `right` channel)
  newS <- channel(s, which=ch)
  writeWave(newS, paste0(folderNew, i))
}
