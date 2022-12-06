# Tom Offrede
# Plot f0 extracted on Praat to inspect it

library(tidyverse)


folder <- "C:/Users/offredet/Documents/1HU/ExperimentEyes/Data/All/"
folderPlots <- "C:/Users/offredet/Documents/1HU/ExperimentEyes/Data/f0Plots/"

# F0 extracted with Praat

# folder <- "C:/Users/offredet/Documents/1HU/ExperimentEyes/Data/PerSpeaker/QQE/"
# folderPlots <- "C:/Users/offredet/Documents/1HU/ExperimentEyes/f0par/"

files <- list.files(folder, "\\.txt")
files <- files[!grepl("Register", files)]

for(i in files){
  f <- read.table(paste0(folder, i), header=TRUE, na.strings = "--undefined--")
  name <- gsub(".txt", "", i)
  png(paste0(folderPlots, name, ".png"))
  plot(f$f0mean)
  dev.off()
}

# F0 averaged over IPUs

load(paste0(folder, "DataConversation.RData"))

for(s in unique(dac$speaker)){
  d <- dac %>% filter(speaker == s, !is.na(f0mean))
  
  png(paste0(folderPlots, s, ".png"), width=750, height=750)
  plot(d$turn, d$f0mean, main = paste0(s, " - data points: ", nrow(d)), cex=3)
  dev.off()
}












