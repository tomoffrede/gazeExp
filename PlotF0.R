# Tom Offrede
# Plot f0 extracted on Praat to inspect it

library(tidyverse)


folder <- "C:/Users/offredet/Documents/1HU/ExperimentEyes/Data/All/"
folderPlots <- "C:/Users/offredet/Documents/1HU/ExperimentEyes/Data/f0Plots/"

files <- list.files(folder, "\\.txt")

for(i in files){
  f <- read.table(paste0(folder, i), header=TRUE, na.strings = "--undefined--")
  name <- gsub(".txt", "", i)
  png(paste0(folderPlots, name, ".png"))
  plot(f$f0mean)
  dev.off()
}