# play around with audio data, try to extract features
# try to extract / understand MFCCs

library(tuneR)
library(tidyverse)

folder <- "C:/Users/tomof/Documents/1HU/ExperimentEyes/Data/test/"

files <- list.files(folder, "WAV")

s <- readWave(paste0(folder, files[[1]]))

m <- melfcc(s, numcep=13, frames_in_rows = FALSE)

d <- deltas(m)
dd <- deltas(d)

a <- rbind(m, d)
b<- rbind(a, dd)
