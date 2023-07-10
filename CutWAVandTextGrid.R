# Tom Offrede
# edit textgrid and wav file for figure for ICPhS paper

library(tuneR)
library(rPraat)
library(tidyverse)

folder <- "C:/Users/offredet/Documents/1HU/ExperimentEyes/Data/"
filewav <- list.files(folder, "GA-CO-FQD.WAV")
filetg <- list.files(folder, "GA-CO-FQD.TextGrid")

tg0 <- tg.read(paste0(folder, filetg), encoding=detectEncoding(paste0(folder, filetg)))
startRobotSection <- as.numeric(tg.getIntervalStartTime(tg0, "robot", as.numeric(tg.findLabels(tg0, "robot", "Robot's IPU 1"))))
endRobotSection <- as.numeric(tg.getIntervalEndTime(tg0, "robot", as.numeric(tg.findLabels(tg0, "robot", "Robot's IPU 3"))))
startHumanSection <- as.numeric(tg.getIntervalStartTime(tg0, "participant", as.numeric(tg.findLabels(tg0, "participant", "Human's IPU 1"))))
endHumanSection <- as.numeric(tg.getIntervalEndTime(tg0, "participant", as.numeric(tg.findLabels(tg0, "participant", "Human's IPU 3"))))
tgR <- tg.cut(tg0, startRobotSection, endRobotSection)
tgH <- tg.cut(tg0, startHumanSection, endHumanSection)
tg <- tg.createNewTextGrid(startRobotSection, endHumanSection)
tg <- tg.insertNewIntervalTier(tg, newInd = 1, "Robot")
tg <- tg.insertNewIntervalTier(tg, newInd = 2, "Human")
for(i in 1:tg.getNumberOfIntervals(tgR, 1)){
  tg <- tg.insertInterval(tg, "Robot", as.numeric(tg.getIntervalStartTime(tgR, 1, i)), as.numeric(tg.getIntervalEndTime(tgR, 1, i)), label=tg.getLabel(tgR, 1, i))
}
for(i in 1:tg.getNumberOfIntervals(tgH, 2)){
  tg <- tg.insertInterval(tg, "Human", as.numeric(tg.getIntervalStartTime(tgH, 2, i)), as.numeric(tg.getIntervalEndTime(tgH, 2, i)), label=tg.getLabel(tgH, 2, i))
}
tg.plot(tg)

tg.write(tg, paste0(folder, "GA-CO-FQD_cut.TextGrid"))

wav <- readWave(paste0(folder, filewav))
wav@left <- wav@left[c(startRobotSection*wav@samp.rate : endRobotSection*wav@samp.rate , startHumanSection*wav@samp.rate : endHumanSection*wav@samp.rate)]
writeWave(wav, paste0(folder, "GA-CO-FQD_cut.WAV"))
