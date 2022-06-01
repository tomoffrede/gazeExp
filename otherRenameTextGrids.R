# Get all TextGrids
# Write new TextGrids with different name

library(rPraat)

folder <- "C:/Users/tomof/Documents/1HU/ExperimentEyes/Data/"
newfolder <- "C:/Users/tomof/Documents/1HU/ExperimentEyes/Data/PreviousTextGrids/"


listfiles <- list.files(folder, "\\.TextGrid")
oglist <- listfiles[!grepl("auses", listfiles)]
complete <- listfiles[grepl("withPauses", listfiles)]

for(i in oglist){
  tg <- tg.read(paste0(folder, i), encoding=detectEncoding(paste0(folder, i)))
  name <- gsub(".TextGrid", "_onlyTurns.TextGrid", i)
  tg.write(tg, paste0(newfolder, name))
}

for(i in complete){
  tg <- tg.read(paste0(folder, i), encoding=detectEncoding(paste0(folder, i)))
  name <- gsub("_withPauses", "", i)
  tg.write(tg, paste0(folder, name))
}

# Done :)