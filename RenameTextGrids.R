# Rename the textgrids that contain "(1)" or "(2)" in their names

library(rPraat)

folder <- "C:/Users/offredet/Documents/1HU/ExperimentEyes/Data/AllWAV/"
folder2 <- "C:/Users/offredet/Documents/1HU/ExperimentEyes/Data/AllWAV/renamed/"

files <- list.files(folder, "TextGrid")
# files <- files[grepl("\\(", files)]

for(i in files){
  # if(grepl("(1)", i)){
  #   newName <- gsub(" \\(1\\)", "", i)
  # } else if(grepl("(2)", i)){
  #   newName <- gsub(" \\(2\\)", "", i)
  # }
  newName <- gsub("\\.TextGrid", "_pauses.TextGrid", i)

  old <- tg.read(paste0(folder, i), encoding=detectEncoding(paste0(folder, i)))
  tg.write(old, paste0(folder2, newName))
}

# now check if they're the same

oldfiles <- files
newfiles <- list.files(folder2)

all <- data.frame(cbind(oldfiles, newfiles))

for(i in 1:nrow(all)){
  old <- tg.read(paste0(folder, all$oldfiles[i]), encoding=detectEncoding(paste0(folder, all$oldfiles[i])))
  new <- tg.read(paste0(folder2, all$newfiles[i]), encoding=detectEncoding(paste0(folder2, all$newfiles[i])))
  
  print(ifelse(identical(old, new), "Ok!", "NOOO!!!!"))
}

# apparently they're somehow different, but if you check them visually with tg.plot() (or on Praat), they're the same!