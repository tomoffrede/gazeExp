# Tom Offrede
# Figures for poster for P&P conference (Bielefeld, 2022)

{library(tidyverse)
  library(ggdist)
  folder <- "C:/Users/offredet/Documents/1HU/ExperimentEyes/Data/All/"
  folder2 <- "C:/Users/offredet/Documents/1HU/ExperimentEyes/Data/"
  folderFig <- "C:/Users/offredet/Documents/1HU/Courses, conferences, workshops/P&P-Bielefeld/"
  files <- list.files(folder, "RData")
  fileConv <- files[grepl("Conversation", files)]
  fileBase <- files[grepl("Baseline", files)]
  load(paste0(folder, fileConv))
  load(paste0(folder, fileBase))
  dac <- dac %>%
    mutate_at(c("turnDur", "turn", "f0mean", "robPrevf0", "gapDur"), as.numeric) %>%
    mutate_at(c("speaker", "condition", "Order", "task"), as.factor) %>% 
    group_by(speaker) %>% 
    mutate(turnDurNormal = (turnDur - min(turnDur)) / (max(turnDur) - min(turnDur)),
           f0z = (f0mean - min(f0mean)) / (max(f0mean) - min(f0mean)),
           gapDurNormal = (gapDur - min(gapDur)) / (max(gapDur) - min(gapDur)),
           f0DiffNormal = (f0Diff - min(f0Diff)) / (max(f0Diff) - min(f0Diff)),
           robf0Normal = (robPrevf0 - min(robPrevf0, na.rm=TRUE)) / (max(robPrevf0, na.rm=TRUE) - min(robPrevf0, na.rm=TRUE))) %>% 
    ungroup()
  dab <- dab %>%
    mutate_at(c("condition"), as.factor)
  ga <- read.csv(paste0(folder2, "simpleGazeData.csv"), sep = ";")
}

# following is some code that I won't use but may be useful
# you can create one plot with two different regression lines, one for each condition
{
  # dNG <- dac %>% filter(condition=="NG")
  # dGA <- dac %>% filter(condition=="GA")
  #
  # modelNG <- lm(f0z ~ robf0Normal, dNG)
  # modelGA <- lm(f0z ~ robf0Normal, dGA)
  #
  # dNG$predicted.y <- predict.lm(modelNG, newdata=dNG)
  # dGA$predicted.y <- predict.lm(modelGA, newdata=dGA)
  #
  #
  # ggplot()+
  #   geom_point(dNG, mapping=aes(robf0Normal, f0z), color="blue")+
  #   geom_line(dNG, mapping=aes(robf0Normal, predicted.y), color="blue")+
  #   geom_point(dGA, mapping=aes(robf0Normal, f0z), color="red")+
  #   geom_line
  
  ggplot(dac, aes(robf0Normal, f0z, color=condition))+
    # geom_point()+
    geom_smooth(method="lm")
}

png(paste0(folderFig, "gapDur.png"))
ggplot(dac, aes(turnNormal, gapDurNormal))+
  geom_point()+
  facet_wrap(~condition)+
  geom_smooth(method="lm")
dev.off()

png(paste0(folderFig, "gapDurAlternative.png"))
ggplot(dac, aes(condition, gapDurNormal))+
  stat_halfeye(adjust = .6,  width = .6, justification = -.2, .width = c(.5, .95))+
  geom_boxplot(width=.1)
dev.off()

png(paste0(folderFig, "turnDur.png"))
ggplot(dac %>% filter(!grepl("Robot", speaker)), aes(turnNormal, turnDurNormal))+
  geom_point()+
  facet_wrap(~condition)+
  geom_smooth(method="lm")
dev.off()

png(paste0(folderFig, "f0.png"))
ggplot(dac, aes(robf0Normal, f0z))+
  geom_point()+
  facet_wrap(~condition)+
  geom_smooth(method="lm")
dev.off()

png(paste0(folderFig, "f0Alternative.png"))
ggplot(dac, aes(robf0Normal, f0z, color=condition))+
  # geom_point()+
  geom_smooth(method="lm")
dev.off()

png(paste0(folderFig, "gaze.png"))
ggplot(ga, aes(condition, rateOfGA))+
  stat_halfeye(adjust = .5,  width = .5, justification = -.2, .width = c(.5, .95))+
  geom_boxplot(width=.13)+
  coord_cartesian(xlim = c(1.2, NA))
dev.off()
