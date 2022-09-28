# Tom Offrede
# Figures for poster for P&P conference (Bielefeld, 2022)

{library(tidyverse)
  library(ggdist)
  library(RColorBrewer)
  library(ggsignif)
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
  files <- list.files(folder2)
  h <- read.csv(paste0(folder2, files[grepl("metadata", files)])) %>% 
    select(-c(GA.1:GA.4, GA.9:GA.12, NG.1:NG.4, NG.9:NG.12, BFI1:BFI40, TMF.M1:TMF.F6)) %>% 
    mutate_at("Order", as.factor)
  h$HL.GA <- (h$GA.5 + h$GA.6 + h$GA.7 + h$GA.8)/4
  h$HL.NG <- (h$NG.5 + h$NG.6 + h$NG.7 + h$NG.8)/4
  h1 <- h %>% 
    gather(condition, rating, c(HL.NG, HL.GA), factor_key = TRUE)
  order <- c("NG", "GA")
  orderHL <- c("HL.NG", "HL.GA")
  labels <- c("NG"="Robot Staring", "GA"="Robot Looks Away")
  labelsHL <- c("HL.NG"="Robot Staring", "HL.GA"="Robot Looks Away")
  dac$condition <- factor(dac$condition, levels=c("NG", "GA")) # to control the order in which the conditions appear with facet_wrap()
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
  order <- c("NG", "GA")
  orderHL <- c("HL.NG", "HL.GA")
}

png(paste0(folderFig, "gapDur.png"))
ggplot(dac, aes(turnNormal, gapDurNormal))+
  theme_minimal()+
  geom_point()+
  facet_wrap(~condition, labeller = as_labeller(labels))+
  geom_smooth(method="lm", size=2.5, fill="black")+
  labs(title = "Between-Turn Gap Duration",
       x = "Turn Index",
       y = "Gap Duration")+
  theme(plot.title = element_text(size=30, hjust=0.5),
        axis.title = element_text(size=26),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        strip.background = element_rect(fill="white", color="black"),
        strip.text = element_text(size=20),
        axis.ticks = element_blank(),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())
# ggsave(paste0(folderFig, "gapDur.png"))
dev.off()

png(paste0(folderFig, "gapDurAlternative.png"))
ggplot(dac, aes(condition, gapDurNormal))+
  theme_minimal()+
  stat_halfeye(adjust = .6,  width = .6, justification = -.2, .width = c(.5, .95))+
  geom_boxplot(width=.1)+
  scale_x_discrete(limits = order, labels=labels)+
  labs(title = "Between-Turn Gap Duration",
       x = "",
       y = "Gap Duration")+
  geom_signif(comparisons = list(c("NG", "GA")), annotations = "*", textsize = 6)+
  theme(plot.title = element_text(size=30, hjust=0.5),
        axis.title = element_text(size=26),
        axis.text.x = element_text(size=24, color="black"),
        axis.text.y = element_text(size=10),
        strip.background = element_rect(fill="white", color="black"),
        strip.text = element_text(size=20),
        axis.ticks = element_blank(),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())
# ggsave(paste0(folderFig, "gapDurAlternative.png"))
dev.off()

png(paste0(folderFig, "turnDur.png"))
ggplot(dac %>% filter(!grepl("Robot", speaker)), aes(turnNormal, turnDurNormal))+
  theme_minimal()+
  geom_point()+
  facet_wrap(~condition, labeller = as_labeller(labels))+
  geom_smooth(method="lm", size=2.5, fill="black")+
  labs(title = "Turn Duration",
       x = "Turn Index",
       y = "Turn Duration")+
  theme(plot.title = element_text(size=30, hjust=0.5),
        axis.title = element_text(size=26),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        strip.background = element_rect(fill="white", color="black"),
        strip.text = element_text(size=20),
        axis.ticks = element_blank(),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())
# ggsave(paste0(folderFig, "turnDur.png"))
dev.off()

png(paste0(folderFig, "f0.png"))
ggplot(dac, aes(robf0Normal, f0z))+
  theme_minimal()+
  geom_point()+
  facet_wrap(~condition, labeller = as_labeller(labels))+
  geom_smooth(method="lm", size=2.5, fill="black", color="#00376c")+
  labs(title = "Fundamental Frequency",
       x = "Robot's previous f0",
       y = "Human's current f0")+
  theme(plot.title = element_text(size=30, hjust=0.5),
        axis.title = element_text(size=26),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        strip.background = element_rect(fill="white", color="black"),
        strip.text = element_text(size=20),
        axis.ticks = element_blank(),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())
# ggsave(paste0(folderFig, "f0.png"))
dev.off()

png(paste0(folderFig, "f0Alternative.png"))
ggplot(dac, aes(robf0Normal, f0z, color=condition))+
  theme_minimal()+
  geom_smooth(method="lm")+
  labs(title = "Fundamental Frequency",
       x = "Robot's previous f0",
       y = "Human's current f0")+
  theme(plot.title = element_text(size=30, hjust=0.5),
        axis.title = element_text(size=26),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        strip.background = element_rect(fill="white", color="black"),
        strip.text = element_text(size=20),
        axis.ticks = element_blank(),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())
# ggsave(paste0(folderFig, "f0Alternative.png"))
dev.off()

png(paste0(folderFig, "gaze.png"))
ggplot(ga, aes(condition, rateOfGA))+
  theme_minimal()+
  stat_halfeye(adjust = .5,  width = .5, justification = -.2, .width = c(.5, .95), , fill="#9bafbd")+
  geom_boxplot(width=.13, notch = TRUE)+
  coord_cartesian(xlim = c(1.2, NA))+
  scale_x_discrete(limits = order, labels=labels)+
  labs(title = "Humans' Gaze Aversion",
       x = "",
       y = "Rate of Gaze Aversion")+
  geom_signif(comparisons = list(c("NG", "GA")), annotations = "*", textsize = 6)+
  theme(plot.title = element_text(size=30, hjust=0.5),
        axis.title.y = element_text(size=26),
        axis.text.x = element_text(size=24, color="black"),
        axis.text.y = element_text(size=10),
        axis.ticks = element_blank(),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())
# ggsave(paste0(folderFig, "gaze.png"))
dev.off()

png(paste0(folderFig, "humanLikeness.png"))
ggplot(h1, aes(condition, rating))+
  theme_minimal()+
  stat_halfeye(adjust = .5,  width = .5, justification = -.2, .width = c(.5, .95), fill="#9bafbd")+
  geom_boxplot(width=.13, notch = TRUE)+
  coord_cartesian(xlim = c(1.2, NA))+
  scale_x_discrete(limits = orderHL, labels=labelsHL)+
  labs(title = "Human-Likeness Ratings",
       x = "",
       y = "Humans' Ratings of the Robots")+
  geom_signif(comparisons = list(c("HL.NG", "HL.GA")), annotations = "*", textsize = 7)+
  theme(plot.title = element_text(size=36, hjust=0.5),
        axis.title.y = element_text(size=28),
        axis.text.x = element_text(size=26, color="black"),
        axis.text.y = element_text(size=10),
        axis.ticks = element_blank(),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())
# ggsave(paste0(folderFig, "humanLikeness.png"))
dev.off()

