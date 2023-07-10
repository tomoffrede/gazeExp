# Tom Offrede
# make plots for ICPhS poster

library(tidyverse)
library(lme4)
library(ggdist)
library(mediation)
library(viridis)

`%!in%` <- Negate(`%in%`)

folder <- "C:/Users/offredet/Documents/1HU/ExperimentEyes/Data/All/"
folderFig <- "C:/Users/offredet/Documents/1HU/ExperimentEyes/gazeExp/icphs_figures/"
files <- list.files(folder, "RData")
load(paste0(folder, "dataConversation.RData"))

dac <- dac[!grepl("Impairment|Dyslexia|Gender|Education|Age", names(dac))]

dac <- dac %>%
  filter(!grepl("Robot", speaker)) %>% 
  mutate_at(c("turnDur", "turn", "f0mean", "f0sd", "intensMean", "intensMax", "robPrevf0mean", "robPrevf0meanMock", "gapDur", "intimMean", "intimSD", "intimMed", "IPUDur", "robPrevf0sd", "robPrevf0sdMock", "robPrevIntMean", "robPrevIntMeanMock", "robPrevIntMax", "robPrevIntMaxMock"), as.numeric) %>%
  mutate_at(c("speaker", "condition", "Order", "task"), as.factor) %>% 
  group_by(speaker) %>%
  mutate(turnDurNormal = (turnDur - min(turnDur, na.rm = TRUE)) / (max(turnDur, na.rm = TRUE) - min(turnDur, na.rm = TRUE)),
         turnDurc = turnDur - mean(turnDur, na.rm=TRUE),
         robf0meanc = robPrevf0mean - mean(robPrevf0mean, na.rm=TRUE),
         robf0sdc = robPrevf0sd - mean(robPrevf0sd, na.rm=TRUE),
         robIntMeanc = robPrevIntMean - mean(robPrevIntMean, na.rm=TRUE),
         robIntMaxc = robPrevIntMax - mean(robPrevIntMax, na.rm=TRUE),
         intimc = intimMean - mean(intimMean, na.rm=TRUE),
         intensMeanC = intensMean - mean(intensMean, na.rm=TRUE),
         intensMaxC = intensMax - mean(intensMax, na.rm=TRUE),
         f0CV = f0sd / f0mean,
         robf0CV = robPrevf0sd / robPrevf0mean,
         robf0CVc = robf0CV - mean(robf0CV, na.rm=TRUE),
         mockf0meanC = robPrevf0meanMock - mean(robPrevf0meanMock, na.rm=TRUE),
         mockf0sdC = robPrevf0sdMock - mean(robPrevf0sdMock, na.rm=TRUE),
         mockf0CV = robPrevf0sdMock / robPrevf0meanMock,
         mockf0CVc = mockf0CV - mean(mockf0CV, na.rm=TRUE),
         f0sdZ = (f0sd - mean(f0sd, na.rm=TRUE))/sd(f0sd, na.rm=TRUE),
         robf0sdZ = robf0sdc / sd(robPrevf0sd, na.rm=TRUE),
         f0CVz = (f0CV - mean(f0CV, na.rm=TRUE))/sd(f0CV, na.rm=TRUE),
         robf0CVz = robf0CVc / sd(robf0CV, na.rm=TRUE),
         f0meanZ = (f0mean - mean(f0mean, na.rm=TRUE))/sd(f0mean, na.rm=TRUE),
         robf0meanZ = robf0meanc / sd(robPrevf0mean, na.rm = TRUE),
         f0meanc = (f0mean - mean(f0mean, na.rm=TRUE)/sd(f0mean, na.rm=TRUE)),
         f0sdc = (f0sd - mean(f0sd, na.rm=TRUE)/sd(f0sd, na.rm=TRUE))) %>%
  ungroup() %>% 
  mutate(ESc = EmotionalStability - mean(EmotionalStability, na.rm=TRUE),
         Agreec = Agreeableness - mean(Agreeableness, na.rm=TRUE),
         Conscc = Conscientiousness - mean(Conscientiousness, na.rm=TRUE),
         Extrac = Extraversion - mean(Extraversion, na.rm=TRUE),
         IOc = IntellectOpenness - mean(IntellectOpenness, na.rm=TRUE),
         ConvQualc = PCConvQuality - mean(PCConvQuality, na.rm=TRUE),
         EvalRobotc = PCEvalRobot - mean(PCEvalRobot, na.=TRUE),
         IPUDurC = IPUDur - mean(IPUDur, na.rm=TRUE)) #%>% 
# filter(scoreEN > 60) # see with and without low-level participants

theme_set(theme_minimal(base_size = 20)+
            theme(panel.grid.major = element_blank()))

###############################################

# General f0 effect

ggplot(dac, aes(robPrevf0mean, f0mean))+
  geom_point()+
  geom_smooth(method="lm")+
  labs(title="Robot's f0 Influence on Humans' f0",
       x = "Robot's f0 mean",
       y = "Humans' f0 mean")
ggsave(file=paste0(folderFig, "speakers-conv.png"), height=5000, width=5000, units = "px")
folder2 <- "C:/Users/offredet/Documents/1HU/ExperimentEyes/Data/"

# Ratings per condition
r <- dac %>% 
  filter(!duplicated(paste0(speaker, condition))) %>%
  dplyr::select(speaker, condition, PCConvQuality, PCEvalRobot) %>%
  pivot_longer(PCConvQuality:PCEvalRobot, names_to = "PC", values_to="rating")

ggplot(r, aes(PC, rating, color=condition))+
  geom_boxplot()+
  labs(title="Humans' Ratings of Robots",
       x = "",
       y = "Rating",
       color = "Condition")+
  scale_x_discrete(labels= c("Conv. Quality", "Eval. of Robot"))+
  scale_color_manual(labels = c("Gaze Aversion", "Fixed Gaze"), values=c("#365C8DFF", "#1FA187FF"))
#46337EFF
ggsave(file=paste0(folderFig, "speakers-conv.png"), height=5000, width=5000, units = "px")
folder2 <- "C:/Users/offredet/Documents/1HU/ExperimentEyes/Data/"
