# Tom Offrede
# Figures for Defense slides

Sys.setenv(LANG="en")
library(tidyverse)
library(lme4)
library(ggdist)
library(mediation)
library(ggpubr)
library(grid)
`%!in%` <- Negate(`%in%`)

folder <- "C:/Users/offredet/Documents/1HU/ExperimentEyes/Data/All/"
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
         IPUDurC = IPUDur - mean(IPUDur, na.rm=TRUE),
         condition = case_when(
           condition == "GA" ~ "Gaze Aversion",
           condition == "NG" ~ "Fixed Gaze"
         ))

theme_set(theme_minimal()+
            theme(axis.ticks.y=element_blank(),
                  axis.ticks.x=element_blank(),
                  axis.title = element_text(size=24),
                  axis.text.x = element_text(color="black", size=24),
                  plot.background = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  plot.title = element_text(hjust = 0.5, size=24),
                  strip.background = element_blank(),
                  strip.text = element_text(size=24)
            ))

ggplot(dac, aes(robPrevf0mean, f0mean))+
  geom_point()+
  geom_smooth(method="lm")+
  labs(title=NULL,
       x = "Robot's f0 mean",
       y = "Humans' f0 mean")
ggsave("C:/Users/offredet/Documents/1HU/Defense/figures-PreviousExp/gaze-generalConv.png", width=4000, height=3000, units="px", dpi=500)

ggplot(dac, aes(robPrevf0mean, f0mean))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_wrap(~condition)+
  labs(title=NULL,
       x = "Robot's f0 mean",
       y = "Humans' f0 mean")
ggsave("C:/Users/offredet/Documents/1HU/Defense/figures-PreviousExp/gaze-generalConvConditions.png", width=5000, height=3000, units="px", dpi=500)

###
i <- dac %>% 
  mutate(intimMean = case_when(
    intimMean>1.19 & intimMean<1.12 ~ (9/12)*1,
    intimMean>1.82 & intimMean<1.83 ~ (9/12)*2,
    intimMean>1.83 & intimMean<1.84 ~ (9/12)*3,
    intimMean>1.97 & intimMean<2 ~ (9/12)*4,
    intimMean>3 & intimMean<3.1 ~ (9/12)*5,
    intimMean>3.5 & intimMean<3.6 ~ (9/12)*6,
    intimMean>4 & intimMean<4.1 ~ (9/12)*7,
    intimMean>4.3 & intimMean<4.4 ~ (9/12)*8,
    intimMean>5.2 & intimMean<5.3 ~ (9/12)*9,
    intimMean>5.4 & intimMean<5.5 ~ (9/12)*10,
    intimMean>6.5 & intimMean<6.6 ~ (9/12)*11,
    intimMean>7 & intimMean<7.1 ~ (9/12)*12
  ))

ggplot(i, aes((intimMean), f0mean))+
  stat_halfeye(adjust = .5,  width = .5, justification = -.2, .width = c(.5, .95), fill="#9bafbd")+
  geom_smooth(method="lm")+
  labs(title="Intimacy and f0",
       x = "Question's Intimacy",
       y = "f0 mean")
ggsave("C:/Users/offredet/Documents/1HU/Defense/figures-PreviousExp/gaze-f0Intimacy.png", width=4000, height=3000, units="px", dpi=500)
















