# Tom Offrede
# make plots for ICPhS poster

library(tidyverse)
library(lme4)
library(ggdist)
library(mediation)
library(viridis)
library(ggpubr)
library(grid)

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

theme_set(theme_minimal()+
            theme(panel.grid.major = element_blank(),
                  plot.title = element_text(size=30, hjust=0.5),
                  axis.title = element_text(size=26),
                  axis.text = element_text(size=10),
                  strip.text = element_text(size = 22, color="black")))

###############################################

# f0 convergence

## general

ggplot(dac, aes(robPrevf0mean, f0mean))+
  geom_point()+
  geom_smooth(method="lm")+
  labs(title="Overall Effect",
       x = "Robot's f0 mean",
       y = "Humans' f0 mean")
ggsave(file=paste0(folderFig, "convGeneral.png"), height=2206, width=2103, units = "px", dpi="print")


## individual examples
d <- dac %>% 
  filter(speaker%in%c("NLF", "UPR")) %>% 
  mutate(speaker=ifelse(speaker=="NLF", "Speaker 1", "Speaker 2"))

ggplot(d %>% filter(f0mean<174 & f0mean>90), aes(robPrevf0mean, f0mean))+
  geom_point()+
  geom_smooth(data=d, method="lm")+
  labs(title="Large Individual Variability!",
       x = "Robot's f0 mean",
       y = "Human's f0 mean")+
  facet_wrap(~speaker)+
  theme(plot.title = element_text(size=27),
        axis.title = element_text(size=24),
        axis.text = element_text(size=9),)
ggsave(file=paste0(folderFig, "individual.png"), height=2378, width=2103, units = "px", dpi="print")
# 2206

###########

# Questionnaire ratings

r <- dac %>% 
  filter(!duplicated(paste0(speaker, condition))) %>%
  dplyr::select(speaker, condition, PCConvQuality, PCEvalRobot) %>%
  pivot_longer(PCConvQuality:PCEvalRobot, names_to = "PC", values_to="rating")

ggplot(r, aes(PC, rating, color=condition))+
  geom_boxplot(notch=TRUE)+
  labs(title="Questionnaire Ratings",
       x = "",
       y = "Rating (higher: more positive)",
       color = "Condition")+
  scale_x_discrete(labels= c("Conv. Quality", "Eval. of Robot"))+
  scale_color_manual(labels = c("Gaze Aversion", "Fixed Gaze"), values=c("#365C8DFF", "#1FA187FF"))+
  theme(axis.text.x = element_text(color="black", size=20))
#46337EFF
ggsave(file=paste0(folderFig, "questionnaire.png"), height=2206, width=2103, units = "px", dpi="print")

(c <- ggplot(r %>% filter(PC=="PCConvQuality"), aes(condition, rating))+
    geom_boxplot(notch=TRUE, width=.13)+
    stat_halfeye(adjust = .5,  width = .5, justification = -.2, .width = c(.5, .95), fill="#9bafbd")+
    labs(title="Conversational Quality",
         x = "",
         y = "Ratings (higher: more positive)")+
    scale_x_discrete(labels= c("Gaze Aversion", "Fixed Gaze"))+
    theme(axis.text.x = element_text(color="black", size=20),
          panel.border = element_rect(colour = "black", fill=NA, size=1)))
# ggsave(file=paste0(folderFig, "convQual.png"), height=2206, width=2103, units = "px", dpi="print")

# (c2 <- ggarrange(c))
# grid.rect(width = .98, height = .98, gp = gpar(lwd = 1, col = "black", fill = NA))

(e <- ggplot(r %>% filter(PC=="PCEvalRobot"), aes(condition, rating))+
    geom_boxplot(notch=TRUE, width=.13)+
    stat_halfeye(adjust = .5,  width = .5, justification = -.2, .width = c(.5, .95), fill="#9bafbd")+
    labs(title="Evaluation of Robot",
         x = "",
         y = "Ratings (higher: more positive)")+
    scale_x_discrete(labels= c("Gaze Aversion", "Fixed Gaze"))+
    theme(axis.text.x = element_text(color="black", size=20),
          axis.text = element_text(size=11)))
ggsave(file=paste0(folderFig, "evalRobot.png"), height=2206, width=2103, units = "px", dpi="print")

(q <- ggarrange(c, e))
ggsave(file=paste0(folderFig, "questionnaire2.png"), height=2206, width=2103, units = "px", dpi="print")

###########

# f0 and intimacy

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
ggsave(file=paste0(folderFig, "f0-intimacy.png"), height=2206, width=2103, units = "px", dpi="print")

###########


