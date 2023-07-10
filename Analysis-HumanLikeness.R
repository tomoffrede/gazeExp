# Tom Offrede
# Analyze the participants' ratings of robots' human-likeness in HRI gaze aversion experiment

library(tidyverse)
library(lme4)
library(ggdist)
`%!in%` <- Negate(`%in%`)

folder <- "C:/Users/offredet/Documents/1HU/ExperimentEyes/Data/"
files <- list.files(folder)

h <- read.csv(paste0(folder, files[grepl("metadata", files)]), sep=";") %>% 
  select(-c(GA.1:GA.4, GA.9:GA.12, NG.1:NG.4, NG.9:NG.12, BFI1:BFI40, TMF.M1:TMF.F6)) %>% 
  mutate_at("Order", as.factor)

# Humanlikeness items:
# [GA-, NG-] [5:8]
# 5: The robot’s face was very human-like.
# 6: The robot’s voice was very human-like.
# 7: The robot’s behavior was very human-like.
# 8: Throughout the conversation, I felt like I was talking to a human.

# see all the humanlikeness ratings pooled together
h$HL.GA <- (h$GA.5 + h$GA.6 + h$GA.7 + h$GA.8)/4
h$HL.NG <- (h$NG.5 + h$NG.6 + h$NG.7 + h$NG.8)/4
h1 <- h %>% 
  gather(condition, rating, c(HL.NG, HL.GA), factor_key = TRUE)

ggplot(h1, aes(condition, rating))+
  stat_halfeye(adjust = .5,  width = .5, justification = -.2, .width = c(.5, .95))+
  geom_boxplot(width=.13)+
  coord_cartesian(xlim = c(1.2, NA))

summary(lmer(rating ~ condition + (1|Participant), h1))
# if anything, people find the no-gaze-aversion more humanlike! opposite of our hypothesis

# let's look at the individual ratings of each item

h2 <- h %>% 
  gather(item, rating, GA.5:NG.8, factor_key = TRUE) %>% 
  mutate(condition = substr(item, 1, 2)) %>% 
  mutate_at("condition", as.factor) %>% 
  mutate(item = ifelse(grepl("5", item), "face",
                      ifelse(grepl("6", item), "voice",
                             ifelse(grepl("7", item), "behavior",
                                    "feel"))))
  
# Item 5 (Face)
ggplot(h2 %>% filter(item == "face"), aes(condition, rating))+
  geom_boxplot()
summary(lmer(rating ~ condition + (1|Participant), h2 %>% filter(item == "face")))
# no effect of GA on face humanlikeness

# Item 6 (Voice)
ggplot(h2 %>% filter(item == "voice"), aes(condition, rating))+
  geom_boxplot()
summary(lmer(rating ~ condition + (1|Participant), h2 %>% filter(item == "voice")))
# NG: voice more humanlike (opposite of hypothesis)

# Item 7 (Behavior)
ggplot(h2 %>% filter(item == "behavior"), aes(condition, rating))+
  geom_boxplot()
summary(lmer(rating ~ condition + (1|Participant), h2 %>% filter(item == "behavior")))
# NG: trend for behavior to be more humanlike (opposite of hypothesis; t - 1.936)

# Item 8 (Feel like talking to a human)
ggplot(h2 %>% filter(item == "feel"), aes(condition, rating))+
  geom_boxplot()
summary(lmer(rating ~ condition + (1|Participant), h2 %>% filter(item == "feel")))
# no effect of GA on overall feel of humanlikess ("feel like talking to a human")


#####

# let's see if there's a difference between people that saw GA first and those that saw NG first
# basically do everything again but add an Order interaction

ggplot(h1, aes(condition, rating))+
  geom_boxplot()+
  facet_wrap(~Order)
summary(lmer(rating ~ condition + (1|Participant), h1 %>% filter(Order=="GAFirst")))
summary(lmer(rating ~ condition + (1|Participant), h1 %>% filter(Order=="NGFirst")))
# still the opposite effect than expected for both groups

# Item 5 (Face)
ggplot(h2 %>% filter(item == "face"), aes(condition, rating))+
  geom_boxplot()+
  facet_wrap(~Order)
summary(lmer(rating ~ condition + (1|Participant), h2 %>% filter(item == "face", Order=="NGFirst")))
summary(lmer(rating ~ condition + (1|Participant), h2 %>% filter(item == "face", Order=="GAFirst")))
# here the two groups seem to have an opposite pattern
# but still no significant effect

# Item 6 (Voice)
ggplot(h2 %>% filter(item == "voice"), aes(condition, rating))+
  geom_boxplot()+
  facet_wrap(~Order)
summary(lmer(rating ~ condition + (1|Participant), h2 %>% filter(item == "voice", Order=="NGFirst")))
summary(lmer(rating ~ condition + (1|Participant), h2 %>% filter(item == "voice", Order=="GAFirst")))
# still same pattern for both groups

# Item 7 (Behavior)
ggplot(h2 %>% filter(item == "behavior"), aes(condition, rating))+
  geom_boxplot()+
  facet_wrap(~Order)
summary(lmer(rating ~ condition + (1|Participant), h2 %>% filter(item == "behavior", Order=="NGFirst")))
summary(lmer(rating ~ condition + (1|Participant), h2 %>% filter(item == "behavior", Order=="GAFirst")))
# no difference between conditions for NG-first
# behavior significantly less humanlike during GA for GA-first group

# Item 8 (Feel like talking to a human)
ggplot(h2 %>% filter(item == "feel"), aes(condition, rating))+
  geom_boxplot()+
  facet_wrap(~Order)
summary(lmer(rating ~ condition + (1|Participant), h2 %>% filter(item == "feel", Order=="NGFirst")))
summary(lmer(rating ~ condition + (1|Participant), h2 %>% filter(item == "feel", Order=="GAFirst")))
# no difference between conditions for either group
# but GA-first group seems to have higher ratings here! let's check

ggplot(h2 %>% filter(item == "feel"), aes(Order, rating))+
  geom_boxplot()
summary(lm(rating ~ Order, h2 %>% filter(item == "feel")))
# the plot shows what should be a clear trend of higher ratings for GA-first participants
#  but the regression really doesn't show anything

# and now overall (not the average rating, but all the ratings taken separately but in the same dataset)
ggplot(h2, aes(Order, rating))+
  geom_boxplot()
summary(lm(rating ~ Order, h2))

ggplot(h2, aes(condition, rating))+
  geom_boxplot()
summary(lmer(rating ~ condition + (1 | Participant), h2))
# like seen above, a trend for lower ratings in GA condition, but not significant