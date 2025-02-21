# library(multcomp)
# library(readr)
# library(dplyr)
# library(tidyverse)
# library(readxl)
# library(ggplot2)
# library(reshape2)
# library(writexl)
# library(emmeans)
# library(MASS)
# library(car)
# library(lattice)
# library(lmerTest)
# library(effects)
# library(MuMIn)
# library(RVAideMemoire)
# library(tidyverse)
# library(dplyr)
# library(lme4)
# library(nlme)

dat <- read_excel("Credibility_ALL.xlsx")
summary(dat)

dat$speaker_language <- as.factor(dat$speaker_language)
dat$speaker_strength <- as.factor(dat$speaker_strength)
dat$speaker_sex <- as.factor(dat$speaker_sex)
dat$statement<- as.factor(dat$statement)
dat$truthfulness <- as.factor(dat$truthfulness)
dat$list <- as.factor(dat$list)
dat$rating <- as.numeric(dat$rating)
dat$participant <- as.factor(dat$participant)
dat$participant_group <- as.factor(dat$participant_group)
dat$participant_sex <- as.factor(dat$participant_sex)
dat$participant_age <- as.factor(dat$participant_age )
dat$participant_L1 <- as.factor(dat$participant_L1)
dat$L1_age <- as.factor(dat$L1_age)
dat$L1_fluency <- as.factor(dat$L1_fluency)
dat$participant_L2 <- as.factor(dat$participant_L2)
dat$L2_age <- as.factor(dat$L2_age)
dat$L2_fluency <- as.factor(dat$L2_fluency)
dat$participant_L3 <- as.factor(dat$participant_L3)
dat$L3_age <- as.factor(dat$L3_age)
dat$L3_fluency <- as.factor(dat$L3_fluency)
dat$participant_L4 <- as.factor(dat$participant_L4)
dat$L4_age <- as.factor(dat$L4_age)
dat$L4_fluency <- as.factor(dat$L4_fluency)
dat$`Exposure to other languages` <- as.factor(dat$`Exposure to other languages`)


dat$Language <- factor(paste(dat$speaker_strength, dat$speaker_language),
                                levels = c("Native English", "Heavy French", "Mild French", 
                                           "Heavy Italian", "Mild Italian", 
                                           "Heavy Spanish", "Mild Spanish"))



mean_dat <- dat %>%
  group_by(Language, speaker_language, speaker_strength, speaker_sex, list, participant, participant_age, participant_sex, participant_group, participant_L1, L1_age, L1_fluency, participant_L2, L2_age, L2_fluency,
           participant_L3, L3_age, L3_fluency, participant_L4, L4_age, L4_fluency, `Exposure to other languages`) %>%
  summarise(mean_rating = mean(rating, na.rm = TRUE), .groups = "drop")

mean_dat$speaker_strength <- factor(mean_dat$speaker_strength, levels = c("Native", "Mild", "Heavy"))

mono_data <- dat %>%
  filter(participant_group == "Monolingual") %>%
  group_by(Language, speaker_language, speaker_strength, speaker_sex, list, participant, participant_age, participant_sex, participant_group, participant_L1, L1_age, L1_fluency, participant_L2, L2_age, L2_fluency,
           participant_L3, L3_age, L3_fluency, participant_L4, L4_age, L4_fluency, `Exposure to other languages`) %>%
  summarise(mean_rating = mean(rating, na.rm = TRUE), .groups = "drop")

mono_data$speaker_strength <- factor(mono_data$speaker_strength, levels = c("Native", "Mild", "Heavy"))


bi_data <- dat %>%
  filter(participant_group == "Bilingual") %>%
  group_by(Language, speaker_language, speaker_strength, speaker_sex, list, participant, participant_sex, participant_age, participant_group, participant_L1, L1_age, L1_fluency, participant_L2, L2_age, L2_fluency,
           participant_L3, L3_age, L3_fluency, participant_L4, L4_age, L4_fluency, `Exposure to other languages`) %>%
  summarise(mean_rating = mean(rating, na.rm = TRUE), .groups = "drop")

bi_data$speaker_strength <- factor(bi_data$speaker_strength, levels = c("Native", "Mild", "Heavy"))

###################### MONOLINGUAL STUDY ############################## 

mboxcox_lang <- boxcox(mean_rating ~ Language, data = mono_data) 
mboxcox_lang $x[which.max(mboxcox_lang$y)] #1.88

mboxcox_lang2 <- boxcox(mean_rating ~ Language * participant_sex + participant_age, data = mono_data)
mboxcox_lang2$x[which.max(mboxcox_lang2$y)] #1.68


mlme_lang <- lmer((mean_rating)^2 ~ Language + (1 | list), data = mono_data)
mlme_lang2 <- lmer((mean_rating)^2 ~ Language * participant_sex + participant_age + (1 | list), data = mono_data)



#Lower is better for each!
AIC(mlme_lang)
AIC(mlme_lang2) #better
BIC(mlme_lang) #better
BIC(mlme_lang2)
summary(mlme_lang) #- LMER for anova
summary(mlme_lang2) #Better

maov_lang <- aov(mean_rating ~ Language, data = mono_data)
summary(maov_lang) #ANOVA
#              Df Sum Sq Mean Sq F value Pr(>F)
# Language      6  0.278 0.04637    0.39  0.884
# Residuals   147 17.467 0.11883   


mph_lang <- glht(maov_lang, linfct = mcp(Language = "Tukey"))

ggplot(mono_data, aes(x = Language, y = mean_rating, fill = factor(Language))) +
  stat_summary(fun = "mean", geom = "bar") +
  stat_summary(fun = "mean", geom = "text", aes(label = round(..y.., 2)), vjust = -0.5, color = "black", size = 5) +
  labs(title = "Credibility Ratings by Language",
       x = "Language",
       y = "Credibility Rating",
       fill = "Language") +
  theme(axis.text.x = element_blank(), axis.text.y = element_text(size=15)) +
  scale_fill_manual(values = c("yellow", "red4", "red", "dodgerblue4", "deepskyblue", "green4", "limegreen")) + 
  scale_y_continuous(limits= c(0,4), breaks = 1:4,  # Specify the y-axis positions
                     labels = c("1: I am certain this is false", "2: I think this is false", "3: I think this is right", "4: I am certain this is right"))



################### Base Language ########################

maov_base <- aov(mean_rating ~ speaker_language, data = mono_data)
mph_base <- glht(maov_base, linfct = mcp(speaker_language= "Tukey"))


mboxcox_base <- boxcox(mean_rating ~ speaker_language, data = mono_data) 
mboxcox_base $x[which.max(mboxcox_base$y)] #1.84

mboxcox_base2 <- boxcox(mean_rating ~ speaker_language * participant_sex + participant_age, data = mono_data) 
mboxcox_base2$x[which.max(mboxcox_base2$y)] # = 1.68

mlme_base <- lmer((mean_rating)^2 ~ speaker_language + (1 | list), data = mono_data)
mlme_base2 <- lmer((mean_rating)^2 ~ speaker_language* participant_sex + participant_age + (1 | list), data = mono_data)


AIC(mlme_base)
AIC(mlme_base2) #

BIC(mlme_base) #
BIC(mlme_base2)

summary(maov_base) #ANOVA

# Df Sum Sq Mean Sq F value Pr(>F)
# speaker_language   3  0.227 0.07577   0.649  0.585
# Residuals        150 17.518 0.11679  

summary(mph_base ) #POST HOC
summary(mlme_base) #REML 584.9
summary(mlme_base2) #REML2 - better 520.8


ggplot(mono_data, aes(x = speaker_language, y = mean_rating, fill = factor(speaker_language))) +
  stat_summary(fun = "mean", geom = "bar") +
  stat_summary(fun = "mean", geom = "text", aes(label = round(..y.., 2)), vjust = -0.5, color = "black", size = 5) +
  labs(title = "Credibility Ratings by Language",
       x = "Language",
       y = "Credibility Rating",
       fill = "Language") +
  theme(axis.text.x = element_blank(), axis.text.y = element_text(size=15)) +
  scale_fill_manual(values = c("yellow", "red4",  "dodgerblue4", "green4", "limegreen")) + 
  scale_y_continuous(limits= c(0,4), breaks = 1:4,  # Specify the y-axis positions
                     labels = c("1: I am certain this is false", "2: I think this is false", "3: I think this is right", "4: I am certain this is right"))




############################################# Accent Strength ########################################################### 

maov_acc <- aov(mean_rating ~ speaker_strength, data = mono_data)
mph_acc <- glht(maov_acc , linfct = mcp(speaker_strength= "Tukey"))

mboxcox_acc <- boxcox(mean_rating ~ speaker_strength, data = mono_data) 
mboxcox_acc $x[which.max(mboxcox_acc$y)]  #1.88

mboxcox_acc2 <- boxcox(mean_rating ~ speaker_strength * participant_sex + participant_age, data = mono_data) 
mboxcox_acc2$x[which.max(mboxcox_acc2$y)] # = 1.72

mlme_acc <- lmer((mean_rating)^2 ~ speaker_strength + (1 | list), data = mono_data)
mlme_acc2 <- lmer((mean_rating)^2 ~ speaker_strength * participant_sex + participant_age + (1 | list), data = mono_data)


#Lower is better for each!
AIC(mlme_acc) 
AIC(mlme_acc2) #

BIC(mlme_acc) #
BIC(mlme_acc2)

summary(maov_acc) #ANOVA
# Df Sum Sq Mean Sq F value Pr(>F)
# speaker_strength   2  0.098 0.04899   0.419  0.658
# Residuals        151 17.648 0.11687 


summary(mph_acc) #POST HOC
summary(mlme_acc) #REML 
summary(mlme_acc2) #REML2 - better



ggplot(mono_data, aes(x = speaker_strength, y = mean_rating, fill = factor(speaker_strength))) +
  stat_summary(fun = "mean", geom = "bar") +
  stat_summary(fun = "mean", geom = "text", aes(label = round(..y.., 2)), vjust = -0.5, color = "black", size = 5) +
  labs(title = "Credibility Ratings by Language",
       x = "Language",
       y = "Credibility Rating",
       fill = "Language") +
  theme(axis.text.x = element_blank(), axis.text.y = element_text(size=15)) +
  scale_fill_manual(values = c("yellow", "limegreen", "red4", "dodgerblue4", "deepskyblue", "green4", "limegreen")) + 
  scale_y_continuous(limits= c(0,4), breaks = 1:4,  # Specify the y-axis positions
                     labels = c("1: I am certain this is false", "2: I think this is false", "3: I think this is right", "4: I am certain this is right")) 


######################### participant_sex ##################

maov_gen <- aov(mean_rating ~ Language * participant_sex, data = mono_data)
memm_gen <- emmeans(maov_gen, ~ Language * participant_sex, data = mono_data)
mpairwise_gen <- pairs(memm_gen, adjust = "tukey")


mboxcox_gen <- boxcox(mean_rating ~ Language * participant_sex, data = mono_data) 
mboxcox_gen $x[which.max(mboxcox_gen$y)] #1.84

mboxcox_gen2 <- boxcox(mean_rating ~ Language* participant_sex + participant_age, data = mono_data) 
mboxcox_gen2$x[which.max(mboxcox_gen2$y)] # 1.68

mlme_gen <- lmer((mean_rating)^2 ~ Language * participant_sex + (1 | list), data = mono_data)
mlme_gen2 <- lmer((mean_rating)^2 ~ Language* participant_sex + participant_age + (1 | list), data = mono_data)

#Lower is better for each!
AIC(mlme_gen)
AIC(mlme_gen2) #
BIC(mlme_gen) #
BIC(mlme_gen2)

summary(maov_gen) #ANOVA
#                            Df Sum Sq Mean Sq F value Pr(>F)
# Language                   6  0.278 0.04637   0.396  0.881
# participant_sex            1  0.232 0.23228   1.982  0.161
# Language:participant_sex   6  0.829 0.13824   1.180  0.321
# Residuals                140 16.406 0.11718 

summary(mpairwise_gen) #PAIR EMMEANS
summary(mlme_gen) #REML 566.1
summary(mlme_gen2) #REML2 - better 520.8


ggplot(mono_data, aes(x = Language, y = mean_rating, fill = factor(Language))) +
  stat_summary(fun = "mean", geom = "bar")  + 
  stat_summary(fun = "mean", geom = "text", aes(label = round(..y.., 2)), position = position_stack(vjust = 0.5), color = "black", size = 5) +
  
  labs(title = "Credibility Ratings of Each Language by Gender",
       x = "Language",
       y = "Credibility Rating",
       fill = "Language") +
  theme(axis.text.x = element_blank(), axis.text.y = element_text(size=15)) +
  scale_fill_manual(values = c("yellow", "red4", "red", "dodgerblue4", "deepskyblue", "green4", "limegreen")) + 
  facet_grid(~participant_sex) + 
  scale_y_continuous(limits= c(0,4), breaks = 1:4,  # Specify the y-axis positions
                     labels = c("1: I am certain this is false", "2: I think this is false", "3: I think this is right", "4: I am certain this is right"))








########### participant age ########################


maov_age <- aov(mean_rating ~ Language * participant_age, data = mono_data)
library(emmeans)
memm_age <- emmeans(maov_age, ~ Language * participant_age)
mpairwise_age <- pairs(memm_age, adjust = "tukey")


mboxcox_age <- boxcox(mean_rating ~ Language * participant_age, data = mono_data) 
mboxcox_age $x[which.max(mboxcox_age$y)]

mboxcox_age2 <- boxcox(mean_rating ~ Language* participant_age + participant_sex, data = mono_data) 
mboxcox_age2$x[which.max(mboxcox_age2$y)] # = 0.3

mlme_age <- lmer(mean_rating ~ Language * participant_age + (1 | list), data = mono_data)
mlme_age2 <- lmer(mean_rating ~ Language* participant_age + participant_sex + (1 | list), data = mono_data)



#Lower is better for each!
AIC(mlme_age) # 1 better
AIC(mlme_age2) 
BIC(mlme_age) # 1 better
BIC(mlme_age2)

summary(maov_age) #ANOVA
#                           Df Sum Sq Mean Sq F value   Pr(>F)    
# Language                  6  0.278  0.0464   0.537 0.777451    
# participant_age          13  4.287  0.3298   3.822 0.000215 ***
#   Language:participant_age 78  8.350  0.1070   1.241 0.198073    
# Residuals                56  4.831  0.0863     


summary(mpairwise_age) #PAIR EMMEANS
summary(mlme_age) #REML 1 - Better?
summary(mlme_age2) #REML2



ggplot(mono_data, aes(x= Language, y = mean_rating, fill = factor(Language))) + 
  stat_summary(fun = "mean", geom = "bar")  + 
  
  labs(title = "Credibility Ratings of Each Language by Age",
       x = "Language",
       y = "Credibility Rating",
       fill = "Language") +
  theme(axis.text.x = element_blank(), axis.text.y = element_text(size=15)) +
  scale_fill_manual(values = c("yellow", "red4", "red", "dodgerblue4", "deepskyblue", "green4", "limegreen")) +
  facet_grid(~participant_age) + 
  scale_y_continuous(limits= c(0,4), breaks = 1:4,  # Specify the y-axis positions
                     labels = c("1: I am certain this is false", "2: I think this is false", "3: I think this is right", "4: I am certain this is right"))


############################ Gender on Gender  ####################




maov_gen_gen <- aov(mean_rating ~ Language * participant_sex * speaker_sex, data = mono_data)
memm_gen_gen <- emmeans(maov_gen_gen, ~ Language * participant_sex * speaker_sex, data = mono_data)
mpairwise_gen_gen <- pairs(memm_gen_gen, adjust = "tukey")


mboxcox_gen_gen <- boxcox(mean_rating ~ Language * participant_sex * speaker_sex, data = mono_data) 
mboxcox_gen_gen $x[which.max(mboxcox_gen_gen$y)] #1.84

mboxcox_gen2_gen <- boxcox(mean_rating ~ Language* participant_sex + participant_age, data = mono_data) 
mboxcox_gen2_gen$x[which.max(mboxcox_gen2_gen$y)] # 1.68

mlme_gen_gen <- lmer((mean_rating)^2 ~ Language * participant_sex + (1 | list), data = mono_data)
mlme_gen2_gen <- lmer((mean_rating)^2 ~ Language* participant_sex + participant_age + speaker_sex + (1 | list), data = mono_data)

#Lower is better for each!
AIC(mlme_gen_gen)
AIC(mlme_gen2_gen) #
BIC(mlme_gen_gen) #
BIC(mlme_gen2_gen)

summary(maov_gen_gen) #ANOVA
#                               Df Sum Sq Mean Sq F value Pr(>F)
# Language                      6  0.278 0.04637   0.390  0.884
# participant_sex               1  0.232 0.23228   1.955  0.164
# speaker_sex                   1  0.011 0.01120   0.094  0.759
# Language:participant_sex      6  0.819 0.13643   1.148  0.338
# participant_sex:speaker_sex   1  0.005 0.00532   0.045  0.833
# Residuals                   138 16.400 0.11884

summary(mpairwise_gen_gen) #PAIR EMMEANS
summary(mlme_gen_gen) #REML 566.1
summary(mlme_gen2_gen) #REML2 - better 520.8


ggplot(mono_data, aes(x = speaker_sex, y = mean_rating, fill = factor(speaker_sex))) +
  stat_summary(fun = "mean", geom = "bar")  + 
  stat_summary(fun = "mean", geom = "text", aes(label = round(..y.., 2)), position = position_stack(vjust = 0.5), color = "black", size = 5) +
  
  labs(title = "Credibility Ratings of Each Language by Gender",
       x = "Speaker Gender",
       y = "Credibility Rating",
       fill = "Speaker Gender") +
  theme(axis.text.x = element_blank(), axis.text.y = element_text(size=15)) +
  scale_fill_manual(values = c("red4", "dodgerblue4")) + 
  facet_grid(~participant_sex) + 
  scale_y_continuous(limits= c(0,4), breaks = 1:4,  # Specify the y-axis positions
                     labels = c("1: I am certain this is false", "2: I think this is false", "3: I think this is right", "4: I am certain this is right"))



####################### BILINGUAL STUDY  #########################
################# The 7 Languages #########################
baov_lang <- aov(mean_rating ~ Language, data = bi_data)
summary(baov_lang) 
#             Df Sum Sq Mean Sq F value Pr(>F)
# Language      6  0.718  0.1197   0.677  0.668
# Residuals   147 25.968  0.1767

bph_lang <- glht(baov_lang, linfct = mcp(Language = "Tukey"))
summary(bph_lang) #POST HOC - no significance 


bboxcox_lang <- boxcox(mean_rating ~ Language, data = bi_data) 
bboxcox_lang $x[which.max(bboxcox_lang$y)] #0.7

bboxcox_lang2 <- boxcox(mean_rating ~ Language * participant_sex + participant_age, data = bi_data) 
bboxcox_lang2$x[which.max(bboxcox_lang2$y)] # = 0.9

blme_lang <- lmer(mean_rating ~ Language + (1 | list), data = bi_data)
blme_lang2 <- lmer(mean_rating ~ Language * participant_sex + participant_age + (1 | list), data = bi_data)


#Lower is better for each!
AIC(blme_lang) #
AIC(blme_lang2)

BIC(blme_lang) #
BIC(blme_lang2)

summary(blme_lang) #REML - better 
summary(blme_lang2) # 

# REML convergence : 184
# LanguageMild French Accent    -0.24545    0.12673 147.00000  -1.937   0.0547 .  



ggplot(bi_data, aes(x = Language, y = mean_rating, fill = factor(Language))) +
  stat_summary(fun = "mean", geom = "bar")  + 
  stat_summary(fun = "mean", geom = "text", aes(label = round(..y.., 2)), position = position_stack(vjust = 0.5), color = "black", size = 5) +
  
  geom_text(data = bi_lang, aes(label = round(mean_rating, 2)), 
            position = position_stack(vjust = 0.5), color = "black", size = 5) +
  labs(title = "Credibility Ratings by Language",
       x = "Language",
       y = "Credibility Rating",
       fill = "Language") +
  theme(axis.text.x = element_blank(), axis.text.y = element_text(size=15)) +
  scale_fill_manual(values = c("yellow", "red4", "red", "dodgerblue4", "deepskyblue", "green4", "limegreen")) + 
  scale_y_continuous(limits= c(0,4), breaks = 1:4,  # Specify the y-axis positions
                     labels = c("1: I am certain this is false", "2: I think this is false", "3: I think this is right", "4: I am certain this is right"))


################# Accent Strength #########################

baov_acc <- aov(mean_rating ~ speaker_strength, data = bi_data)
summary(baov_acc)
#                 Df Sum Sq Mean Sq F value Pr(>F)
# speaker_strength   2  0.374  0.1868   1.072  0.345
# Residuals      151 26.312  0.1742

bph_acc <- glht(baov_acc , linfct = mcp(speaker_strength= "Tukey"))
summary(bph_acc) # no significance

bboxcox_acc <- boxcox(mean_rating ~ speaker_strength, data = bi_data) 
bboxcox_acc $x[which.max(bboxcox_acc$y)] #0.7

bboxcox_acc2 <- boxcox(mean_rating ~ speaker_strength * participant_sex + participant_age, data = bi_data) 
bboxcox_acc2$x[which.max(bboxcox_acc2$y)] # = 0.9

blme_acc <- lmer(mean_rating ~ speaker_strength+ (1 | list), data = bi_data)
blme_acc2 <- lmer(mean_rating ~ speaker_strength * participant_sex + participant_age + (1 | list), data = bi_data)


#Lower is better for each!
AIC(blme_acc) #
AIC(blme_acc2)

BIC(blme_acc) #
BIC(blme_acc2)

summary(blme_acc) #REML - no significance 

summary(blme_acc2) # significance 
# speaker_strengthNative          0.21701    0.11417 134.00000   1.901   0.0595
# Age36                           -0.41197    0.15934 134.00000  -2.585   0.0108 *  


bi_accent <- bi_data %>%
  group_by(speaker_strength) %>%
  summarize(mean_rating = mean(mean_rating, na.rm = TRUE))

print(bi_accent)

ggplot(bi_data, aes(x = speaker_strength, y = mean_rating, fill = factor(speaker_strength))) +
  stat_summary(fun = "mean", geom = "bar")  + 
  stat_summary(fun = "mean", geom = "text", aes(label = round(..y.., 2)), position = position_stack(vjust = 0.5), color = "black", size = 5) +
  
  labs(title = "Credibility Ratings by Accent",
       x = "Language",
       y = "Credibility Rating",
       fill = "Language") +
  theme(axis.text.x = element_blank(), axis.text.y = element_text(size=15)) +
  scale_fill_manual(values = c("yellow", "green4", "red4", "dodgerblue4", "deepskyblue", "green4", "limegreen")) + 
  scale_y_continuous(limits= c(0,4), breaks = 1:4,  # Specify the y-axis positions
                     labels = c("1: I am certain this is false", "2: I think this is false", "3: I think this is right", "4: I am certain this is right"))


################# Base Language  #########################

baov_base <- aov(mean_rating ~ speaker_language, data = bi_data)
summary(baov_base) 
#               Df Sum Sq Mean Sq F value Pr(>F)
# speaker_language   3  0.519  0.1729   0.991  0.399
# Residuals    150 26.167  0.1744   

bph_base <- glht(baov_base, linfct = mcp(speaker_language= "Tukey"))
summary(bph_base ) #POST HOC - no significance


bboxcox_base <- boxcox(mean_rating ~ speaker_language, data = bi_data) 
bboxcox_base $x[which.max(bboxcox_base$y)] #0.7

bboxcox_base2 <- boxcox(mean_rating ~ speaker_language* participant_sex + participant_age, data = bi_data) 
bboxcox_base2$x[which.max(bboxcox_base2$y)] # = 0.9

blme_base <- lmer(mean_rating ~ speaker_language+ (1 | list), data = bi_data)
blme_base2 <- lmer(mean_rating ~ speaker_language* participant_sex + participant_age + (1 | list), data = bi_data)

#Lower is better for each!
AIC(blme_base) #
AIC(blme_base2)
BIC(blme_base) #
BIC(blme_base2)

summary(blme_base) #REML #Better
# BaseLanguageFrench   -0.18523    0.10906 150.00000  -1.698   0.0915 .  


summary(blme_base2) #REML2
# BaseLanguageFrench              -0.23177    0.12156 132.00000  -1.907   0.0587 .  
# BaseLanguageSpanish             -0.20625    0.12156 132.00000  -1.697   0.0921 .  
# Age36                           -0.41197    0.15997 132.00000  -2.575   0.0111 *  
#   



ggplot(bi_data, aes(x = speaker_language, y = mean_rating, fill = factor(speaker_language))) +
  stat_summary(fun = "mean", geom = "bar")  + 
  stat_summary(fun = "mean", geom = "text", aes(label = round(..y.., 2)), position = position_stack(vjust = 0.5), color = "black", size = 5) +
  
  labs(title = "Credibility Ratings by Language",
       x = "Language",
       y = "Credibility Rating",
       fill = "Language") +
  theme(axis.text.x = element_blank(), axis.text.y = element_text(size=15)) +
  scale_fill_manual(values = c("yellow", "red4", "dodgerblue4", "green4", "limegreen")) + 
  scale_y_continuous(limits= c(0,4), breaks = 1:4,  # Specify the y-axis positions
                     labels = c("1: I am certain this is false", "2: I think this is false", "3: I think this is right", "4: I am certain this is right"))
###################################################   participant_sex ########################################################### 

baov_gen <- aov(mean_rating ~ Language * participant_sex, data = bi_data)
summary(baov_gen) 
#                   Df Sum Sq Mean Sq F value Pr(>F)
# Language          6  0.718 0.11966   0.662  0.680
# participant_sex            1  0.011 0.01058   0.059  0.809
# Language:participant_sex   6  0.665 0.11080   0.613  0.719
# Residuals       140 25.293 0.18066       

bemm_gen <- emmeans(baov_gen, ~ Language * participant_sex)
bpairwise_gen <- pairs(bemm_gen, adjust = "tukey")
summary(bpairwise_gen) #PAIR EMMEANS - no significance



bboxcox_gen <- boxcox(mean_rating ~ Language * participant_sex, data = bi_data) 
bboxcox_gen $x[which.max(bboxcox_gen$y)] #0,7

bboxcox_gen2 <- boxcox(mean_rating ~ Language* participant_sex + participant_age, data = bi_data) 
bboxcox_gen2$x[which.max(bboxcox_gen2$y)] # = 0.9

blme_gen <- lmer(mean_rating ~ Language+ (1 | list), data = bi_data)
blme_gen2 <- lmer(mean_rating ~ Language* participant_sex + participant_age + (1 | list), data = bi_data)



#Lower is better for each!
AIC(blme_gen) #
AIC(blme_gen2) 
BIC(blme_gen) #
BIC(blme_gen2)

summary(blme_gen) #REML - better
# LanguageMild French Accent    -0.24545    0.12673 147.00000  -1.937   0.0547 .  

summary(blme_gen2) #REML2 - significances
# LanguageMild French Accent               -0.28229    0.14179 126.00000  -1.991   0.0487 *  
# LanguageHeavy Italian Accent             -0.26354    0.14179 126.00000  -1.859   0.0654 .  
# Age36                                    -0.41197    0.16159 126.00000  -2.549   0.0120 *  
# LanguageHeavy Italian Accent:GenderMale   0.48854    0.27151 126.00000   1.799   0.0744 .  


ggplot(bi_data, aes(x = Language, y = mean_rating, fill = factor(Language))) +
  stat_summary(fun = "mean", geom = "bar")  + 
  stat_summary(fun = "mean", geom = "text", aes(label = round(..y.., 2)), position = position_stack(vjust = 0.5), color = "black", size = 5) +
  
  labs(title = "Credibility Ratings of Each Language by participant_sex",
       x = "Language",
       y = "Credibility Rating",
       fill = "Language") +
  theme(axis.text.x = element_blank(), axis.text.y = element_text(size=15)) +
  scale_fill_manual(values = c("yellow", "red4", "red", "dodgerblue4", "deepskyblue", "green4", "limegreen")) + 
  facet_grid(~participant_sex) + 
  scale_y_continuous(limits= c(0,4), breaks = 1:4,  # Specify the y-axis positions
                     labels = c("1: I am certain this is false", "2: I think this is false", "3: I think this is right", "4: I am certain this is right"))




###################################################   participant_age  ########################################################### 

baov_age <- aov(mean_rating ~ Language * participant_age, data = bi_data)
summary(baov_age) 
#               Df Sum Sq Mean Sq F value Pr(>F)  
# Language      6  0.718  0.1197   0.667 0.6762  
# participant_age          14  4.993  0.3566   1.989 0.0389 *
#   Language:participant_age 84 12.191  0.1451   0.810 0.8043  
# Residuals    49  8.784  0.1793    

bemm_age <- emmeans(baov_age, ~ Language * participant_age)
bpairwise_age <- pairs(bemm_age, adjust = "tukey")
summary(bpairwise_age) #PAIR EMMEANS - no significance

bboxcox_age <- boxcox(mean_rating ~ Language * participant_age, data = bi_data) 
bboxcox_age $x[which.max(bboxcox_age$y)] #2

bboxcox_age2 <- boxcox(mean_rating ~ Language* participant_age + participant_sex, data = bi_data) 
bboxcox_age2$x[which.max(bboxcox_age2$y)] #2 

blme_age <- lmer((mean_rating)^2 ~ Language * participant_age + (1 | list), data = bi_data)
blme_age2 <- lmer((mean_rating)^2 ~ Language* participant_age + participant_sex + (1 | list), data = bi_data)



#Lower is better for each!
AIC(blme_age) #
AIC(blme_age2) 
BIC(blme_age) #
BIC(blme_age2)

summary(blme_age) #REML - Better model 236.5
                                          # Estimate Std.    t value Pr(>|t|)    
# LanguageHeavy Italian:participant_age27  -6.6474     3.4484   -1.928   0.0597 .  
# LanguageHeavy Italian:participant_age33  -7.3874     3.4484   -2.142   0.0372 *  
#   LanguageHeavy Spanish:participant_age33  -6.3521     3.4484   -1.842   0.0715 .  


summary(blme_age2) #REML2 - 235.4



ggplot(bi_data, aes(x = Language, y = mean_rating, fill = factor(Language))) +
  stat_summary(fun = "mean", geom = "bar")  + 

  labs(title = "Credibility Ratings of Each Language by participant_sex",
       x = "Language",
       y = "Credibility Rating",
       fill = "Language") +
  theme(axis.text.x = element_blank(), axis.text.y = element_text(size=15)) +
  scale_fill_manual(values = c("yellow", "red4", "red", "dodgerblue4", "deepskyblue", "green4", "limegreen")) + 
  facet_grid(~participant_age) + 
  scale_y_continuous(limits= c(0,4), breaks = 1:4,  # Specify the y-axis positions
                     labels = c("1: I am certain this is false", "2: I think this is false", "3: I think this is right", "4: I am certain this is right"))


View(bi_data)

###################################################   Distribution ##############################################

ggplot(dat, aes(x = Language, y = rating, fill = factor(Language))) +
  geom_violin(trim = FALSE) +
  labs(title = "Density of Difficulty Ratings of Each Language",
       x = "Language",
       y = "Difficulty Rating",
       fill = "Language") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("yellow", "red4", "red", "dodgerblue4", "deepskyblue", "green4", "limegreen"))


ggplot(merged_data, aes(x = Language, y = rating, fill = factor(Language))) +
  geom_violin(trim = FALSE) +
  labs(title = "Density of Difficulty Ratings of Each Language By participant_age",
       x = "Language",
       y = "Difficulty Rating",
       fill = "Language") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("yellow", "red4", "red", "dodgerblue4", "deepskyblue", "green4", "limegreen")) +
  facet_grid(~participant_sex)


########## Gender on Gender ###################3 


baov_gen_gen <- aov(mean_rating ~ Language * participant_sex * speaker_sex, data = bi_data)
bemm_gen_gen <- emmeans(baov_gen_gen, ~ Language * participant_sex * speaker_sex, data = bi_data)
bpairwise_gen_gen <- pairs(bemm_gen_gen, adjust = "tukey")


bboxcox_gen_gen <- boxcox(mean_rating ~ Language * participant_sex * speaker_sex, data = bi_data) 
bboxcox_gen_gen $x[which.max(bboxcox_gen_gen$y)] #0.79

bboxcox_gen2_gen <- boxcox(mean_rating ~ Language* participant_sex + participant_age, data = bi_data) 
bboxcox_gen2_gen$x[which.max(bboxcox_gen2_gen$y)] # 0.95

blme_gen_gen <- lmer((mean_rating)^2 ~ Language * participant_sex + (1 | list), data = bi_data)
blme_gen2_gen <- lmer((mean_rating)^2 ~ Language* participant_sex + participant_age + speaker_sex + (1 | list), data = bi_data)

#Lower is better for each!
AIC(blme_gen_gen)
AIC(blme_gen2_gen) #
BIC(blme_gen_gen) #
BIC(blme_gen2_gen)

summary(baov_gen_gen) #ANOVA
#                               Df Sum Sq Mean Sq F value Pr(>F)
# Language                      6  0.718 0.11966   0.662  0.680
# participant_sex               1  0.011 0.01058   0.059  0.809
# speaker_sex                   1  0.312 0.31239   1.729  0.191
# Language:participant_sex      6  0.695 0.11578   0.641  0.697
# participant_sex:speaker_sex   1  0.024 0.02410   0.133  0.715
# Residuals                   138 24.926 0.18063 

summary(bpairwise_gen_gen) #PAIR EMMEANS
summary(blme_gen_gen) #REML 636.4

summary(blme_gen2_gen) #REML2 - better 583.7
# LanguageMild French                        -1.44769    0.69215 125.00000  -2.092  0.03850 *  
# participant_age30                          -1.75734    0.94848 125.00000  -1.853  0.06627 .  
# participant_age36                          -2.08841    0.78880 125.00000  -2.648  0.00915 ** 
# LanguageHeavy Italian:participant_sexMale   2.46274    1.32537 125.00000   1.858  0.06550 .  


ggplot(bi_data, aes(x = speaker_sex, y = mean_rating, fill = factor(speaker_sex))) +
  stat_summary(fun = "mean", geom = "bar")  + 
  stat_summary(fun = "mean", geom = "text", aes(label = round(..y.., 2)), position = position_stack(vjust = 0.5), color = "black", size = 5) +
  
  labs(title = "Credibility Ratings of Each Language by Gender",
       x = "Speaker Gender",
       y = "Credibility Rating",
       fill = "Speaker Gender") +
  theme(axis.text.x = element_blank(), axis.text.y = element_text(size=15)) +
  scale_fill_manual(values = c("red4", "dodgerblue4")) + 
  facet_grid(~participant_sex) + 
  scale_y_continuous(limits= c(0,4), breaks = 1:4,  # Specify the y-axis positions
                     labels = c("1: I am certain this is false", "2: I think this is false", "3: I think this is right", "4: I am certain this is right"))




######################################### MULTILINGUAL STUDY ###############################################
################# The 7 Languages #########################
mixed_aov_lang <- aov(mean_rating ~ Language * participant_group, data = mean_dat)
summary(mixed_aov_lang) 
#                     Df Sum Sq Mean Sq F value Pr(>F)
# Language           6   0.48 0.07944   0.538  0.779
# participant_group            1   0.02 0.01912   0.129  0.719
# Language:participant_group   6   0.52 0.08659   0.586  0.741
# Residuals        294  43.44 0.14774 

mixed_emmeans_lang <- emmeans(mixed_aov_lang, ~ Language * participant_group)
pairs(mixed_emmeans_lang, adjust = "tukey") # No sig



mixed_boxcox_lang <- boxcox(mean_rating ~ Language * participant_group, data = mean_dat) 
mixed_boxcox_lang $x[which.max(mixed_boxcox_lang$y)] # = 1.2

mixed_boxcox_lang2 <- boxcox(mean_rating ~ Language * participant_group + participant_sex + participant_age, data = mean_dat) 
mixed_boxcox_lang2$x[which.max(mixed_boxcox_lang2$y)] # = 1.19

mixed_lme_lang <- lmer(mean_rating ~ Language  * participant_group + (1 | list), data = mean_dat)
mixed_lme_lang2 <- lmer(mean_rating ~ Language * participant_group + participant_sex + participant_age + (1 | list), data = mean_dat)


#Lower is better for each!
AIC(mixed_lme_lang) #
AIC(mixed_lme_lang2)

BIC(mixed_lme_lang) #
BIC(mixed_lme_lang2)

summary(mixed_lme_lang) #REML  315.4 - BETTER 
# LanguageMild French                                 -0.245454   0.115892   -2.118    0.035 *  
  
summary(mixed_lme_lang2) #REML2 319.1

# Estimate Std. Error         df t value Pr(>|t|)    
# Mono_BiMono                                  0.219789   0.121895 263.330686   1.803  0.07252 .  
# Age21                                       -0.378693   0.195269 172.118437  -1.939  0.05409 .  
# Age32                                       -0.436988   0.163725 152.703175  -2.669  0.00843 ** 
# Age36                                       -0.247455   0.132541 259.197814  -1.867  0.06303 .  
# Age37                                       -0.461072   0.156651 267.383073  -2.943  0.00353 ** 
# Age41                                       -0.638994   0.199546 161.899643  -3.202  0.00164 ** 
# Age46                                       -0.566701   0.194412 193.937638  -2.915  0.00398 ** 
# Age51                                        0.354453   0.190103 133.405552   1.865  0.06444 .  
# 


facet_labels <- c("Bilingual" = "Multilingual", "Monolingual" = "Monolingual")



ggplot(mean_dat, aes(x = Language, y = mean_rating, fill = factor(Language))) +
  stat_summary(fun = "mean", geom = "bar")  + 
  stat_summary(fun = "mean", geom = "text", aes(label = round(..y.., 2)), position = position_stack(vjust = 0.5), color = "black", size = 5) +
  labs(title = "Credibility Ratings by Language",
       x = "Language",
       y = "Credibility Rating",
       fill = "Language") +
  theme(axis.text.x = element_blank(), axis.text.y = element_text(size=15)) +
  scale_fill_manual(values = c("yellow", "red4", "red", "dodgerblue4", "deepskyblue", "green4", "limegreen")) + 
  scale_y_continuous(limits= c(0,4), breaks = 1:4,  # Specify the y-axis positions
                     labels = c("1: I am certain this is false", "2: I think this is false", "3: I think this is right", "4: I am certain this is right")) +
  facet_grid(~ participant_group, labeller = as_labeller(facet_labels))



################# Accent Strength #########################

mixed_aov_acc <- aov(mean_rating ~ speaker_strength * participant_group, data = mean_dat)
summary(mixed_aov_acc)
#                 Df Sum Sq Mean Sq F value Pr(>F)
# speaker_strength           2   0.40 0.20112   1.382  0.253
# participant_group                  1   0.02 0.01912   0.131  0.717
# speaker_strength:participant_group   2   0.07 0.03464   0.238  0.788
# Residuals              302  43.96 0.14556 


mixed_emmeans_acc <- emmeans(mixed_aov_acc, ~ speaker_strength * participant_group)
pairs(mixed_emmeans_acc, adjust = "tukey") # No sig



mixed_boxcox_acc <- boxcox(mean_rating ~ speaker_strength * participant_group, data = mean_dat) 
mixed_boxcox_acc $x[which.max(mixed_boxcox_acc$y)] # = 1.23

mixed_boxcox_acc2 <- boxcox(mean_rating ~ speaker_strength * participant_group + participant_sex + participant_age, data = mean_dat) 
mixed_boxcox_acc2$x[which.max(mixed_boxcox_acc2$y)] # = 1.19

mixed_lme_acc <- lmer(mean_rating ~ speaker_strength  * participant_group + (1 | list), data = mean_dat)
mixed_lme_acc2 <- lmer(mean_rating ~ speaker_strength  * participant_group + participant_sex + participant_age + (1 | list), data = mean_dat)


#Lower is better for each!
AIC(mixed_lme_acc) #
AIC(mixed_lme_acc2)

BIC(mixed_lme_acc) #
BIC(mixed_lme_acc2)

summary(mixed_lme_acc) #REML  298 - no sig

summary(mixed_lme_acc2) # significance  301.2




ggplot(mean_dat, aes(x = speaker_strength, y = mean_rating, fill = factor(speaker_strength))) +
  stat_summary(fun = "mean", geom = "bar")  + 
  stat_summary(fun = "mean", geom = "text", aes(label = round(..y.., 2)), position = position_stack(vjust = 0.5), color = "black", size = 5) +
  labs(title = "Credibility Ratings by Accent",
       x = "Language",
       y = "Credibility Rating",
       fill = "Language") +
  theme(axis.text.x = element_blank(), axis.text.y = element_text(size=15)) +
  scale_fill_manual(values = c("yellow", "green4", "red4", "dodgerblue4", "deepskyblue", "green4", "limegreen")) + 
  scale_y_continuous(limits= c(0,4), breaks = 1:4,  # Specify the y-axis positions
                     labels = c("1: I am certain this is false", "2: I think this is false", "3: I think this is right", "4: I am certain this is right")) + 
  facet_grid(~ participant_group, labeller = as_labeller(facet_labels))




################# Base Language  #########################

mixed_aov_base <- aov(mean_rating ~ speaker_language  * participant_group, data = mean_dat)
summary(mixed_aov_base) 
#               Df Sum Sq Mean Sq F value Pr(>F)
# speaker_language           3   0.40 0.13384   0.919  0.432
# participant_group                1   0.02 0.01912   0.131  0.717
# speaker_language:participant_group   3   0.34 0.11484   0.789  0.501
# Residuals            300  43.69 0.14562 

mixed_base <- emmeans(mixed_aov_base, ~ speaker_language * participant_group)
pairs(mixed_base, adjust = "tukey") #no sig


mixed_boxcox_base <- boxcox(mean_rating ~ speaker_language  * participant_group, data = mean_dat) 
mixed_boxcox_base $x[which.max(mixed_boxcox_base$y)] # = 1.23

mixed_boxcox_base2 <- boxcox(mean_rating ~ speaker_language * participant_group +participant_sex + participant_age, data = mean_dat) 
mixed_boxcox_base2$x[which.max(mixed_boxcox_base2$y)] # = 1.19

mixed_lme_base <- lmer(mean_rating ~ speaker_language  * participant_group + (1 | list), data = mean_dat)
mixed_lme_base2 <- lmer(mean_rating ~ speaker_language * participant_group + participant_sex + participant_age + (1 | list), data = mean_dat)

#Lower is better for each!
AIC(mixed_lme_base) #
AIC(mixed_lme_base2)
BIC(mixed_lme_base) #
BIC(mixed_lme_base2)

summary(mixed_lme_base) #REML 302.2
#                                   Estimate Std. Error        df t value Pr(>|t|)    
# BaseLanguageFrench               -0.18523    0.09964 300.00000  -1.859    0.064 .  

summary(mixed_lme_base2) #REML2 305.5
# BaseLanguageFrench               -0.18523    0.09374 271.17521  -1.976  0.04917 *  
# Age21                            -0.38085    0.19400 176.49818  -1.963  0.05120 .  
# Age32                            -0.43744    0.16269 156.63063  -2.689  0.00795 ** 
# Age36                            -0.24767    0.13160 265.13382  -1.882  0.06093 .  
# Age37                            -0.46216    0.15552 273.24326  -2.972  0.00323 ** 
# Age41                            -0.63816    0.19827 166.40617  -3.219  0.00155 ** 
# Age46                            -0.56543    0.19312 198.48276  -2.928  0.00381 ** 
# Age51                             0.35543    0.18893 137.42433   1.881  0.06204 .




# Filter out rows with NA values in mean_rating

ggplot(mean_dat, aes(x = speaker_language, y = mean_rating, fill = factor(speaker_language))) +
  stat_summary(fun = "mean", geom = "bar")  + 
  stat_summary(fun = "mean", geom = "text", aes(label = round(..y.., 2)), position = position_stack(vjust = 0.5), color = "black", size = 5) +
  labs(title = "Credibility Ratings by Language",
       x = "Language",
       y = "Credibility Rating",
       fill = "Language") +
  theme(axis.text.x = element_blank(), axis.text.y = element_text(size=15)) +
  scale_fill_manual(values = c("yellow", "red4", "dodgerblue4", "green4", "limegreen")) + 
  scale_y_continuous(limits= c(0,4), breaks = 1:4,  # Specify the y-axis positions
                     labels = c("1: I am certain this is false", "2: I think this is false", "3: I think this is right", "4: I am certain this is right")) + 
  facet_grid(~ participant_group, labeller = as_labeller(facet_labels))




#################### Truthfulness #############################


mixed_aov_truth <- aov(rating ~  truthfulness * Language + participant_group, data = dat)
summary(mixed_aov_truth)
# Df Sum Sq Mean Sq F value   Pr(>F)    
# truthfulness             1   25.3  25.330  35.863 2.54e-09 ***
#   Language                 6    2.8   0.474   0.671    0.673    
# participant_group        1    0.1   0.126   0.178    0.673    
# truthfulness:Language    6    1.1   0.180   0.255    0.957    
# Residuals             1832 1293.9   0.706      

mixed_emmeans_truth <- emmeans(mixed_aov_truth, ~ truthfulness * participant_group)
pairs(mixed_emmeans_truth, adjust = "tukey") # No sig



mixed_boxcox_truth <- boxcox(rating ~ truthfulness * Language +  participant_group, data = dat) 
mixed_boxcox_truth $x[which.max(mixed_boxcox_truth$y)] # = 0.87

mixed_boxcox_truth2 <- boxcox(rating ~ truthfulness * Language + participant_group + participant_sex + participant_age, data = dat) 
mixed_boxcox_truth2$x[which.max(mixed_boxcox_truth2$y)] # = 0.87

mixed_lme_truth <- lmer(rating ~ truthfulness * Language + participant_group + (1 | list), data = dat)
mixed_lme_truth2 <- lmer(rating ~ truthfulness * Language + participant_group + participant_sex + participant_age + (1 | list), data = dat)


#Lower is better for each!
AIC(mixed_lme_truth) #
AIC(mixed_lme_truth2)

BIC(mixed_lme_truth) #
BIC(mixed_lme_truth2)

summary(mixed_lme_truth) #REML 4636.4
# truthfulness1                          0.28321    0.10356 1832.00000   2.735   0.0063 ** 
  
summary(mixed_lme_truth2) # significance  4633.1
# truthfulness1                          0.28203    0.10233 1804.58015   2.756  0.00591 ** 
# participant_groupMonolingual           0.20353    0.06459  271.36583   3.151  0.00181 ** 
# 


ggplot(dat, aes(x = truthfulness, y = rating, fill = factor(Language))) +
  stat_summary(fun = "mean", geom = "bar", position = position_dodge(width = 0.9)) +  
  stat_summary(fun = "mean", geom = "text", 
               aes(label = round(..y.., 2)), 
               position = position_dodge(width = 0.9), vjust = -0.5, color = "black", size = 5) +
  
  labs(title = "Credibility Ratings by Truthfulness, Language, and Participant Group",
       x = "Truthfulness of Statement",
       y = "Credibility Rating",
       fill = "Speaker Language") +   # Updated the label for fill
  theme(axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 15)) +
  scale_x_discrete(labels = c("0" = "False", "1" = "True")) +  # Custom labels for truthfulness
  scale_fill_manual(values = c("yellow", "red4", "red", "dodgerblue4", "deepskyblue", "green4", "limegreen")) +
  scale_y_continuous(limits = c(0, 4), breaks = 1:4,  # Specify the y-axis positions
                     labels = c("1: I am certain this is false", 
                                "2: I think this is false", 
                                "3: I think this is right", 
                                "4: I am certain this is right")) +
  facet_grid(~participant_group, 
             labeller = as_labeller(c("Monolingual" = "Monolingual", "Bilingual" = "Multilingual")))  # Custom facet labels






# Calculate mean_rating for each combination of Language and truthfulness
dat_summary <- dat %>%
  group_by(Language, truthfulness, participant_group) %>%
  summarize(mean_rating = mean(rating, na.rm = TRUE), .groups = "drop")


ggplot(dat_summary, aes(x = truthfulness, y = mean_rating, fill = factor(Language))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +  # "stat = 'identity'" for pre-calculated means
  geom_text(aes(label = round(mean_rating, 2)), 
            position = position_dodge(width = 0.9), vjust = -0.5, color = "black", size = 5) +
  
  labs(title = "Mean Credibility Ratings by Truthfulness, Language, and Participant Group",
       x = "Truthfulness of Statement",
       y = "Mean Credibility Rating",
       fill = "Speaker Language") +
  theme(axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 15)) +
  scale_x_discrete(labels = c("0" = "False", "1" = "True")) +
  scale_fill_manual(values = c("yellow", "red4", "red", "dodgerblue4", "deepskyblue", "green4", "limegreen")) +
  scale_y_continuous(limits = c(0, 4), breaks = 1:4,  
                     labels = c("1: I am certain this is false", 
                                "2: I think this is false", 
                                "3: I think this is right", 
                                "4: I am certain this is right")) +
  facet_grid(~participant_group, 
             labeller = as_labeller(c("Monolingual" = "Monolingual", "Bilingual" = "Multilingual"))) 



###################################################   participant_sex ########################################################### 

mixed_aov_gen <- aov(mean_rating ~ Language * participant_group * participant_sex, data = mean_dat)
summary(mixed_aov_gen) 
#                           Df Sum Sq Mean Sq F value Pr(>F)
# Language                  6   0.48 0.07944   0.533  0.783
# participant_group                   1   0.02 0.01912   0.128  0.720
# participant_sex                    1   0.17 0.17100   1.148  0.285
# Language:participant_group          6   0.52 0.08659   0.581  0.745
# Language:participant_sex           6   0.27 0.04464   0.300  0.937
# participant_group:participant_sex            1   0.07 0.07185   0.482  0.488
# Language:participant_group:participant_sex   6   1.23 0.20440   1.373  0.226
# Residuals               280  41.70 0.14892 

mixed__gen <- emmeans(mixed_aov_gen, ~ Language * participant_group * participant_sex)
pairs(mixed__gen, adjust = "tukey")



mixed_boxcox_gen <- boxcox(mean_rating ~ Language * participant_group * participant_sex, data = mean_dat) 
mixed_boxcox_gen $x[which.max(mixed_boxcox_gen$y)] #

mixed_boxcox_gen2 <- boxcox(mean_rating ~ Language * participant_group * participant_sex + participant_age, data = mean_dat) 
mixed_boxcox_gen2$x[which.max(mixed_boxcox_gen2$y)] # 

mixed_lme_gen <- lmer(mean_rating ~ Language * participant_group * participant_sex + (1 | list), data = mean_dat)
mixed_lme_gen2 <- lmer(mean_rating ~ Language * participant_group * participant_sex + participant_age + (1 | list), data = mean_dat)



#Lower is better for each!
AIC(mixed_lme_gen) #
AIC(mixed_lme_gen2) 
BIC(mixed_lme_gen) #
BIC(mixed_lme_gen2)

summary(mixed_lme_gen) #REML  325.3

summary(mixed_lme_gen2) #REML2 - 




facet_gen <- c("Bilingual" = "Multilingual", "Mono" = "Monolingual", Female = "Female", Male = "Male")


ggplot(mean_dat, aes(x = Language, y = mean_rating, fill = factor(Language))) +
  stat_summary(fun = "mean", geom = "bar")  + 
  stat_summary(fun = "mean", geom = "text", aes(label = round(..y.., 2)), position = position_stack(vjust = 0.5), color = "black", size = 5) +
  labs(title = "Credibility Ratings of Each Language by participant_sex",
       x = "Language",
       y = "Credibility Rating",
       fill = "Language") +
  theme(axis.text.x = element_blank(), axis.text.y = element_text(size=15)) +
  scale_fill_manual(values = c("yellow", "red4", "red", "dodgerblue4", "deepskyblue", "green4", "limegreen")) + 
  facet_grid( participant_group ~participant_sex, labeller = as_labeller(facet_gen)) + 
  scale_y_continuous(limits= c(0,4), breaks = 1:4,  # Specify the y-axis positions
                     labels = c("1: I am certain this is false", "2: I think this is false", "3: I think this is right", "4: I am certain this is right"))




###################################################   participant_age  ########################################################### 

mixed_aov_age <- aov(mean_rating ~ Language * participant_group * participant_age, data = mean_dat)
summary(mixed_aov_age) 
#               Df Sum Sq Mean Sq F value Pr(>F)  
# Language                                     6  0.477  0.0794   0.613 0.71974   
# participant_group                            1  0.019  0.0191   0.147 0.70176   
# participant_age                             21  6.818  0.3247   2.504 0.00117 **
#   Language:participant_group                   6  0.520  0.0866   0.668 0.67582   
# Language:participant_age                   126 16.205  0.1286   0.992 0.51971   
# participant_group:participant_age            6  2.461  0.4102   3.164 0.00682 **
#   Language:participant_group:participant_age  36  4.336  0.1205   0.929 0.58804   

mixed__age <- emmeans(mixed_aov_age, ~ Language * participant_group * participant_age)
mixed__age <- pairs(mixed__age, adjust = "tukey")
summary(mixed__age) #PAIR EMMEANS 



mixed_age_emm <- emmeans(mixed_aov_age, ~ Language * participant_group * participant_age)
mixed_age_pairs <- pairs(mixed_age_emm, adjust = "tukey")

# Convert pairwise comparisons to a data frame and filter significant results
significant_results <- summary(mixed_age_pairs) %>%
  as.data.frame() %>%
  filter(p.value < 0.05)

print(significant_results)


mixed_boxcox_age <- boxcox(mean_rating ~ Language * participant_group* participant_age, data = mean_dat) 
mixed_boxcox_age $x[which.max(mixed_boxcox_age$y)] #2

mixed_boxcox_age2 <- boxcox(mean_rating ~ Language * participant_group * participant_age + participant_sex, data = mean_dat) 
mixed_boxcox_age2$x[which.max(mixed_boxcox_age2$y)] #2 

mixed_lme_age <- lmer((mean_rating)^2 ~ Language * participant_group * participant_age + (1 | list), data = mean_dat)
mixed_lme_age2 <- lmer((mean_rating)^2 ~ Language * participant_group * participant_age + participant_sex + (1 | list), data = mean_dat)



#Lower is better for each!
AIC(mixed_lme_age) #
AIC(mixed_lme_age2) 
BIC(mixed_lme_age) #
BIC(mixed_lme_age2)

summary(mixed_lme_age) #REML 301.7 
# Age51                                            3.7047     2.1001 104.9907   1.764  0.08063 .  
# LanguageMild French Accent:Mono_BiMono           6.0789     3.4045  99.9447   1.786  0.07720 .  
# LanguageHeavy Spanish Accent:Age22              -5.0160     2.9483  99.9447  -1.701  0.09200 .  
# LanguageHeavy Italian Accent:Age26              -4.0878     2.1976  99.9447  -1.860  0.06580 .  
# LanguageHeavy Italian Accent:Age27              -6.6474     2.9483  99.9447  -2.255  0.02634 *  
# LanguageHeavy Spanish Accent:Age32              -8.0700     4.1696  99.9447  -1.935  0.05576 .  
# LanguageHeavy French Accent:Age33               -5.0263     2.9483  99.9447  -1.705  0.09134 .  
# LanguageMild French Accent:Age33                 2.1128     2.9483  99.9447   0.717  0.47529    
# LanguageHeavy Italian Accent:Age33              -7.3874     2.9483  99.9447  -2.506  0.01384 *  
# LanguageHeavy Spanish Accent:Age33              -6.3521     2.9483  99.9447  -2.154  0.03361 *  
#   LanguageMild Spanish Accent:Age33               -5.2342     2.9483  99.9447  -1.775  0.07889 .  
# LanguageHeavy Italian Accent:Age37              -7.9289     4.1696  99.9447  -1.902  0.06010 .  
# LanguageMild Italian Accent:Age37               -5.5260     4.1696  99.9447  -1.325  0.18809    
# LanguageHeavy Spanish Accent:Age37              -9.4258     4.1696  99.9447  -2.261  0.02595 *  
# LanguageHeavy Spanish Accent:Age46              -8.0479     4.5037  99.9447  -1.787  0.07697 .  
# LanguageHeavy Italian Accent:Age51              -5.1221     2.9483  99.9447  -1.737  0.08542 .  
# LanguageHeavy Spanish Accent:Age55               5.6776     2.9483  99.9447   1.926  0.05698 .  
# Mono_BiMono:Age23                                5.9463     2.9856 102.5408   1.992  0.04907 *  


summary(mixed_lme_age2) #REML2 479.4


mixed_age<- mean_dat %>%
  group_by(Language, participant_age, participant_group) %>%
  summarize(mean_rating = mean(mean_rating, na.rm = TRUE))
print(mixed_age)

facet_age <- c("Bi" = "Multilingual", "Mono" = "Monolingual", "19" = "19", "21" = "21", "22" = "22","23" = "23", "24" = "24", "26" = "26", "27" = "27", "28" = "28", "30" = "30", "31" = "31", "32" = "32", "33" = "33", "34" = "34", "36" = "36", "37" = "37", "38" = "38", "40" = "40", "41" = "41", "46" = "46", "51" = "51", "55" = "55", "57" = "57")


ggplot(mean_dat, aes(x = Language, y = mean_rating, fill = factor(Language))) +
  stat_summary(fun = "mean", geom = "bar")  + 

  labs(title = "Credibility Ratings of Each Language by participant_sex",
       x = "Language",
       y = "Credibility Rating",
       fill = "Language") +
  theme(axis.text.x = element_blank(), axis.text.y = element_text(size=15)) +
  scale_fill_manual(values = c("yellow", "red4", "red", "dodgerblue4", "deepskyblue", "green4", "limegreen")) + 
  facet_grid( participant_group ~ participant_age, labeller = as_labeller(facet_age)) + 
  scale_y_continuous(limits= c(0,4), breaks = 1:4,  # Specify the y-axis positions
                     labels = c("1: I am certain this is false", "2: I think this is false", "3: I think this is right", "4: I am certain this is right"))


###################################################################################################################################################################################################################################################################################################
# Arrange by participant_group (Mono first, then Bi) and then by mean_rating (highest to lowest)
age22 <- mixed_age %>%
  filter(participant_age == 22) %>%
  arrange(participant_group == "Mono", desc(mean_rating)) 
print(age22)

# Load ggplot2 if not already loaded
library(ggplot2)

# Create the plot
ggplot(age22, aes(x = reorder(Language, -mean_rating), y = mean_rating, fill = participant_group)) +
  stat_summary(fun = "mean", geom = "bar")  + 
  stat_summary(fun = "mean", geom = "text", aes(label = round(..y.., 2)), position = position_stack(vjust = 0.5), color = "black", size = 5) +
  labs(title = "Credibility Ratings by Language for participant_age 22",
       x = "Language",
       y = "Mean Rating",
       fill = "Language Group") +
  theme_minimal() +
  scale_fill_manual(name = "Language Group", 
                    labels = c("Multilingual", "Monolingual"), 
                    values = c("red", "blue")) +  # Customize colors if desired
  scale_y_continuous(limits = c(0, 4), breaks = 1:4,
                     labels = c("1: Certain False", "2: Think False", "3: Think Right", "4: Certain Right"))




age23 <- mixed_age %>%
  filter(participant_age == 23) %>%
  arrange(participant_group == "Mono", desc(mean_rating)) 
print(age23)

# Load ggplot2 if not already loaded
library(ggplot2)

# Create the plot
ggplot(age23, aes(x = reorder(Language, -mean_rating), y = mean_rating, fill = participant_group)) +
  stat_summary(fun = "mean", geom = "bar")  + 
  stat_summary(fun = "mean", geom = "text", aes(label = round(..y.., 2)), position = position_stack(vjust = 0.5), color = "black", size = 5) +
  labs(title = "Credibility Ratings by Language for participant_age 23",
       x = "Language",
       y = "Mean Rating",
       fill = "Language Group") +
  theme_minimal() +
  scale_fill_manual(name = "Language Group", 
                    labels = c("Multilingual", "Monolingual"), 
                    values = c("red", "blue")) +  # Customize colors if desired
  scale_y_continuous(limits = c(0, 4), breaks = 1:4,
                     labels = c("1: Certain False", "2: Think False", "3: Think Right", "4: Certain Right"))






age27 <- mixed_age %>%
  filter(participant_age == 27) %>%
  arrange(participant_group == "Mono", desc(mean_rating)) 
print(age27)

# Load ggplot2 if not already loaded
library(ggplot2)

# Create the plot
ggplot(age27, aes(x = reorder(Language, -mean_rating), y = mean_rating, fill = participant_group)) +
  stat_summary(fun = "mean", geom = "bar")  + 
  stat_summary(fun = "mean", geom = "text", aes(label = round(..y.., 2)), position = position_stack(vjust = 0.5), color = "black", size = 5) +
  labs(title = "Credibility Ratings by Language for participant_age 27",
       x = "Language",
       y = "Mean Rating",
       fill = "Language Group") +
  theme_minimal() +
  scale_fill_manual(name = "Language Group", 
                    labels = c("Multilingual", "Monolingual"), 
                    values = c("red", "blue")) +  # Customize colors if desired
  scale_y_continuous(limits = c(0, 4), breaks = 1:4,
                     labels = c("1: Certain False", "2: Think False", "3: Think Right", "4: Certain Right"))



age30 <- mixed_age %>%
  filter(participant_age == 30) %>%
  arrange(participant_group == "Mono", desc(mean_rating)) 
print(age30)

# Load ggplot2 if not already loaded
library(ggplot2)

# Create the plot
ggplot(age30, aes(x = reorder(Language, -mean_rating), y = mean_rating, fill = participant_group)) +
  geom_bar(stat = "identity", position = "dodge") + 
  geom_text(aes(label = round(mean_rating, 2)), 
            position = position_dodge(width = 0.9), vjust = -0.3, color = "black", size = 5) + 
  labs(title = "Credibility Ratings by Language for participant_age 30",
       x = "Language",
       y = "Mean Rating",
       fill = "Language Group") +
  theme_minimal() +
  scale_fill_manual(name = "Language Group", 
                    labels = c("Multilingual", "Monolingual"), 
                    values = c("red", "blue")) +  # Customize colors if desired
  scale_y_continuous(limits = c(0, 4), breaks = 1:4,
                     labels = c("1: Certain False", "2: Think False", "3: Think Right", "4: Certain Right"))



age33 <- mixed_age %>%
  filter(participant_age == 33) %>%
  arrange(participant_group == "Mono", desc(mean_rating)) 
print(age33)

# Load ggplot2 if not already loaded
library(ggplot2)

# Create the plot
ggplot(age33, aes(x = reorder(Language, -mean_rating), y = mean_rating, fill = participant_group)) +
  stat_summary(fun = "mean", geom = "bar")  + 
  stat_summary(fun = "mean", geom = "text", aes(label = round(..y.., 2)), position = position_stack(vjust = 0.5), color = "black", size = 5) +
  labs(title = "Credibility Ratings by Language for participant_age 33",
       x = "Language",
       y = "Mean Rating",
       fill = "Language Group") +
  theme_minimal() +
  scale_fill_manual(name = "Language Group", 
                    labels = c("Multilingual", "Monolingual"), 
                    values = c("red", "blue")) +  # Customize colors if desired
  scale_y_continuous(limits = c(0, 4), breaks = 1:4,
                     labels = c("1: Certain False", "2: Think False", "3: Think Right", "4: Certain Right"))



age36 <- mixed_age %>%
  filter(participant_age == 36) %>%
  arrange(participant_group == "Mono", desc(mean_rating)) 
print(age36)

# Load ggplot2 if not already loaded
library(ggplot2)

# Create the plot
ggplot(age36, aes(x = reorder(Language, -mean_rating), y = mean_rating, fill = participant_group)) +
  stat_summary(fun = "mean", geom = "bar")  + 
  stat_summary(fun = "mean", geom = "text", aes(label = round(..y.., 2)), position = position_stack(vjust = 0.5), color = "black", size = 5) +
  labs(title = "Credibility Ratings by Language for participant_age 36",
       x = "Language",
       y = "Mean Rating",
       fill = "Language Group") +
  theme_minimal() +
  scale_fill_manual(name = "Language Group", 
                    labels = c("Multilingual", "Monolingual"), 
                    values = c("red", "blue")) +  # Customize colors if desired
  scale_y_continuous(limits = c(0, 4), breaks = 1:4,
                     labels = c("1: Certain False", "2: Think False", "3: Think Right", "4: Certain Right"))



age40 <- mixed_age %>%
  filter(participant_age == 40) %>%
  arrange(participant_group == "Mono", desc(mean_rating)) 
print(age40)

# Load ggplot2 if not already loaded
library(ggplot2)

# Create the plot
ggplot(age40, aes(x = reorder(Language, -mean_rating), y = mean_rating, fill = participant_group)) +
  stat_summary(fun = "mean", geom = "bar")  + 
  stat_summary(fun = "mean", geom = "text", aes(label = round(..y.., 2)), position = position_stack(vjust = 0.5), color = "black", size = 5) +
  labs(title = "Credibility Ratings by Language for participant_age 40",
       x = "Language",
       y = "Mean Rating",
       fill = "Language Group") +
  theme_minimal() +
  scale_fill_manual(name = "Language Group", 
                    labels = c("Multilingual", "Monolingual"), 
                    values = c("red", "blue")) +  # Customize colors if desired
  scale_y_continuous(limits = c(0, 4), breaks = 1:4,
                     labels = c("1: Certain False", "2: Think False", "3: Think Right", "4: Certain Right"))



age37 <- mixed_age %>%
  filter(participant_age == 37) %>%
  arrange(participant_group == "Mono", desc(mean_rating)) 
print(age37)

# Load ggplot2 if not already loaded
library(ggplot2)

# Create the plot
ggplot(age37, aes(x = reorder(Language, -mean_rating), y = mean_rating, fill = participant_group)) +
  stat_summary(fun = "mean", geom = "bar")  + 
  stat_summary(fun = "mean", geom = "text", aes(label = round(..y.., 2)), position = position_stack(vjust = 0.5), color = "black", size = 5) +
  labs(title = "Credibility Ratings by Language for participant_age 37",
       x = "Language",
       y = "Mean Rating",
       fill = "Language Group") +
  theme_minimal() +
  scale_fill_manual(name = "Language Group", 
                    labels = c("Multilingual", "Monolingual"), 
                    values = c("red", "blue")) +  # Customize colors if desired
  scale_y_continuous(limits = c(0, 4), breaks = 1:4,
                     labels = c("1: Certain False", "2: Think False", "3: Think Right", "4: Certain Right"))


###################################################################################################################################################################################################################################################################################################


##################### Specific AGE ###############################

library(patchwork)
library(tidyverse)


############### AGE 22 ##########################################


# Filter data for monolingual and bilingual groups
age22_mono <- age22 %>% filter(participant_group == "Mono")
age22_multi <- age22 %>% filter(participant_group == "Bi")

# Graph 1: Combined bar chart
plot_combined <- ggplot(age22, aes(x = reorder(Language, -mean_rating), y = mean_rating, fill = participant_group)) +
  stat_summary(fun = "mean", geom = "bar")  + 
  stat_summary(fun = "mean", geom = "text", aes(label = round(..y.., 2)), position = position_stack(vjust = 0.5), color = "black", size = 5) +
  labs(title = "Combined: Credibility Ratings by Language for participant_age 22",
       x = "Language",
       y = "Mean Rating",
       fill = "Language Group") +
  theme_minimal() +
  scale_fill_manual(name = "Language Group", 
                    labels = c("Bilingual", "Monolingual"), 
                    values = c("red", "blue")) +
  scale_y_continuous(limits = c(0, 4), breaks = 1:4,
                     labels = c("1: Certain False", "2: Think False", "3: Think Right", "4: Certain Right")) 

# Graph 2: Monolingual-only bar chart with customized labels and title
plot_mono <- ggplot(age22_mono, aes(x = reorder(Language, -mean_rating), y = mean_rating, fill = participant_group)) +
  stat_summary(fun = "mean", geom = "bar")  + 
  stat_summary(fun = "mean", geom = "text", aes(label = round(..y.., 2)), position = position_stack(vjust = 0.5), color = "black", size = 5) +
  labs(title = "Monolingual: Credibility Ratings by Language for participant_age 22",
       x = "Language",
       y = "Mean Rating",
       fill = "Language Group") +
  theme_minimal() +
  scale_fill_manual(name = "Language Group", values = c("blue"), labels = "Monolingual") + 
  scale_y_continuous(limits = c(0, 4), breaks = 1:4,
                     labels = c("1: Certain False", "2: Think False", "3: Think Right", "4: Certain Right")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10)) +
  coord_cartesian(clip = "off")

# Graph 3: Bilingual-only bar chart with customized labels and title
plot_multi <- ggplot(age22_multi, aes(x = reorder(Language, -mean_rating), y = mean_rating, fill = participant_group)) +
  geom_bar(stat = "identity") + 
  stat_summary(fun = "mean", geom = "bar")  + 
  stat_summary(fun = "mean", geom = "text", aes(label = round(..y.., 2)), position = position_stack(vjust = 0.5), color = "black", size = 5) +
  labs(title = "Multilingual: Credibility Ratings by Language for participant_age 22",
       x = "Language",
       y = "Mean Rating",
       fill = "Language Group") +
  theme_minimal() +
  scale_fill_manual(name = "Language Group", values = c("red"), labels = "Multilingual") + 
  scale_y_continuous(limits = c(0, 4), breaks = 1:4,
                     labels = c("1: Certain False", "2: Think False", "3: Think Right", "4: Certain Right")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10)) +
  coord_cartesian(clip = "off")

# Combine all three plots
(plot_combined / (plot_multi | plot_mono)) + 
  plot_layout(heights = c(2, 1))  # Adjust heights to balance the visual layout



############### AGE 23 ##########################################


# Filter data for monolingual and bilingual groups
age23_mono <- age23 %>% filter(participant_group == "Mono")
age23_multi <- age23 %>% filter(participant_group == "Bi")

# Graph 1: Combined bar chart
plot_combined <- ggplot(age23, aes(x = reorder(Language, -mean_rating), y = mean_rating, fill = participant_group)) +
  stat_summary(fun = "mean", geom = "bar")  + 
  stat_summary(fun = "mean", geom = "text", aes(label = round(..y.., 2)), position = position_stack(vjust = 0.5), color = "black", size = 5) +
  labs(title = "Combined: Credibility Ratings by Language for participant_age 23",
       x = "Language",
       y = "Mean Rating",
       fill = "Language Group") +
  theme_minimal() +
  scale_fill_manual(name = "Language Group", 
                    labels = c("Bilingual", "Monolingual"), 
                    values = c("red", "blue")) +
  scale_y_continuous(limits = c(0, 4), breaks = 1:4,
                     labels = c("1: Certain False", "2: Think False", "3: Think Right", "4: Certain Right")) 

# Graph 2: Monolingual-only bar chart with customized labels and title
plot_mono <- ggplot(age23_mono, aes(x = reorder(Language, -mean_rating), y = mean_rating, fill = participant_group)) +
  stat_summary(fun = "mean", geom = "bar")  + 
  stat_summary(fun = "mean", geom = "text", aes(label = round(..y.., 2)), position = position_stack(vjust = 0.5), color = "black", size = 5) +
  labs(title = "Monolingual: Credibility Ratings by Language for participant_age 23",
       x = "Language",
       y = "Mean Rating",
       fill = "Language Group") +
  theme_minimal() +
  scale_fill_manual(name = "Language Group", values = c("blue"), labels = "Monolingual") + 
  scale_y_continuous(limits = c(0, 4), breaks = 1:4,
                     labels = c("1: Certain False", "2: Think False", "3: Think Right", "4: Certain Right")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10)) +
  coord_cartesian(clip = "off")

# Graph 3: Bilingual-only bar chart with customized labels and title
plot_multi <- ggplot(age23_multi, aes(x = reorder(Language, -mean_rating), y = mean_rating, fill = participant_group)) +
  geom_bar(stat = "identity") + 
  geom_text(aes(label = round(mean_rating, 2)), 
            vjust = -0.3, color = "black", size = 5) + 
  labs(title = "Multilingual: Credibility Ratings by Language for participant_age 23",
       x = "Language",
       y = "Mean Rating",
       fill = "Language Group") +
  theme_minimal() +
  scale_fill_manual(name = "Language Group", values = c("red"), labels = "Multilingual") + 
  scale_y_continuous(limits = c(0, 4), breaks = 1:4,
                     labels = c("1: Certain False", "2: Think False", "3: Think Right", "4: Certain Right")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10)) +
  coord_cartesian(clip = "off")

# Combine all three plots
(plot_combined / (plot_multi | plot_mono)) + 
  plot_layout(heights = c(2, 1))  # Adjust heights to balance the visual layout



############### AGE 27 ##########################################


# Filter data for monolingual and bilingual groups
age27_mono <- age27 %>% filter(participant_group == "Mono")
age27_multi <- age27 %>% filter(participant_group == "Bi")

# Graph 1: Combined bar chart
plot_combined <- ggplot(age27, aes(x = reorder(Language, -mean_rating), y = mean_rating, fill = participant_group)) +
  stat_summary(fun = "mean", geom = "bar")  + 
  stat_summary(fun = "mean", geom = "text", aes(label = round(..y.., 2)), position = position_stack(vjust = 0.5), color = "black", size = 5) +
  labs(title = "Combined: Credibility Ratings by Language for participant_age 27",
       x = "Language",
       y = "Mean Rating",
       fill = "Language Group") +
  theme_minimal() +
  scale_fill_manual(name = "Language Group", 
                    labels = c("Bilingual", "Monolingual"), 
                    values = c("red", "blue")) +
  scale_y_continuous(limits = c(0, 4), breaks = 1:4,
                     labels = c("1: Certain False", "2: Think False", "3: Think Right", "4: Certain Right")) 

# Graph 2: Monolingual-only bar chart with customized labels and title
plot_mono <- ggplot(age27_mono, aes(x = reorder(Language, -mean_rating), y = mean_rating, fill = participant_group)) +
  stat_summary(fun = "mean", geom = "bar")  + 
  stat_summary(fun = "mean", geom = "text", aes(label = round(..y.., 2)), position = position_stack(vjust = 0.5), color = "black", size = 5) +
  labs(title = "Monolingual: Credibility Ratings by Language for participant_age 27",
       x = "Language",
       y = "Mean Rating",
       fill = "Language Group") +
  theme_minimal() +
  scale_fill_manual(name = "Language Group", values = c("blue"), labels = "Monolingual") + 
  scale_y_continuous(limits = c(0, 4), breaks = 1:4,
                     labels = c("1: Certain False", "2: Think False", "3: Think Right", "4: Certain Right")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10)) +
  coord_cartesian(clip = "off")

# Graph 3: Bilingual-only bar chart with customized labels and title
plot_multi <- ggplot(age27_multi, aes(x = reorder(Language, -mean_rating), y = mean_rating, fill = participant_group)) +
  stat_summary(fun = "mean", geom = "bar")  + 
  stat_summary(fun = "mean", geom = "text", aes(label = round(..y.., 2)), position = position_stack(vjust = 0.5), color = "black", size = 5) +
  labs(title = "Multilingual: Credibility Ratings by Language for participant_age 27",
       x = "Language",
       y = "Mean Rating",
       fill = "Language Group") +
  theme_minimal() +
  scale_fill_manual(name = "Language Group", values = c("red"), labels = "Multilingual") + 
  scale_y_continuous(limits = c(0, 4), breaks = 1:4,
                     labels = c("1: Certain False", "2: Think False", "3: Think Right", "4: Certain Right")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10)) +
  coord_cartesian(clip = "off")

# Combine all three plots
(plot_combined / (plot_multi | plot_mono)) + 
  plot_layout(heights = c(2, 1))  # Adjust heights to balance the visual layout

############### AGE 30 ##########################################


# Filter data for monolingual and bilingual groups
age30_mono <- age30 %>% filter(participant_group == "Mono")
age30_multi <- age30 %>% filter(participant_group == "Bi")

# Graph 1: Combined bar chart
plot_combined <- ggplot(age30, aes(x = reorder(Language, -mean_rating), y = mean_rating, fill = participant_group)) +
  stat_summary(fun = "mean", geom = "bar")  + 
  stat_summary(fun = "mean", geom = "text", aes(label = round(..y.., 2)), position = position_stack(vjust = 0.5), color = "black", size = 5) +
  labs(title = "Combined: Credibility Ratings by Language for participant_age 30",
       x = "Language",
       y = "Mean Rating",
       fill = "Language Group") +
  theme_minimal() +
  scale_fill_manual(name = "Language Group", 
                    labels = c("Bilingual", "Monolingual"), 
                    values = c("red", "blue")) +
  scale_y_continuous(limits = c(0, 4), breaks = 1:4,
                     labels = c("1: Certain False", "2: Think False", "3: Think Right", "4: Certain Right")) 

# Graph 2: Monolingual-only bar chart with customized labels and title
plot_mono <- ggplot(age30_mono, aes(x = reorder(Language, -mean_rating), y = mean_rating, fill = participant_group)) +
  stat_summary(fun = "mean", geom = "bar")  + 
  stat_summary(fun = "mean", geom = "text", aes(label = round(..y.., 2)), position = position_stack(vjust = 0.5), color = "black", size = 5) +
  labs(title = "Monolingual: Credibility Ratings by Language for participant_age 30",
       x = "Language",
       y = "Mean Rating",
       fill = "Language Group") +
  theme_minimal() +
  scale_fill_manual(name = "Language Group", values = c("blue"), labels = "Monolingual") + 
  scale_y_continuous(limits = c(0, 4), breaks = 1:4,
                     labels = c("1: Certain False", "2: Think False", "3: Think Right", "4: Certain Right")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10)) +
  coord_cartesian(clip = "off")

# Graph 3: Bilingual-only bar chart with customized labels and title
plot_multi <- ggplot(age30_multi, aes(x = reorder(Language, -mean_rating), y = mean_rating, fill = participant_group)) +
  stat_summary(fun = "mean", geom = "bar")  + 
  stat_summary(fun = "mean", geom = "text", aes(label = round(..y.., 2)), position = position_stack(vjust = 0.5), color = "black", size = 5) +
  labs(title = "Multilingual: Credibility Ratings by Language for participant_age 30",
       x = "Language",
       y = "Mean Rating",
       fill = "Language Group") +
  theme_minimal() +
  scale_fill_manual(name = "Language Group", values = c("red"), labels = "Multilingual") + 
  scale_y_continuous(limits = c(0, 4), breaks = 1:4,
                     labels = c("1: Certain False", "2: Think False", "3: Think Right", "4: Certain Right")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10)) +
  coord_cartesian(clip = "off")

# Combine all three plots
(plot_combined / (plot_multi | plot_mono)) + 
  plot_layout(heights = c(2, 1))  # Adjust heights to balance the visual layout

############### AGE 33 ##########################################


# Filter data for monolingual and bilingual groups
age33_mono <- age33 %>% filter(participant_group == "Mono")
age33_multi <- age33 %>% filter(participant_group == "Bi")

# Graph 1: Combined bar chart
plot_combined <- ggplot(age33, aes(x = reorder(Language, -mean_rating), y = mean_rating, fill = participant_group)) +
  stat_summary(fun = "mean", geom = "bar")  + 
  stat_summary(fun = "mean", geom = "text", aes(label = round(..y.., 2)), position = position_stack(vjust = 0.5), color = "black", size = 5) +
  labs(title = "Combined: Credibility Ratings by Language for participant_age 33",
       x = "Language",
       y = "Mean Rating",
       fill = "Language Group") +
  theme_minimal() +
  scale_fill_manual(name = "Language Group", 
                    labels = c("Bilingual", "Monolingual"), 
                    values = c("red", "blue")) +
  scale_y_continuous(limits = c(0, 4), breaks = 1:4,
                     labels = c("1: Certain False", "2: Think False", "3: Think Right", "4: Certain Right")) 

# Graph 2: Monolingual-only bar chart with customized labels and title
plot_mono <- ggplot(age33_mono, aes(x = reorder(Language, -mean_rating), y = mean_rating, fill = participant_group)) +
  stat_summary(fun = "mean", geom = "bar")  + 
  stat_summary(fun = "mean", geom = "text", aes(label = round(..y.., 2)), position = position_stack(vjust = 0.5), color = "black", size = 5) +
  labs(title = "Monolingual: Credibility Ratings by Language for participant_age 33",
       x = "Language",
       y = "Mean Rating",
       fill = "Language Group") +
  theme_minimal() +
  scale_fill_manual(name = "Language Group", values = c("blue"), labels = "Monolingual") + 
  scale_y_continuous(limits = c(0, 4), breaks = 1:4,
                     labels = c("1: Certain False", "2: Think False", "3: Think Right", "4: Certain Right")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10)) +
  coord_cartesian(clip = "off")

# Graph 3: Bilingual-only bar chart with customized labels and title
plot_multi <- ggplot(age33_multi, aes(x = reorder(Language, -mean_rating), y = mean_rating, fill = participant_group)) +
  stat_summary(fun = "mean", geom = "bar")  + 
  stat_summary(fun = "mean", geom = "text", aes(label = round(..y.., 2)), position = position_stack(vjust = 0.5), color = "black", size = 5) +
  labs(title = "Multilingual: Credibility Ratings by Language for participant_age 33",
       x = "Language",
       y = "Mean Rating",
       fill = "Language Group") +
  theme_minimal() +
  scale_fill_manual(name = "Language Group", values = c("red"), labels = "Multilingual") + 
  scale_y_continuous(limits = c(0, 4), breaks = 1:4,
                     labels = c("1: Certain False", "2: Think False", "3: Think Right", "4: Certain Right")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10)) +
  coord_cartesian(clip = "off")

# Combine all three plots
(plot_combined / (plot_multi | plot_mono)) + 
  plot_layout(heights = c(2, 1))  # Adjust heights to balance the visual layout


############### AGE 36 ##########################################


# Filter data for monolingual and bilingual groups
age36_mono <- age36 %>% filter(participant_group == "Mono")
age36_multi <- age36 %>% filter(participant_group == "Bi")

# Graph 1: Combined bar chart
plot_combined <- ggplot(age36, aes(x = reorder(Language, -mean_rating), y = mean_rating, fill = participant_group)) +
  stat_summary(fun = "mean", geom = "bar")  + 
  stat_summary(fun = "mean", geom = "text", aes(label = round(..y.., 2)), position = position_stack(vjust = 0.5), color = "black", size = 5) +
  labs(title = "Combined: Credibility Ratings by Language for participant_age 36",
       x = "Language",
       y = "Mean Rating",
       fill = "Language Group") +
  theme_minimal() +
  scale_fill_manual(name = "Language Group", 
                    labels = c("Bilingual", "Monolingual"), 
                    values = c("red", "blue")) +
  scale_y_continuous(limits = c(0, 4), breaks = 1:4,
                     labels = c("1: Certain False", "2: Think False", "3: Think Right", "4: Certain Right")) 

# Graph 2: Monolingual-only bar chart with customized labels and title
plot_mono <- ggplot(age36_mono, aes(x = reorder(Language, -mean_rating), y = mean_rating, fill = participant_group)) +
  stat_summary(fun = "mean", geom = "bar")  + 
  stat_summary(fun = "mean", geom = "text", aes(label = round(..y.., 2)), position = position_stack(vjust = 0.5), color = "black", size = 5) +
  labs(title = "Monolingual: Credibility Ratings by Language for participant_age 36",
       x = "Language",
       y = "Mean Rating",
       fill = "Language Group") +
  theme_minimal() +
  scale_fill_manual(name = "Language Group", values = c("blue"), labels = "Monolingual") + 
  scale_y_continuous(limits = c(0, 4), breaks = 1:4,
                     labels = c("1: Certain False", "2: Think False", "3: Think Right", "4: Certain Right")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10)) +
  coord_cartesian(clip = "off")

# Graph 3: Bilingual-only bar chart with customized labels and title
plot_multi <- ggplot(age36_multi, aes(x = reorder(Language, -mean_rating), y = mean_rating, fill = participant_group)) +
  stat_summary(fun = "mean", geom = "bar")  + 
  stat_summary(fun = "mean", geom = "text", aes(label = round(..y.., 2)), position = position_stack(vjust = 0.5), color = "black", size = 5) +
  labs(title = "Multilingual: Credibility Ratings by Language for participant_age 36",
       x = "Language",
       y = "Mean Rating",
       fill = "Language Group") +
  theme_minimal() +
  scale_fill_manual(name = "Language Group", values = c("red"), labels = "Multilingual") + 
  scale_y_continuous(limits = c(0, 4), breaks = 1:4,
                     labels = c("1: Certain False", "2: Think False", "3: Think Right", "4: Certain Right")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10)) +
  coord_cartesian(clip = "off")

# Combine all three plots
(plot_combined / (plot_multi | plot_mono))+ 
  plot_layout(heights = c(2, 1))  # Adjust heights to balance the visual layout


############### AGE 40 ##########################################


# Filter data for monolingual and bilingual groups
age40_mono <- age40 %>% filter(participant_group == "Mono")
age40_multi <- age40 %>% filter(participant_group == "Bi")

# Graph 1: Combined bar chart
plot_combined <- ggplot(age40, aes(x = reorder(Language, -mean_rating), y = mean_rating, fill = participant_group)) +
  stat_summary(fun = "mean", geom = "bar")  + 
  stat_summary(fun = "mean", geom = "text", aes(label = round(..y.., 2)), position = position_stack(vjust = 0.5), color = "black", size = 5) +
  labs(title = "Combined: Credibility Ratings by Language for participant_age 40",
       x = "Language",
       y = "Mean Rating",
       fill = "Language Group") +
  theme_minimal() +
  scale_fill_manual(name = "Language Group", 
                    labels = c("Bilingual", "Monolingual"), 
                    values = c("red", "blue")) +
  scale_y_continuous(limits = c(0, 4), breaks = 1:4,
                     labels = c("1: Certain False", "2: Think False", "3: Think Right", "4: Certain Right")) 

# Graph 2: Monolingual-only bar chart with customized labels and title
plot_mono <- ggplot(age40_mono, aes(x = reorder(Language, -mean_rating), y = mean_rating, fill = participant_group)) +
  stat_summary(fun = "mean", geom = "bar")  + 
  stat_summary(fun = "mean", geom = "text", aes(label = round(..y.., 2)), position = position_stack(vjust = 0.5), color = "black", size = 5) +
  labs(title = "Monolingual: Credibility Ratings by Language for participant_age 40",
       x = "Language",
       y = "Mean Rating",
       fill = "Language Group") +
  theme_minimal() +
  scale_fill_manual(name = "Language Group", values = c("blue"), labels = "Monolingual") + 
  scale_y_continuous(limits = c(0, 4), breaks = 1:4,
                     labels = c("1: Certain False", "2: Think False", "3: Think Right", "4: Certain Right")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10)) +
  coord_cartesian(clip = "off")

# Graph 3: Bilingual-only bar chart with customized labels and title
plot_multi <- ggplot(age40_multi, aes(x = reorder(Language, -mean_rating), y = mean_rating, fill = participant_group)) +
  stat_summary(fun = "mean", geom = "bar")  + 
  stat_summary(fun = "mean", geom = "text", aes(label = round(..y.., 2)), position = position_stack(vjust = 0.5), color = "black", size = 5) +
  labs(title = "Multilingual: Credibility Ratings by Language for participant_age 40",
       x = "Language",
       y = "Mean Rating",
       fill = "Language Group") +
  theme_minimal() +
  scale_fill_manual(name = "Language Group", values = c("red"), labels = "Multilingual") + 
  scale_y_continuous(limits = c(0, 4), breaks = 1:4,
                     labels = c("1: Certain False", "2: Think False", "3: Think Right", "4: Certain Right")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10)) +
  coord_cartesian(clip = "off")

# Combine all three plots
(plot_combined / (plot_multi | plot_mono)) + 
  plot_layout(heights = c(2, 1))  # Adjust heights to balance the visual layout


############# Gender on Gender ##############################


ggplot(mean_dat, aes(x = speaker_sex, y = mean_rating, fill = factor(speaker_sex))) +
  stat_summary(fun = "mean", geom = "bar")  + 
  stat_summary(fun = "mean", geom = "text", aes(label = round(..y.., 2)), position = position_stack(vjust = 0.5), color = "black", size = 5) +
  
  labs(title = "Credibility Ratings of Each Language by Gender",
       x = "Speaker Gender",
       y = "Credibility Rating",
       fill = "Speaker Gender") +
  theme(axis.text.x = element_blank(), axis.text.y = element_text(size=15)) +
  scale_fill_manual(values = c("red4", "dodgerblue4")) + 
  facet_grid(~participant_sex) + 
  scale_y_continuous(limits= c(0,4), breaks = 1:4,  # Specify the y-axis positions
                     labels = c("1: I am certain this is false", "2: I think this is false", "3: I think this is right", "4: I am certain this is right"))





