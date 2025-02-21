library(readxl)
library(multcomp)
library(readr)
library(dplyr)
library(tidyverse)
library(readxl)
library(ggplot2)
library(reshape2)
library(writexl)
library(MASS)
library(car)
library(lattice)
library(lmerTest)
library(effects)
library(MuMIn)
library(RVAideMemoire)
library(tidyverse)
library(dplyr)
library(lme4)
library(nlme)
library(emmeans)


pilot_data <- read_excel("pilot_stats.xlsx") #file with only rating values, age, gender, and prolific ID 

############################################################ The 7 Languages ############################################################ 
boxcox_plang <- boxcox(mean_rating ~ Language, data = pilot_data) 
boxcox_plang$x[which.max(boxcox_plang$y)] # = 0.26

boxcox_plang2 <- boxcox(mean_rating ~ Language * Gender + Age, data = pilot_data) 
boxcox_plang2$x[which.max(boxcox_plang2$y)] # = 0.3


plme_lang <- lmer(mean_rating ~ Language + (1 | List), data = pilot_data)
plme_lang2 <- lmer(mean_rating ~ Language * Gender + Age + (1 | List), data = pilot_data)

AIC(plme_lang)
AIC(plme_lang2) #
BIC(plme_lang) #
BIC(plme_lang2)
summary(plme_lang)
summary(plme_lang2) #


paov_lang <- aov(mean_rating ~ Language, data = pilot_data)
summary(paov_lang)

# Df Sum Sq Mean Sq F value Pr(>F)    
# Language      6 239.80   39.97   118.6 <2e-16 ***
#   Residuals   273  92.02    0.34                   

pph_lang <- glht(paov_lang , linfct = mcp(Language= "Tukey"))





ggplot(pilot_data, aes(x=Language, y= mean_rating, fill = factor(Language))) +
  stat_summary(fun = "mean", geom = "bar")  + 
  labs(title = "Difficulty in Understanding Each Language",
       x = "Language",
       y = "Difficulty Rating",
       fill = "Language") +
  theme(axis.text.x = element_blank(), axis.text.y = element_text(size=15)) +
  scale_fill_manual(values = c("yellow", "red4", "red", "dodgerblue4", "deepskyblue", "green4", "limegreen")) +
  scale_y_continuous(limits= c(0,5), breaks = 1:5,  # Specify the y-axis positions
                     labels = c("1: Extremely Easy", " 2: Somewhat Easy", " 3: Neutral", "4: Somewhat Difficult", "5: Extremely Difficult"))


############################################################ Accent Strength ############################################################ 

paov_acc <- aov(mean_rating ~ AccentStrength, data = pilot_data)
summary(paov_acc )

pph_acc <- glht(paov_acc , linfct = mcp(AccentStrength= "Tukey"))
summary(pph_acc)

boxcox_acc <- boxcox(mean_rating ~ AccentStrength, data = pilot_data) 
boxcox_acc $x[which.max(boxcox_acc$y)] #0.1
plme_acc <- lmer(log(mean_rating) ~ AccentStrength+ (1 | List), data = pilot_data)

boxcox_acc2 <- boxcox(mean_rating ~ AccentStrength * Gender + Age, data = pilot_data) 
boxcox_acc2$x[which.max(boxcox_acc2$y)] # = 0.6

plme_acc2 <- lmer(sqrt(mean_rating) ~ AccentStrength * Gender + Age + (1 | List), data = pilot_data)


#Lower is better for each!
AIC(plme_acc)
AIC(plme_acc2) #better 

BIC(plme_acc) #better
BIC(plme_acc2)

summary(plme_acc)
summary(plme_acc2) #better

ggplot(pilot_data, aes(x = AccentStrength, y = mean_rating, fill = factor(AccentStrength))) +
  stat_summary(fun = "mean", geom = "bar")  + 
  labs(title = "Credibility Ratings by Language",
       x = "Language",
       y = "Credibility Rating",
       fill = "Language") +
  theme(axis.text.x = element_blank(), axis.text.y = element_text(size=15)) +
  scale_fill_manual(values = c("yellow", "red4", "red", "dodgerblue4", "deepskyblue", "green4", "limegreen")) + 
  scale_y_continuous(limits= c(0,3), breaks = 1:4,  # Specify the y-axis positions
                     labels = c("1: I am certain this is false", "2: I think this is false", "3: I think this is right", "4: I am certain this is right"))



############################################################ Base Language ############################################################ 

paov_base <- aov(mean_rating ~ BaseLanguage, data = pilot_data)
summary(paov_base)

pph_base <- glht(paov_base, linfct = mcp(BaseLanguage= "Tukey"))
summary(pph_base )

boxcox_base <- boxcox(mean_rating ~ BaseLanguage, data = pilot_data) 
boxcox_base $x[which.max(boxcox_base $y)]
plme_base <- lmer((mean_rating)^-1 ~ BaseLanguage+ (1 | List), data = pilot_data)

boxcox_base2 <- boxcox(mean_rating ~ BaseLanguage* Gender + Age, data = pilot_data) 
boxcox_base2$x[which.max(boxcox_base2$y)] # = 0.3
plme_base2 <- lmer((mean_rating)^-1 ~ BaseLanguage* Gender + Age + (1 | List), data = pilot_data)


#Lower is better for each!
AIC(plme_base)
AIC(plme_base2) #better

BIC(plme_base) #better
BIC(plme_base2)

summary(plme_base) #better
summary(plme_base2)

ggplot(pilot_data, aes(x=BaseLanguage, y= mean_rating, fill = factor(BaseLanguage))) +
  stat_summary(fun = "mean", geom = "bar")  + 
  labs(title = "Difficulty in Understanding Each Language",
       x = "Language",
       y = "Difficulty Rating",
       fill = "Language") +
  theme(axis.text.x = element_blank(), axis.text.y = element_text(size=15)) +
  scale_fill_manual(values = c("yellow", "red4", "red", "dodgerblue4", "deepskyblue", "green4", "limegreen")) +
  scale_y_continuous(limits= c(0,5), breaks = 1:5,  # Specify the y-axis positions
                     labels = c("1: Extremely Easy", " 2: Somewhat Easy", " 3: Neutral", "4: Somewhat Difficult", "5: Extremely Difficult"))
