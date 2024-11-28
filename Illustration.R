# Illustrates data from first individual with and without modified data
# 

library(tidyverse)
library(lme4)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(lattice)
library(patchwork)

# Clear working environment.
rm(list = ls())
load("simulated_data.RData")
df <- simulated_data
#
font_size <- 8 # Font size. May be changed to 8. 
font_family = "sans" # Font family. "sans" (arial) or "serif" (times)
theme_set(theme_classic()) 
theme_update(axis.text = element_text(size = font_size, colour = "black", family = font_family),
             axis.line = element_line(colour = "black", linewidth = 0.2), 
             axis.ticks = element_line(colour = "black", linewidth = 0.2), 
             axis.title.x = element_text(margin = margin(t = 0.25, r = 0, b = 0, l = 0, unit = 'cm')),
             axis.title.y = element_text(margin = margin(t = 0, r = 0.25, b = 0, l = 0, unit = 'cm')),
             legend.background = element_blank(),
             legend.key.width = unit(1, "cm"),
             legend.key.height = unit(0.4, "cm"),
             legend.margin = margin(0, 0, 0, 0.5, "cm"),
             legend.spacing =  unit(0, "cm"),
             legend.position = "bottom",
             legend.text = element_text(size = font_size, colour = "black", family = font_family),
             legend.title = element_text(size = font_size, colour = "black", family = font_family),
             strip.background = element_blank(),
             strip.text = element_blank(),
             panel.border = element_blank(),
             panel.grid = element_blank(),  
             plot.subtitle = element_text(size = font_size, colour = "black", family = font_family, face = "plain", hjust = 0),
             plot.title = element_text(size = font_size, colour = "black", family = font_family, face = "plain", hjust = 0),
             text = element_text(size = font_size, colour = "black", family = font_family))
update_geom_defaults("text", list(size = font_size / ggplot2:::.pt, family = font_family))

# make a copy of df and add indices in order to easily extract
df_copy <- df
df_copy$Index <- c(1:nrow(df))
N_FirstSubject <- nrow(df %>% filter(ID == 1))
n_home <- nrow(df %>% filter(ID == 1, Setting == 1))
n_hosp <- nrow(df %>% filter(ID == 1, Setting == 2))
rowToSlice <- (df_copy %>% filter(ID == 1)) %>% slice(n()) # need the row to extract age and index
rowToSlice
indexToSlice <- rowToSlice$Index
ageToSlice <- rowToSlice$Age
df_prel <- df %>% slice(-indexToSlice)

newdata <- select(rowToSlice, -c(Index, FEV1)) # deletes column: Index and FEV1

# fitting models
joint_lmm_2 <- lmer(FEV1 ~ Setting + Age + Setting * Age + (-1 + as.factor(Setting) + Age:as.factor(Setting) | ID), data = df_prel)
joint_lmm <- lmer(FEV1 ~ Setting + Age + Setting * Age + (-1 + as.factor(Setting) + Age:as.factor(Setting) | ID), data = df)
df_copy$predictions <- predict(joint_lmm)
df_copy$predictions_LOOCV <- append(predict(joint_lmm_2), predict(joint_lmm_2, newdata = newdata), (indexToSlice-1))

# add
df_firstOriginalData <- df %>% filter (ID == 1)
df_firstModifiedData <- df %>% filter (ID == 1) # this will be same as df_firstOriginalData but the predicitons will be different
df_firstOriginalData$predictions <- predict(joint_lmm)[1:N_FirstSubject]
df_firstModifiedData$predictions <- c(predict(joint_lmm_2), predict(joint_lmm_2, newdata = newdata))[1:N_FirstSubject]

# extracting fixed effects
fixedOriginalData <- fixef(joint_lmm)
fixedModifiedData <- fixef(joint_lmm_2)

ggplot(data = df %>% filter(ID == 1), aes(x = Age, y = FEV1, color = as.factor(Setting))) + geom_point(size = 2) + 
  geom_line(data = df_copy %>% filter(ID == 1, Setting == 2), aes(x = Age, y = predictions), color = "blue") +
  geom_line(data = df_copy %>% filter(ID == 1, Setting == 2), aes(x = Age, y = predictions_LOOCV), color = "red") +
  geom_line(aes(x = Age, y = (fixef(joint_lmm)[1]+fixef(joint_lmm)[2] + (fixef(joint_lmm)[4])*Age )), color = "black") +
  geom_line(aes(x = Age, y = (fixef(joint_lmm_2)[1]+fixef(joint_lmm_2)[2] + (fixef(joint_lmm_2)[4])*Age )), color = "black", linetype=2) #+
  #labs(title = "Illustration first subject", color = "Setting", 
       #subtitle = "Lines; Blue: Predictions on hospital data on full data, Red: predictions on hospital LOOCV, 
                          #Black: Fixed effect line on full data, Black dashed: Fixed effect line LOOCV")