# Illustrates data from first individual with and without modified data

library(tidyverse)
library(lme4)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(lattice)
library(patchwork)

# Clear working environment and set seed.
rm(list = ls())
load("simulated_data.RData")

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

#
joint_lmm <- lmer(FEV1 ~ Setting + Age + Setting * Age + (-1 + as.factor(Setting) + Age:as.factor(Setting) | ID), data = df)

df_prel <- df %>% slice(-9)
joint_lmm_2 <- lmer(FEV1 ~ Setting + Age + Setting * Age + (-1 + as.factor(Setting) + Age:as.factor(Setting) | ID), data = df_prel)

predict(joint_lmm_2)[1:8]
predict(joint_lmm)[1:9]
df$FEV1[1:9]
(df %>% filter(ID == 1))$Age[9]
new_data <- data.frame(ID = 1, Setting = 2, Measurement_nr = 4, Age = 17.11730)
predict(joint_lmm_2, newdata = new_data)
pred <- c(predict(joint_lmm_2)[1:8], predict(joint_lmm_2, newdata = new_data))

(df %>% slice(9))$Age

age1 <- (df %>% filter(ID == 1, Setting == 1))$Age
age2 <- (df %>% filter(ID == 1, Setting == 2))$Age
age1and2 <- c(age1, age2)
age1and2

# fixed effects, population average trend
fixed <- fixef(joint_lmm)
fixed
fixed2 <- fixef(joint_lmm_2)

ggplot(data = df %>% filter(ID == 1), aes(x = Age, y = FEV1, color = as.factor(Setting))) + geom_point(size = 2) +
  geom_line(data = df %>% filter(ID == 1, Setting == 1), aes(x = age1, y = (predict(joint_lmm)[1:5]) ), color = "blue") +
  geom_line(data = df %>% filter(ID == 1, Setting == 2), aes(x = age2, y = (predict(joint_lmm)[6:9]) ), color = "red") +
  geom_line(data = df %>% filter(ID == 1, Setting == 2), aes(x = age2, y = pred[6:9]), color = "green") +
  #geom_line(aes(x = age1and2, y = fixed[1] + age1and2*fixed[3]), color = "black") + # fixed effect line, home
  geom_line(aes(x = age1and2, y = fixed2[1] + age1and2*fixed2[3]), color = "black", linetype=2) + # fixed effect line on modified data, home
  #geom_line(aes(x = age1and2, y = (fixed[1]+fixed[2]) + age1and2*(fixed[3]+fixed[4])), color = "black") + ## fixed effect line, hospital
  geom_line(aes(x = age1and2, y = (fixed2[1]+fixed2[2]) + age1and2*(fixed2[3]+fixed2[4])), color = "black", linetype=2) + # fixed effect line, hospital
  labs(title = "Data visualization of the first individual", color = "Setting", subtitle = "Dashed black lines are the fixed effect line's of home and hospital", 
       caption = "Blue line: prediction of home spirometry, Read line: prediction of hospital spirometry, Green line: prediction of hospital but model is trained on modified data")
