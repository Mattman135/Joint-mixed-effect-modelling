# This file has the aim to answer the question: Can observations in one of the measurements, hospital spirometry, be replaced
# by additional measurements in the home measurements without loss of accuracy?

library(tidyverse)
library(lme4)
library(dplyr)
rm(list = ls())

source("C:\\Users\\hallb\\R_projects\\mixed models examples\\simulate_data_Analys1.R")
source("C:\\Users\\hallb\\R_projects\\mixed models examples\\performe_LOOCV.R")

df1 <- simulate_data(8, 4)
df2 <- simulate_data(9, 3)
df3 <- simulate_data(10, 2)

## Perform LOOCV analysis
results1 <- performe_LOOCV(df1)
results2 <- performe_LOOCV(df2)
results3 <- performe_LOOCV(df3)

#### Quick analysis using resid and plot.
joint_lmm1 <- lmer(FEV1 ~ Setting + Age + Setting * Age + (-1 + as.factor(Setting) + Age:as.factor(Setting) | ID), data = df1)
joint_lmm2 <- lmer(FEV1 ~ Setting + Age + Setting * Age + (-1 + as.factor(Setting) + Age:as.factor(Setting) | ID), data = df2)
joint_lmm3 <- lmer(FEV1 ~ Setting + Age + Setting * Age + (-1 + as.factor(Setting) + Age:as.factor(Setting) | ID), data = df3)

plot(resid(joint_lmm1))
plot(resid(joint_lmm2))
plot(resid(joint_lmm3))

sqrt(sum(resid(joint_lmm1)^2)/100)
sqrt(sum(resid(joint_lmm2)^2)/100)
sqrt(sum(resid(joint_lmm3)^2)/100)