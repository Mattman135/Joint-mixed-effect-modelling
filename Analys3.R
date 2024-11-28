# This file tries to answer the question: How big is the error in prediction if we remove data?
library(tidyverse)
library(lme4)
library(dplyr)
rm(list = ls())

source("C:\\Users\\hallb\\R_projects\\mixed models examples\\simulate_data_Analys3.R")
source("C:\\Users\\hallb\\R_projects\\mixed models examples\\performe_LOOCV.R")

df1 <- simulate_data(50)
df2 <- simulate_data(100)
df3 <- simulate_data(150)
df4 <- simulate_data(200)
df5 <- simulate_data(250)

results1 <- performe_LOOCV(df1)
results2 <- performe_LOOCV(df2)
results3 <- performe_LOOCV(df3)
results4 <- performe_LOOCV(df4)
results5 <- performe_LOOCV(df5)

joint_lmm1 <- lmer(FEV1 ~ Setting + Age + Setting * Age + (-1 + as.factor(Setting) + Age:as.factor(Setting) | ID), data = df1)
joint_lmm2 <- lmer(FEV1 ~ Setting + Age + Setting * Age + (-1 + as.factor(Setting) + Age:as.factor(Setting) | ID), data = df2)
joint_lmm3 <- lmer(FEV1 ~ Setting + Age + Setting * Age + (-1 + as.factor(Setting) + Age:as.factor(Setting) | ID), data = df3)
joint_lmm4 <- lmer(FEV1 ~ Setting + Age + Setting * Age + (-1 + as.factor(Setting) + Age:as.factor(Setting) | ID), data = df4)
joint_lmm5 <- lmer(FEV1 ~ Setting + Age + Setting * Age + (-1 + as.factor(Setting) + Age:as.factor(Setting) | ID), data = df5)

plot(resid(joint_lmm1))
plot(resid(joint_lmm2))
plot(resid(joint_lmm3))
plot(resid(joint_lmm4))
plot(resid(joint_lmm5))

sqrt(sum(resid(joint_lmm1)^2)/100)
sqrt(sum(resid(joint_lmm2)^2)/100)
sqrt(sum(resid(joint_lmm3)^2)/100)
sqrt(sum(resid(joint_lmm4)^2)/100)
sqrt(sum(resid(joint_lmm5)^2)/100)