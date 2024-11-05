# This file compares simple linear regression and univariate mixed effect and
# joint mixed effect models. 

library(tidyverse)
library(readr)
library(MASS)
library(Matrix)
library(spatstat.random)
library(Matrix)
library(corrplot)
library(lme4)
library(dplyr)
rm(list = ls())
source("C:\\Users\\hallb\\R_projects\\mixed models examples\\simulate_data.R")

# Variables.
xmin <- 7
xmax <- 8
sigma_beta0 <- 1
sigma_beta1 <- 1
rho_home <- -0.5
sigma_beta0_hosp <- 1
sigma_beta1_hosp <- 1
rho_hosp <- -0.5
N <- 100
m <- 8
n_hosp <- 4
varhomeb0 <- 1
varhospb0 <- 1
varhomeb1 <- 1
varhospb1 <- 1
cov_homeb0hospb0 <- 0.5
cov_homeb0homeb1 <- -0.5
cov_homeb0hospb1 <- -0.5
cov_hospb0homeb1 <- -0.5
cov_hospb0hospb1 <- -0.5
cov_homeb1hospb1 <- 0.5

data <- simulate_data(
  xmin,
  xmax,
  sigma_beta0,
  sigma_beta1,
  rho_home,
  sigma_beta0_hosp,
  sigma_beta1_hosp,
  rho_hosp,
  N,
  m,
  n_hosp,
  varhomeb0,
  varhospb0, 
  varhomeb1,
  varhospb1,
  cov_homeb0hospb0,
  cov_homeb0homeb1,
  cov_homeb0hospb1,
  cov_hospb0homeb1,
  cov_hospb0hospb1,
  cov_homeb1hospb1)
df <- data$df
beta_home_true <- data$beta_home_true
beta_hosp_true <- data$beta_hosp_true
b_true <-data$b_true

# Joint model
joint_lmm <- lmer(FEV1 ~ Setting + Age + Setting * Age + (-1 + as.factor(Setting) + Age:as.factor(Setting) | ID), data = df)
fixef(joint_lmm)
fixedeffects <- tibble(fixef(joint_lmm))
randomeffects <- tibble(ranef(joint_lmm)$ID)

###### Linear regression, LR
# LR, fixed effects
lr_fixed_home <- lm(FEV1 ~ Age, data = df %>% filter(Setting == 1))
lr_fixed_hosp <- lm(FEV1 ~ Age, data = df %>% filter(Setting == 2))

# LR random effects
lr_parameters <- tibble(
  lr_b0home = c(),
  lr_b0hosp = c(),
  lr_b1home = c(),
  lr_b1hosp = c()
)

for (i in 1:N) {
 df_home <- lm(FEV1 ~ Age, data = df %>% filter(ID == i, Setting == 1))
 df_hosp <- lm(FEV1 ~ Age, data = df %>% filter(ID == i, Setting == 2))
 row_data <- tibble(
   lr_b0home = df_home$coefficients[1] - beta_home_true[1],
   lr_b0hosp = df_hosp$coefficients[1] - beta_hosp_true[1],
   lr_b1home = df_home$coefficients[2],
   lr_b1hosp = df_hosp$coefficients[2]
 )
 lr_parameters <- bind_rows(lr_parameters, row_data)
}

#### Univariate mixed effect models, treat home and hospital spirometry seperate.
lmm_home <- lmer(FEV1 ~ Age + (1 + Age | ID), data = df %>% filter(Setting == 1))
lmm_hosp <- lmer(FEV1 ~ Age + (1 + Age | ID), data = df %>% filter(Setting == 2))


# b0_home - b0_hosp - b1_home - b1_hosp
### Results
# LR compared to true values
lrMSEFixed <- c(
  mean((beta_home_true[1] - lr_fixed_home$coefficients[1])^2), 
  mean((beta_hosp_true[1] - lr_fixed_hosp$coefficients[1])^2),
  mean((beta_home_true[2] - lr_fixed_home$coefficients[2])^2),
  mean((beta_hosp_true[2] - lr_fixed_hosp$coefficients[2])^2))
lrMSERandom <- c(
  mean((b_true[,1] - lr_parameters$lr_b0home)^2),
  mean((b_true[,2] - lr_parameters$lr_b0hosp)^2),
  mean((b_true[,3] - lr_parameters$lr_b1home)^2),
  mean((b_true[,4] - lr_parameters$lr_b1hosp)^2)
)
LResults <- tibble(fixedEffects = lrMSEFixed, randomeffects = lrMSERandom)

# Univariate compared to true values
univariateMSEFixed <- c(
  mean((beta_home_true[1] - fixef(lmm_home)[1])^2), 
  mean((beta_hosp_true[1] - fixef(lmm_hosp)[1])^2),
  mean((beta_home_true[2] - fixef(lmm_home)[2])^2),
  mean((beta_hosp_true[2] - fixef(lmm_hosp)[2])^2))
univariateMSERandom <- c(
  mean((b_true[,1] - ranef(lmm_home)$ID[,1])^2),
  mean((b_true[,2] - ranef(lmm_hosp)$ID[,1])^2),
  mean((b_true[,3] - ranef(lmm_home)$ID[,2])^2),
  mean((b_true[,4] - ranef(lmm_hosp)$ID[,2])^2)
)
univariateResults <- tibble(fixedEffects = univariateMSEFixed, randomeffects = univariateMSERandom)

# Joint mixed-effect model
jointMSEFixed <- c(
  mean((beta_home_true[1] - fixef(joint_lmm)[1])^2), 
  mean((beta_hosp_true[1] - fixef(joint_lmm)[2])^2),
  mean((beta_home_true[2] - fixef(joint_lmm)[3])^2),
  mean((beta_hosp_true[2] - fixef(joint_lmm)[4])^2))
jointMSERandom <- c(
  mean((b_true[,1] - ranef(joint_lmm)$ID[,1])^2),
  mean((b_true[,2] - ranef(joint_lmm)$ID[,1])^2),
  mean((b_true[,3] - ranef(joint_lmm)$ID[,2])^2),
  mean((b_true[,4] - ranef(joint_lmm)$ID[,2])^2)
)
jointResults <- tibble(fixedEffects = jointMSEFixed, randomeffects = jointMSERandom)


print(LResults)
print(univariateResults)
print(jointResults)
