# How many singular for joint for lower values of N?
# is singular N <- c(10, 25, 50, 100) for last year
# compare joint and univariate

rm(list = ls())
library(tidyverse)
library(corrplot)
library(lme4)
library(dplyr)
library(tidyr)
library(gridExtra)
library(ggplot2)
library(ggpubr)

source("C:\\Users\\hallb\\R_projects\\mixed models examples\\2.0\\SIMULATE DATA.R")

# FUNCTIONS
sim_data <- function(N) {
  # default settings
  xmin <- 7
  xmax <- 18
  sigma_beta0_home <- 1 
  sigma_beta1_home <- 1
  sigma_beta0_hosp <- 1
  sigma_beta1_hosp <- 1
  #N <- 100
  n_home <- 12
  n_hosp <- 4
  residual_error <- 1
  varhomeb0 <- 1
  varhospb0 <- 1 
  varhomeb1 <- 1
  varhospb1 <- 1
  beta_cov_homeb0hospb0 <- 0.5
  beta_cov_homeb0homeb1 <- -0.5
  beta_cov_homeb0hospb1 <- 0
  beta_cov_hospb0homeb1 <- 0
  beta_cov_hospb0hospb1 <- -0.5
  beta_cov_homeb1hospb1 <- 0.5
  cov_homeb0hospb0 <- 0.5
  cov_homeb0homeb1 <- -0.5
  cov_homeb0hospb1 <- 0
  cov_hospb0homeb1 <- 0
  cov_hospb0hospb1 <- -0.5
  cov_homeb1hospb1 <- 0.5
  years_projection <- 1
  
  meta_data <- simulate_data(
    xmin,
    xmax,
    sigma_beta0_home, 
    sigma_beta1_home,
    sigma_beta0_hosp,
    sigma_beta1_hosp,
    N,
    n_home,
    n_hosp,
    residual_error,
    varhomeb0,
    varhospb0, 
    varhomeb1,
    varhospb1,
    beta_cov_homeb0hospb0,
    beta_cov_homeb0homeb1,
    beta_cov_homeb0hospb1,
    beta_cov_hospb0homeb1,
    beta_cov_hospb0hospb1,
    beta_cov_homeb1hospb1,
    cov_homeb0hospb0,
    cov_homeb0homeb1,
    cov_homeb0hospb1,
    cov_hospb0homeb1,
    cov_hospb0hospb1,
    cov_homeb1hospb1,
    years_projection
  )
  return (meta_data)
}

# 
#####
num_simulations <- 1000
N <- c(10, 25, 50, 75, 100)

result <- tibble(
  count_joint = rep(0, length(N)),
  count_uni = rep(0, length(N)),
  N = N
)

for (j in 1:length(N)) {
  for (i in 1:num_simulations) {
    meta_data <- sim_data(N[j])
    if(isSingular(lmer(FEV1 ~ -1 + as.factor(Setting) + as.factor(Setting):Age + (-1 + as.factor(Setting) + Age:as.factor(Setting) | ID), data = meta_data$df))) {
      result[j, 1] <- result[j, 1] + 1
    }
    
    if(isSingular(lmer(FEV1 ~ Age + (1 + Age | ID), data = meta_data$df))) {
      result[j, 2] <- result[j, 2] + 1
    }

    
  }
}

save(result, file="C:\\Users\\hallb\\R_projects\\mixed models examples\\2.0\\investigatingIsSingular\\Investigating isSing 3 29 mars.RData")
#####





#####
load("C:\\Users\\hallb\\R_projects\\mixed models examples\\2.0\\investigatingIsSingular\\Investigating isSing 3 29 mars.RData")

new_result <- tibble(
  count = c(result$count_joint, result$count_uni),
  nhome_nhosp = rep(c(10, 25, 50, 75, 100), 2),
  model = c(rep(0, length(c(10, 25, 50, 75, 100))), rep(1, length(c(10, 25, 50, 75, 100))))
)

ggplot(new_result, aes(x = nhome_nhosp, y = count / num_simulations*100, group = model, color = as.factor(model))) +
  geom_line(linewidth=.7) + 
  labs(x = "N", y = "%") + 
  scale_x_continuous(breaks = c(10, 25, 50, 75, 100)) + 
  scale_color_discrete(name = "", labels = c("0" = "Joint model", "1" = "Univariate model")) +
  theme_pubr()

