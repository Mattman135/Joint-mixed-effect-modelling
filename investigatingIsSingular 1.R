# How many is singular?  N <- seq(50, 500, by = 50). compare joint and univariate.
# last 1, 3 and 5 years
rm(list = ls())
library(tidyverse)
library(corrplot)
library(lme4)
library(dplyr)
library(tidyr)
library(gridExtra)
library(ggplot2)
library(ggpubr)

rm(list = ls())
source("C:\\Users\\hallb\\R_projects\\mixed models examples\\2.0\\SIMULATE DATA.R")

# FUNCYIONS
sim_data <- function(N, years_projection) {
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
  #years_projection <- 3
  
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


num_simulations <- 1000
N <- seq(50, 500, by = 50)

#####
result <- tibble(
  count1_joint = rep(0, length(N)),
  count3_joint = rep(0, length(N)),
  count5_joint = rep(0, length(N)),
  count1_uni   = rep(0, length(N)),
  count3_uni   = rep(0, length(N)),
  count5_uni   = rep(0, length(N)),
)

for (j in 1:length(N)) {
  for (i in 1:num_simulations) {
    print(paste(j, "and ", i))
    meta_data1 <- sim_data(N[j], 1)
    meta_data3 <- sim_data(N[j], 3)
    meta_data5 <- sim_data(N[j], 5)
    
    # 1 year
    if(isSingular(lmer(FEV1 ~ -1 + as.factor(Setting) + as.factor(Setting):Age + (-1 + as.factor(Setting) + as.factor(Setting):Age | ID), data = meta_data1$df))) {
      result[j, 1] <- result[j, 1] + 1
    }
    if(isSingular(lmer(FEV1 ~ Age + (1 + Age | ID), data = meta_data1$df))) {
      result[j, 4] <- result[j, 4] + 1
    }
    
    # 3 year
    if(isSingular(lmer(FEV1 ~ -1 + as.factor(Setting) + as.factor(Setting):Age + (-1 + as.factor(Setting) + as.factor(Setting):Age | ID), data = meta_data3$df))) {
      result[j, 2] <- result[j, 2] + 1
    }
    if(isSingular(lmer(FEV1 ~ Age + (1 + Age | ID), data = meta_data3$df))) {
      result[j, 5] <- result[j, 5] + 1
    }
    
    # 5 year
    if(isSingular(lmer(FEV1 ~ -1 + as.factor(Setting) + as.factor(Setting):Age + (-1 + as.factor(Setting) + as.factor(Setting):Age | ID), data = meta_data5$df))) {
      result[j, 3] <- result[j, 3] + 1
    }
    if(isSingular(lmer(FEV1 ~ Age + (1 + Age | ID), data = meta_data5$df))) {
      result[j, 6] <- result[j, 6] + 1
    }
  }
}

save(result, file="C:\\Users\\hallb\\R_projects\\mixed models examples\\2.0\\investigatingIsSingular\\inv. 1, 1K simulations, 16 mars.RData")
######

#PLotting
#####


load("C:\\Users\\hallb\\R_projects\\mixed models examples\\2.0\\investigatingIsSingular\\inv. 1, 1K simulations, 16 mars.RData")
result$N <- seq(50, 500, by = 50)

new_res <- cbind(
  "model" = c(
    rep(1, 10),
    rep(2, 10),
    rep(3, 10),
    rep(4, 10),
    rep(5, 10),
    rep(6, 10)
  ),
  "count" = c(
    result$count1_joint, 
    result$count3_joint, 
    result$count5_joint,
    result$count1_uni,
    result$count3_uni,
    result$count5_uni
  ),
  N = rep(seq(50, 500, by = 50), 6)
)


ggplot(new_res, aes(x = N, y = count / 10, group = as.factor(model), color = as.factor(model))) + 
  geom_line(linewidth=.7) + 
  labs(x = "N", y = "%") + 
  theme_pubr() + 
  scale_color_discrete(name = "", labels = c("1" = "Joint 1 year", "2" = "Joint 3 year", "3" = "Joint 5 year",
                                                       "4" = "Uni. 1 year", "5" = "Uni. 3 year", "6" = "Uni. 5 year")) 
