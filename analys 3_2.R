# This file compares joint to univariate and slr in a real world scenario
# It uses standard variables N == 100, nhome = 15, nhosp = 4 etc.
# it deletes the last hosp for all individuals and calculates mse for all the last for each df
# 1000 simulations.
# df1 is the full data set, df2 is the full data but all last hospital are removed and df3 is only the last hospital measurements.
# This is to get average results from a real world CF scenario.

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
source("C:\\Users\\hallb\\R_projects\\mixed models examples\\2.0\\analysis 3\\analys 3 Joint CV.R")
source("C:\\Users\\hallb\\R_projects\\mixed models examples\\2.0\\analysis 3\\analys 3 Uni CV.R")
source("C:\\Users\\hallb\\R_projects\\mixed models examples\\2.0\\analysis 3\\analys 3 SLR CV.R")

# Functions
sim_data <- function() {
  # default settings
  xmin <- 7
  xmax <- 18
  sigma_beta0_home <- 1 
  sigma_beta1_home <- 1
  sigma_beta0_hosp <- 1
  sigma_beta1_hosp <- 1
  N <- 100
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
  years_projection <- 3 # how much data in years does each subject provide?
  
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
} # make sure to differentiate between this and simulate data file
modify_data <- function(df1) {
  # this deletes all the last hospital measurements
  
  df2 <- tibble()
  df3 <- tibble()
  
  for (i in unique(df1$ID)) {
    df_home <- df1 %>% filter(ID == i, Setting == 1)
    df_hosp <- df1 %>% filter(ID == i, Setting == 2)
    
    df2 <- rbind(
      df2, 
      df_home,
      df_hosp %>% slice(-n())
    )
    
    df3 <- rbind(
      df3,
      df_hosp %>% slice(n())
    )
  }
  
  
  return (list(df2, df3))
}

#
num_simulations <- 1000
result_joint <- tibble(
  msePRED1 = rep(0, num_simulations),
  msePRED2 = rep(0, num_simulations),
  msePRED3 = rep(0, num_simulations),
  mseEST   = rep(0, num_simulations)
)
result_univariate <- result_joint
result_slr <- result_joint

# simulate results
for (i in 1:num_simulations) {
  print(i)
  #
  meta_data <- sim_data()
  df1 <- meta_data$df
  modified_data <- modify_data(df1)
  df2 <- modified_data[[1]]
  df3 <- modified_data[[2]]
  
  #
  result_joint[i, ]      <- CV_joint(df1, df2, df3)
  result_univariate[i, ] <- CV_univariate(df1, df2, df3)
  result_slr[i, ]        <- CV_slr(df1, df2, df3)
  
}

# save files
save(result_joint, file="C:\\Users\\hallb\\R_projects\\mixed models examples\\2.0\\analysis 3\\result_joint 3 mars.RData")
save(result_univariate, file="C:\\Users\\hallb\\R_projects\\mixed models examples\\2.0\\analysis 3\\result_unvariate 3 mars.RData")
save(result_slr, file="C:\\Users\\hallb\\R_projects\\mixed models examples\\2.0\\analysis 3\\result_slr 3 mars.RData")
#####


#####
#
load("C:\\Users\\hallb\\R_projects\\mixed models examples\\2.0\\analysis 3\\result_joint 3 mars.RData")
load("C:\\Users\\hallb\\R_projects\\mixed models examples\\2.0\\analysis 3\\result_unvariate 3 mars.RData")
load("C:\\Users\\hallb\\R_projects\\mixed models examples\\2.0\\analysis 3\\result_slr 3 mars.RData")

result_joint$factor <- rep(1, nrow(result_joint))
result_univariate$factor <- rep(2, nrow(result_univariate))
result_slr$factor <- rep(3, nrow(result_slr))

result <- rbind(
  result_joint,
  result_univariate
)


#VIOLIN plot
ggplot(result, aes(x = factor(factor), y = msePRED1)) +
  geom_violin(fill = "skyblue") +
  labs(
    x = "",
    y = "RMSE(CV)"
  ) + 
  theme_pubr(base_family = "sans") +
  theme(
    axis.title = element_text(size = 16),       # Axis title size
    axis.text = element_text(size = 16),        # Axis tick label size
    plot.title = element_text(size = 16),
    legend.text = element_text(size = 16)
  ) +
  scale_x_discrete(labels = c("Joint model", "Univariate model"))

ggplot(result, aes(x = factor(factor), y = msePRED2)) +
  geom_violin(fill = "skyblue") +
  labs(
    x = "",
    y = "RMSE(CM)"
  ) + 
  theme_pubr(base_family = "sans") +
  theme(
    axis.title = element_text(size = 16),       # Axis title size
    axis.text = element_text(size = 16),        # Axis tick label size
    plot.title = element_text(size = 16),
    legend.text = element_text(size = 16)
  ) +
  scale_x_discrete(labels = c("Joint model", "Univariate model"))

ggplot(result, aes(x = factor(factor), y = msePRED3)) +
  geom_violin(fill = "skyblue") +
  labs(
    x = "",
    y = "RMSE(SST)"
  ) + 
  theme_pubr(base_family = "sans") +
  theme(
    axis.title = element_text(size = 16),       # Axis title size
    axis.text = element_text(size = 16),        # Axis tick label size
    plot.title = element_text(size = 16),
    legend.text = element_text(size = 16)
  ) +
  scale_x_discrete(labels = c("Joint model", "Univariate model"))

ggplot(result, aes(x = factor(factor), y = mseEST)) +
  geom_violin(fill = "skyblue") +
  labs(
    x = "",
    y = "RMSE(PAT)"
  ) + 
  theme_pubr(base_family = "sans") +
  theme(
    axis.title = element_text(size = 16),       # Axis title size
    axis.text = element_text(size = 16),        # Axis tick label size
    plot.title = element_text(size = 16),
    legend.text = element_text(size = 16)
  ) +
  scale_x_discrete(labels = c("Joint model", "Univariate model"))





# print average values
print("msePRED1")
print(mean(result_joint$msePRED1))
print(mean(result_univariate$msePRED1))
print(mean(result_slr$msePRED1))

print("msePRED2")
print(mean(result_joint$msePRED2))
print(mean(result_univariate$msePRED2))
print(mean(result_slr$msePRED2))

print("msePRED3")
print(mean(result_joint$msePRED3))
print(mean(result_univariate$msePRED3))
print(mean(result_slr$msePRED3))

print("mseEST")
print(mean(result_joint$mseEST))
print(mean(result_univariate$mseEST))
print(mean(result_slr$mseEST))