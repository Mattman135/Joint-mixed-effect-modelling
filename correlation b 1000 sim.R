# 1000 SIMULATIONS

# note that it is num_simulations per covariance. If I use 1000 simulations its actually 9K simulations 
# if I have covariance <- c(-.9, -.5, seq(0, .95, by = .1))
# note that more correlations on the positive axis is more interesting because negative correlation is not interesting
# in a clinical sense.
# Some important parameters:
# beta <- c(150, 100, -5, -1). beta is fixed meaning we do not simulate
# N = 100, nhome = 15, nhosp = 4, 

rm(list = ls())
library(tidyverse)
library(corrplot)
library(lme4)
library(dplyr)
library(tidyr)
library(gridExtra)
library(ggplot2)
library(ggpubr)

source("C:\\Users\\hallb\\R_projects\\mixed models examples\\2.0\\korrelation simulering\\ny simulate data.R")
source("C:\\Users\\hallb\\R_projects\\mixed models examples\\2.0\\ny cv joint.R")
covariance <- c(-.9, -.5, seq(0, .95, by = .1))



# FUNCTIONS
sim_data <- function(cov_homeb1hospb1) {
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
  #cov_homeb1hospb1 <- 0.5
  years_projection <- 3
  
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
modify_data <- function(df1) {
  
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
} # this deletes all the last hospital measurements in df1 and returns this data and the df with only removed values


######
num_simulations <- 1000
covariance <- c(-.9, -.5, seq(0, .95, by = .1))

# only last deleted results
result_mean <- tibble(
  msePRED1 = rep(0, num_simulations*length(covariance)),
  msePRED2 = rep(0, num_simulations*length(covariance)),
  msePRED3 = rep(0, num_simulations*length(covariance)),
  mseEST   = rep(0, num_simulations*length(covariance))
)
result_sd   <- result_mean

idx <- 1
for (i in 1:num_simulations) {
  for (cov in covariance) {
    print(paste(cov, "and", i))
    meta_data <- sim_data(cov)
    df1 <- meta_data$df
    modified_data <- modify_data(df1)
    df2 <- modified_data[[1]]
    df3 <- modified_data[[2]] # this time the fifth element is only the last measurements
    
    
    result_mean[idx, ] <- CV_joint(df1, df2, df3) %>%
      mutate(across(everything(), ~ .x^2)) %>%
      summarise(across(everything(), mean)) %>% 
      mutate(across(everything(), sqrt))
    
    idx <- idx + 1
  }
}


# I should actually calculate sd here somewhere.
# it is not the sd of each sum but sd of each sqrt.
# result_mean will include
# I need to separate mean and sd because they differ in size

result_mean$covariance <- c(rep(covariance, num_simulations))

result_sd <- tibble()
for (c in covariance) {
  result_sd <- rbind(
    result_sd,
    result_mean %>%
      filter(covariance == c) %>%
      dplyr::select(-covariance) %>%
      summarise(across(everything(), sd))
  )
}

save(result_mean, file="C:\\Users\\hallb\\R_projects\\mixed models examples\\2.0\\korrelation simulering\\result_mean_1Ksim 16 mars.RData")
save(result_sd, file="C:\\Users\\hallb\\R_projects\\mixed models examples\\2.0\\korrelation simulering\\result_sd_1Ksim 16 mars.RData")


######

# plot
######
load("C:\\Users\\hallb\\R_projects\\mixed models examples\\2.0\\korrelation simulering\\result_mean_1Ksim 16 mars.RData")
load("C:\\Users\\hallb\\R_projects\\mixed models examples\\2.0\\korrelation simulering\\result_sd_1Ksim 16 mars.RData")

covariance <- c(-.9, -.5, seq(0, .95, by = .1))
res_plot <- tibble()

for (c in covariance) {
  res_plot <- rbind(
    res_plot,
    result_mean %>%
      filter(covariance == c) %>%
      dplyr::select(-covariance) %>%
      summarise(across(everything(), mean))
  )
}
res_plot$covariance <- covariance

ggplot(res_plot, aes(x = covariance, y = msePRED1), size = 2, color = "black") +
  geom_point() +
  geom_smooth(data = result_mean, aes(x = covariance, y = msePRED1)) +
  labs(x = "Correlation", y = "RMSE(CV)") + 
  theme_pubr()

ggplot(res_plot, aes(x = covariance, y = msePRED2), size = 2, color = "black") +
  geom_point() +
  geom_smooth(data = result_mean, aes(x = covariance, y = msePRED2)) +
  labs(x = "Correlation", y = "RMSE(SSM)") + 
  theme_pubr()

ggplot(res_plot, aes(x = covariance, y = msePRED3), size = 2, color = "black") +
  geom_point() +
  geom_smooth(data = result_mean, aes(x = covariance, y = msePRED3)) +
  labs(x = "Correlation", y = "RMSE(SST)") + 
  theme_pubr()

ggplot(res_plot, aes(x = covariance, y = mseEST), size = 2, color = "black") +
  geom_point() +
  geom_smooth(data = result_mean, aes(x = covariance, y = mseEST)) +
  labs(x = "Correlation", y = "RMSE(PAT)") + 
  theme_pubr()
