# note that it is num_simulations per covariance. If I use 1000 simulations its actually 9K simulations 
# if I have covariance <- seq(-1, 1, by = 0.25)
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
source("C:\\Users\\hallb\\R_projects\\mixed models examples\\2.0\\korrelation simulering\\ny cv joint.R")
covariance <- seq(-1, 1, by = 0.25)

run_simulation <- function() {
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
    n_home <- 15
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
    
    # indices to delete If I delete 50%
    indices_to_delete <- c()
    indices_to_delete_ONLY_last <- c()
    
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
      
      # indices to delete
      prel_df <- df1 %>% filter(ID == i, Setting == 2)
      num_hosp_to_delete <- nrow(prel_df) - floor(0.5 *nrow(prel_df))
      samp <- sort(sample(1:nrow(prel_df), num_hosp_to_delete, replace=FALSE  ))
      
      indices_to_delete <- c(
        indices_to_delete,
        (prel_df %>% slice(samp))$Indices,
        (prel_df %>% slice(n()))$Indices
      )
      
      indices_to_delete_ONLY_last <- c(
        indices_to_delete_ONLY_last,
        (prel_df %>% slice(n()))$Indices
      )
    }
    
    df4 <- df1 %>% slice(-indices_to_delete)
    df5 <- df1 %>% slice(indices_to_delete)
    df6_only_last <- df1 %>% slice(indices_to_delete_ONLY_last)
    
    return (list(df2, df3, df4, df5, df6_only_last))
  } # this deletes all the last hospital measurements in df1 and returns this data and the df with only removed values
  
  num_simulations <- 1000
  covariance <- seq(-1, 1, by = 0.25)
  
  # only last deleted results
  result_mean2_onlylast <- tibble(
    msePRED1 = rep(0, num_simulations*length(covariance)),
    msePRED2 = rep(0, num_simulations*length(covariance)),
    msePRED3 = rep(0, num_simulations*length(covariance)),
    mseEST   = rep(0, num_simulations*length(covariance))
  )
  result_sd2_onlylast   <- result_mean2_onlylast
  
  idx <- 1
  for (cov in covariance) {
    for (i in 1:num_simulations) {
      print(paste(cov, "and", i))
      meta_data <- sim_data(cov)
      df1 <- meta_data$df
      modified_data <- modify_data(df1)
      df4 <- modified_data[[3]]
      df5 <- modified_data[[5]] # this time the fifth element is only the last measurements
      
      
      result_mean2_onlylast[idx, ] <- CV_joint(df1, df4, df5) %>%
        mutate(across(everything(), ~ .x^2)) %>%
        summarise(across(everything(), mean))
      
      result_sd2_onlylast[idx, ] <- CV_joint(df1, df4, df5) %>%
        mutate(across(everything(), ~ .x^2)) %>%
        summarise(across(everything(), sd))
      
      idx <- idx + 1
    }
  }
  
  res2_mean_and_sd_onlylast_1000simulations <- rbind(
    result_mean2_onlylast %>%
      mutate(group = ceiling(row_number() / num_simulations)) %>%
      group_by(group) %>%
      summarise(across(everything(), mean, .names = "{.col}"), .groups = "drop"),
    result_sd2_onlylast %>%
      mutate(group = ceiling(row_number() / num_simulations)) %>%
      group_by(group) %>%
      summarise(across(everything(), mean, .names = "{.col}"), .groups = "drop")
  )
  
  res2_mean_and_sd_onlylast_1000simulations$covariance <- rep(covariance, 2)
  res2_mean_and_sd_onlylast_1000simulations$type <- c(rep(0, 9), rep(1, 9))
  
  
  save(res2_mean_and_sd_onlylast_1000simulations, file="C:\\Users\\hallb\\R_projects\\mixed models examples\\2.0\\korrelation simulering\\res2_mean_and_sd_onlylast_1000simulations.RData")
  
} # this function includes all the code essential to run the simulations. from simulating data to save result


# plotting
load("C:\\Users\\hallb\\R_projects\\mixed models examples\\2.0\\korrelation simulering\\res2_mean_and_sd_onlylast_1000simulations.RData")

meansd <- cbind(
  mean = (res2_mean_and_sd_onlylast_1000simulations %>% filter(type == 0))$msePRED3,
  sd = (res2_mean_and_sd_onlylast_1000simulations %>% filter(type == 1))$msePRED3,
  covariance = covariance
)

ggplot(meansd, aes(x = covariance, y = mean)) + 
  geom_smooth() +
  geom_point(size = 2, color = "black") +
  #geom_errorbar(data = meansd, aes(ymin = mean - sd, ymax = mean + sd), width = 0.2) + 
  labs(x = "Covariance", y = "SST") + 
  theme_pubr()
  
meansd <- cbind(
  mean = (res2_mean_and_sd_onlylast_1000simulations %>% filter(type == 0))$mseEST,
  sd = (res2_mean_and_sd_onlylast_1000simulations %>% filter(type == 1))$mseEST,
  covariance = covariance
)

ggplot(meansd, aes(x = covariance, y = mean)) + 
  geom_smooth() +
  geom_point(size = 2, color = "black") +
  #geom_errorbar(data = meansd, aes(ymin = mean - sd, ymax = mean + sd), width = 0.2) + 
  labs(x = "Covariance", y = "PAT") + 
  theme_pubr()


# Some notes to remember: correlation between home slope and hosp slope is the correlation we are varying. The others are fixed.
# 
