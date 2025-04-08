# kör om korrelations plotten men jag jämför joint och univariate och slr
# kör med högre varians tex 6, kommer från riktig data
# jag har satt att antalet hemma mätning skiljer sig och är inte fixed som det egentligen ska vara.
# sätt också att home har lägre intercept och brantare lutning

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
source("C:\\Users\\hallb\\R_projects\\mixed models examples\\2.0\\CV_joint.R")
source("C:\\Users\\hallb\\R_projects\\mixed models examples\\2.0\\CV_univariate.R")
source("C:\\Users\\hallb\\R_projects\\mixed models examples\\2.0\\CV_slr.R")



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
  residual_error <- 6
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
covariance <- seq(0, .95, by = .1)

# only last deleted results
result_joint <- tibble(
  msePRED1 = rep(0, num_simulations*length(covariance)),
  msePRED2 = rep(0, num_simulations*length(covariance)),
  msePRED3 = rep(0, num_simulations*length(covariance)),
  mseEST   = rep(0, num_simulations*length(covariance))
)
result_uni <- result_joint
result_slr <- result_joint


idx <- 1
for (i in 1:num_simulations) {
  for (cov in covariance) {
    print(paste(idx, " /", num_simulations*length(covariance)))
    meta_data <- sim_data(cov)
    df1 <- meta_data$df
    modified_data <- modify_data(df1)
    df2 <- modified_data[[1]]
    df3 <- modified_data[[2]] # this time the fifth element is only the last measurements
    
    
    result_joint[idx, ] <- CV_joint(df1, df2, df3)
    result_uni[idx, ]   <- CV_univariate(df1, df2, df3)
    result_slr[idx, ]   <- CV_slr(df1, df2, df3)
    
    idx <- idx + 1
  }
}
result_joint$covariance <- rep(covariance, num_simulations)
result_uni$covariance   <- rep(covariance, num_simulations)
result_slr$covariance   <- rep(covariance, num_simulations)


save(result_joint, file="C:\\Users\\hallb\\R_projects\\mixed models examples\\2.0\\korrelation simulering\\result_joint 4 april.RData")
save(result_uni,   file="C:\\Users\\hallb\\R_projects\\mixed models examples\\2.0\\korrelation simulering\\result_uni 4 april.RData")
save(result_slr,   file="C:\\Users\\hallb\\R_projects\\mixed models examples\\2.0\\korrelation simulering\\result_slr 4 april.RData")
######

# plot
######
load("C:\\Users\\hallb\\R_projects\\mixed models examples\\2.0\\korrelation simulering\\result_joint 4 april.RData")
load("C:\\Users\\hallb\\R_projects\\mixed models examples\\2.0\\korrelation simulering\\result_uni 4 april.RData")
load("C:\\Users\\hallb\\R_projects\\mixed models examples\\2.0\\korrelation simulering\\result_slr 4 april.RData")


covariance <- seq(0, .95, by = .1)
res_plot <- tibble()
res_plot_uni <- tibble()
res_plot_slr <- tibble()

for (c in covariance) {
  res_plot <- rbind(
    res_plot,
    result_joint %>%
      filter(covariance == c) %>%
      dplyr::select(-covariance) %>%
      summarise(across(everything(), mean))
  )
  
  res_plot_uni <- rbind(
    res_plot_uni,
    result_uni %>%
      filter(covariance == c) %>%
      dplyr::select(-covariance) %>%
      summarise(across(everything(), mean))
  )
  
  res_plot_slr <- rbind(
    res_plot_slr,
    result_slr %>%
      filter(covariance == c) %>%
      dplyr::select(-covariance) %>%
      summarise(across(everything(), mean))
  )
}
res_plot$covariance <- covariance
res_plot_uni$covariance <- covariance
res_plot_slr$covariance <- covariance

ggplot() +
  geom_point(data = res_plot, aes(x = covariance, y = msePRED1), size = 2, color = "black") +
  geom_point(data = res_plot_uni, aes(x = covariance, y = msePRED1), size = 2, color = "red") +
  geom_smooth(data = result_joint, aes(x = covariance, y = msePRED1), color = "black") +
  geom_smooth(data = result_uni, aes(x = covariance, y = msePRED1), color = "red") +
  
  #geom_point(data = res_plot_slr, aes(x = covariance, y = msePRED1), size = 2, color = "green") +
  #geom_smooth(data = res_plot_slr, aes(x = covariance, y = msePRED1), color = "green") +
  labs(x = "Correlation", y = "RMSE(CV)") + 
  theme_pubr(base_family = "sans") +
  theme(
    axis.title = element_text(size = 16),       # Axis title size
    #axis.text = element_text(size = 16),        # Axis tick label size
    plot.title = element_text(size = 16),
    legend.text = element_text(size = 16)
  )

ggplot() +
  geom_point(data = res_plot, aes(x = covariance, y = msePRED2), size = 2, color = "black") +
  geom_point(data = res_plot_uni, aes(x = covariance, y = msePRED2), size = 2, color = "red") +
  geom_smooth(data = result_joint, aes(x = covariance, y = msePRED2), color = "black") +
  geom_smooth(data = result_uni, aes(x = covariance, y = msePRED2), color = "red") +
  
  #geom_point(data = res_plot_slr, aes(x = covariance, y = msePRED2), size = 2, color = "green") +
  #geom_smooth(data = res_plot_slr, aes(x = covariance, y = msePRED2), color = "green") +
  labs(x = "Correlation", y = "RMSE(CM)") + 
  theme_pubr(base_family = "sans") +
  theme(
    axis.title = element_text(size = 16),       # Axis title size
    #axis.text = element_text(size = 16),        # Axis tick label size
    plot.title = element_text(size = 16),
    legend.text = element_text(size = 16)
  )

ggplot() +
  geom_point(data = res_plot, aes(x = covariance, y = msePRED3), size = 2, color = "black") +
  geom_point(data = res_plot_uni, aes(x = covariance, y = msePRED3), size = 2, color = "red") +
  geom_smooth(data = result_joint, aes(x = covariance, y = msePRED3), color = "black") +
  geom_smooth(data = result_uni, aes(x = covariance, y = msePRED3), color = "red") +
  
  #geom_point(data = res_plot_slr, aes(x = covariance, y = msePRED3), size = 2, color = "green") +
  #geom_smooth(data = res_plot_slr, aes(x = covariance, y = msePRED3), color = "green") +
  labs(x = "Correlation", y = "RMSE(SST)") + 
  theme_pubr(base_family = "sans") +
  theme(
    axis.title = element_text(size = 16),       # Axis title size
    #axis.text = element_text(size = 16),        # Axis tick label size
    plot.title = element_text(size = 16),
    legend.text = element_text(size = 16)
  )

ggplot() +
  geom_point(data = res_plot, aes(x = covariance, y = mseEST), size = 2, color = "black") +
  geom_point(data = res_plot_uni, aes(x = covariance, y = mseEST), size = 2, color = "red") +
  geom_smooth(data = result_joint, aes(x = covariance, y = mseEST), color = "black") +
  geom_smooth(data = result_uni, aes(x = covariance, y = mseEST), color = "red") +
  
  #geom_point(data = res_plot_slr, aes(x = covariance, y = mseEST), size = 2, color = "green") +
  #geom_smooth(data = res_plot_slr, aes(x = covariance, y = mseEST), color = "green") +
  labs(x = "Correlation", y = "RMSE(PAT)") + 
  theme_pubr(base_family = "sans") +
  theme(
    axis.title = element_text(size = 16),       # Axis title size
    #axis.text = element_text(size = 16),        # Axis tick label size
    plot.title = element_text(size = 16),
    legend.text = element_text(size = 16)
  )


# slr stats
print(mean_slr)
