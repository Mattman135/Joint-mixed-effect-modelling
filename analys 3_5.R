# compare joint, uni and slr. 1000 simulations. Last 3 years of data. basic default variables.
# only remove last hospital and calculate mse


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

  }
  return (list(df2, df3))
} # this deletes all the last hospital measurements in df1 and returns this data and the df with only removed values

num_simulations <- 1000

# only last deleted results
result_joint <- tibble(
  msePRED1 = rep(0, num_simulations),
  msePRED2 = rep(0, num_simulations),
  msePRED3 = rep(0, num_simulations),
  mseEST   = rep(0, num_simulations)
)
result_uni   <- result_joint
result_slr   <- result_joint

for (i in 1:num_simulations) {
  print(i)
  meta_data <- sim_data()
  df1 <- meta_data$df
  modified_data <- modify_data(df1)
  df2 <- modified_data[[1]]
  df3 <- modified_data[[2]]
  
  
  #
  result_joint[i, ] <- CV_joint(df1, df2, df3)
  result_uni[i, ]   <- CV_univariate(df1, df2, df3)
  result_slr[i, ]   <- CV_slr(df1, df2, df3)
  
}


######

result <- rbind(
  result_joint,
  result_uni,
  result_slr
)
result$model <- c(
  rep("joint", nrow(result_joint)),
  rep("uni", nrow(result_uni)),
  rep("slr", nrow(result_slr))
)

save(result, file="C:\\Users\\hallb\\R_projects\\mixed models examples\\2.0\\analysis 3\\result 11 mars.RData")




######

######
load("C:\\Users\\hallb\\R_projects\\mixed models examples\\2.0\\analysis 3\\result 11 mars.RData")
result_joint <- result %>% filter(model == "joint")
result_uni   <- result %>% filter(model == "uni")
result_slr   <- result %>% filter(model == "slr")

col_name <- "msePRED3"
#
p1 <- ggplot(result_joint, aes(x = .data[[col_name]])) + geom_histogram() + theme_pubr() + 
  geom_vline(xintercept = mean(result_joint[[col_name]]), color = "orange", size = 1) + 
  labs(x = "SST") + 
  theme_pubr()+
  xlim(0, .3)

p2 <- ggplot(result_uni,   aes(x = .data[[col_name]])) + geom_histogram() + theme_pubr()+ 
  geom_vline(xintercept = mean(result_uni[[col_name]]), color = "orange", size = 1) + 
  labs(x = "SST") +
  theme_pubr()+
  xlim(0, .3)

p3 <- ggplot(result_slr,   aes(x = .data[[col_name]])) + geom_histogram() + theme_pubr()+ 
  geom_vline(xintercept = mean(result_slr[[col_name]]), color = "orange", size = 1) + 
  labs(x = "SST") + 
  theme_pubr()+
  xlim(0, .3)

grid.arrange(p1, p2, p3, nrow=1)

mean(result_joint[[col_name]])
mean(result_uni[[col_name]])
mean(result_slr[[col_name]])





# print average values
print("msePRED1")
print(mean(result_joint$msePRED1))
print(mean(result_uni$msePRED1))
print(mean(result_slr$msePRED1))

print("msePRED2")
print(mean(result_joint$msePRED2))
print(mean(result_uni$msePRED2))
print(mean(result_slr$msePRED2))

print("msePRED3")
print(mean(result_joint$msePRED3))
print(mean(result_uni$msePRED3))
print(mean(result_slr$msePRED3))

print("mseEST")
print(mean(result_joint$mseEST))
print(mean(result_uni$mseEST))
print(mean(result_slr$mseEST))
