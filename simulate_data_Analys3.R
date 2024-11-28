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
simulate_data <- function(N) {
  
  # variables
  xmin <- 7
  xmax <- 8
  sigma_beta0 <- 1 
  sigma_beta1 <- 1
  rho_home <- -0.5
  sigma_beta0_hosp <- 1 
  sigma_beta1_hosp <- 1
  rho_hosp <- -0.5
  #N <- 100
  m <- 8 # number of home measurements.
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
  x_minmax <- c(xmin, xmax)
  
  ##### HOME SPIROMETRY Population trend aka fixed effect
  Sigma_home <- matrix(c(sigma_beta0^2, rho_home * sigma_beta0 * sigma_beta1, rho_home * sigma_beta0 * sigma_beta1, sigma_beta1^2),
                       nrow = 2)
  beta <- MASS::mvrnorm(n = 1, mu = c(100, -1), Sigma = Sigma_home) # If n=1 then return is a vector of same length as mu. mvrnorm.
  mu_pop_home <- beta[1] + beta[2] * x_minmax
  
  ##### HOSPITAL SPIROMTERY also population trend aka fixed effect
  Sigma_hosp <- matrix(c(sigma_beta0_hosp^2, rho_hosp * sigma_beta0_hosp * sigma_beta1_hosp, rho_hosp * sigma_beta0_hosp * sigma_beta1_hosp, sigma_beta1_hosp^2),
                       nrow = 2)
  
  beta_hosp <- MASS::mvrnorm(n=1, mu = c(100, -1), Sigma = Sigma_hosp)
  mu_pop_hosp <- beta_hosp[1] + beta_hosp[2] * x_minmax
  
  ##### Subject-specific trends HOME SPIROMETRY
  n_ind <- rpoistrunc(N, m, minimum = 1, method=c("harding", "transform"), implem=c("R", "C")) # number of observations, poisson distributed
  
  ## Simulating random effect
  
  # Covariance matrix 
  Sigma_e <- as.matrix(nearPD(matrix(c(varhomeb0, cov_homeb0hospb0, cov_homeb0homeb1, cov_homeb0hospb1,
                                       cov_homeb0hospb0, varhospb0, cov_hospb0homeb1, cov_hospb0hospb1,
                                       cov_homeb0homeb1, cov_hospb0homeb1, varhomeb1, cov_homeb1hospb1,
                                       cov_homeb0hospb1, cov_hospb0hospb1, cov_homeb1hospb1, varhospb1),
                                     nrow = 4))$mat)
  
  b <- MASS::mvrnorm(n = N, mu = c(0, 0, 0, 0), Sigma = Sigma_e)
  
  random_ages <- sample(xmin:xmax, size = N, replace = TRUE)
  age_vector <- c()
  Measurement_nr_vector <- c()
  Setting_vector <- c()
  ID_vector <- c()
  FEV1_data <- c()
  
  for ( i in 1:N ) {
    # HOME
    n <- m # n_ind[i] # if you want home measurements fixed change this to equal m, random change to n_ind[i]
    x <- sort(runif(n, xmin, xmax))
    mu <- (beta[1] + b[i, 1]) + (beta[2] + b[i, 2]) * x
    y <- mu + rnorm(n, sd = 1)
    
    # HOSPITAL
    n_hosp <- n_hosp
    x_hosp <- sort(runif(n_hosp, xmin, xmax))
    mu_hosp <- (beta_hosp[1] + b[i, 3]) + (beta_hosp[2] + b[i, 4]) * x_hosp
    y_hosp <- mu_hosp + rnorm(n_hosp, sd = 1)
    
    # Columns
    FEV1_data <- c(FEV1_data, y, y_hosp)
    ID_vector <- c(ID_vector, rep(i, n+n_hosp))
    Setting_vector <- c(Setting_vector, rep(1, n), rep(2, n_hosp))
    Measurement_nr_vector <- c(Measurement_nr_vector, c(1:n), c(1:n_hosp))
    age_vector <- c(age_vector, x, x_hosp)
  }
  
  Age <- age_vector
  FEV1 <- FEV1_data
  ID <- ID_vector
  Setting <- Setting_vector
  Measurement_nr <- Measurement_nr_vector
  df <- tibble(ID, Setting, Measurement_nr, Age, FEV1)
  return (df)
}