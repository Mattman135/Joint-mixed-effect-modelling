library(tidyverse)
library(readr)
library(MASS)
library(Matrix)
library(spatstat.random)
library(Matrix)
library(lme4)
library(dplyr)

simulate_data <- function(
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
  {
  
  # Variable default setting
  #xmin <- 7
  #xmax <- 18
  x_minmax <- c(xmin, xmax)
  
  #sigma_beta0_home <- 1 
  #sigma_beta1_home <- 1
  #rho_home <- -0.5
  #sigma_beta0_hosp <- 1
  #sigma_beta1_hosp <- 1
  #rho_hosp <- -0.5
  #N <- 1000
  #n_home <- 25
  #n_hosp <- 25
  
  #residual_error <- 1
  
  #
  #varhomeb0 <- 1
  #varhospb0 <- 1 
  #varhomeb1 <- 1
  #varhospb1 <- 1
  
  # Var_Cov matrix of fixed effects
  #beta_cov_homeb0hospb0 <- 0.5
  #beta_cov_homeb0homeb1 <- -0.5
  #beta_cov_homeb0hospb1 <- 0
  #beta_cov_hospb0homeb1 <- 0
  #beta_cov_hospb0hospb1 <- -0.5
  #beta_cov_homeb1hospb1 <- 0.5
  
  # Var_Cov matrix of random effects
  #cov_homeb0hospb0 <- 0.5
  #cov_homeb0homeb1 <- -0.5
  #cov_homeb0hospb1 <- 0
  #cov_hospb0homeb1 <- 0
  #cov_hospb0hospb1 <- -0.5
  #cov_homeb1hospb1 <- 0.5
  
  #years_projection <- 1
  
  # Should I have random amount of nhome?
  # Should beta be randomly generated or fix?
  
  #########################################################
  Sigma_beta <- as.matrix(nearPD(matrix(c(
    sigma_beta0_home, beta_cov_homeb0hospb0, beta_cov_homeb0homeb1, beta_cov_homeb0hospb1,
    beta_cov_homeb0hospb0, sigma_beta0_hosp, beta_cov_hospb0homeb1, beta_cov_hospb0hospb1,
    beta_cov_homeb0homeb1, beta_cov_hospb0homeb1, sigma_beta1_home, beta_cov_homeb1hospb1,
    beta_cov_homeb0hospb1, beta_cov_hospb0hospb1, beta_cov_homeb1hospb1, sigma_beta1_hosp
  ), nrow = 4))$mat)
  
  beta <- MASS::mvrnorm(n = 1, mu = c(150, 100, -1, -1), Sigma = Sigma_beta)
  #beta <- c(150, 100, -1, -1)
  
  
  #
  mu_pop_home <- beta[1] + beta[3] * x_minmax
  mu_pop_hosp <- beta[2] + beta[4] * x_minmax
  
  Sigma_b <- as.matrix(nearPD(matrix(c(
    varhomeb0, cov_homeb0hospb0, cov_homeb0homeb1, cov_homeb0hospb1,
    cov_homeb0hospb0, varhospb0, cov_hospb0homeb1, cov_hospb0hospb1,
    cov_homeb0homeb1, cov_hospb0homeb1, varhomeb1, cov_homeb1hospb1,
    cov_homeb0hospb1, cov_hospb0hospb1, cov_homeb1hospb1, varhospb1
  ), nrow = 4))$mat)
  #print(Sigma_b)
  
  b <- MASS::mvrnorm(n = N, mu = c(0, 0, 0, 0), Sigma = Sigma_b)
  
  #
  m_home <- 15 # average measurements per indvidual
  n_ind <- rpois(N, m_home) # measurements per individual
  
  # så nu vill jag simulera en slumpålder (heltal) mellan xmin och xmax
  # sedan göra n_ind mätningar mellan simulerad ålder och ett år framåt
  
  #
  random_ages <- sample(xmin:xmax, size = N, replace = TRUE)
  age_vector <- c()
  Measurement_nr_vector <- c()
  Setting_vector <- c()
  ID_vector <- c()
  FEV1_data <- c()
  mu_vector <- c()
  
  for ( i in 1:N ) {
    
    n_home <- n_ind[i]
   
    #
    x_start <- runif(1, min = xmin, max = xmax)
    x_end <- x_start + years_projection
    
    # HOME
    x_home <- sort(runif(n_home, x_start, x_end))
    
    if (N == 1) {
      mu_home <- (beta[1] + b[1]) + (beta[3] + b[3]) * x_home
    } else {
      mu_home <- (beta[1] + b[i, 1]) + (beta[3] + b[i, 3]) * x_home
    }
    y_home <- mu_home + rnorm(n_home, sd = residual_error)
    
    # HOSPITAL
    x_hosp <- sort(runif(n_hosp, x_start, x_end))
    if (N == 1) {
      mu_hosp <- (beta[2] + b[2]) + (beta[4] + b[4]) * x_hosp
    } else {
      mu_hosp <- (beta[2] + b[i, 2]) + (beta[4] + b[i, 4]) * x_hosp
    }
    y_hosp <- mu_hosp + rnorm(n_hosp, sd = residual_error)
    
    #
    FEV1_data <- c(FEV1_data, y_home, y_hosp)
    ID_vector <- c(ID_vector, rep(i, n_home + n_hosp))
    Setting_vector <- c(Setting_vector, rep(1, n_home), rep(2, n_hosp))
    Measurement_nr_vector <- c(Measurement_nr_vector, c(1:n_home), c(1:n_hosp))
    age_vector <- c(age_vector, x_home, x_hosp)
    mu_vector <- c(mu_vector, mu_home, mu_hosp)
  }
  
  df <- tibble(
    ID = ID_vector, 
    Setting = Setting_vector, 
    Measurement_nr = Measurement_nr_vector, 
    Age = age_vector, 
    FEV1 = FEV1_data
  )
  
  return (
    list(
      df = df, beta_true = beta, b_true = b, mu_vector = mu_vector
    )
  )
}

