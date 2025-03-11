library(tidyverse)
library(readr)
library(MASS)
library(Matrix)
library(spatstat.random)
library(Matrix)
library(lme4)
library(dplyr)

# ny från claude
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
  # Define fixed effects
  x_minmax <- c(xmin, xmax)
  
  # Create var-cov matrix for fixed effects
  Sigma_beta <- as.matrix(Matrix::nearPD(matrix(c(
    sigma_beta0_home, beta_cov_homeb0hospb0, beta_cov_homeb0homeb1, beta_cov_homeb0hospb1,
    beta_cov_homeb0hospb0, sigma_beta0_hosp, beta_cov_hospb0homeb1, beta_cov_hospb0hospb1,
    beta_cov_homeb0homeb1, beta_cov_hospb0homeb1, sigma_beta1_home, beta_cov_homeb1hospb1,
    beta_cov_homeb0hospb1, beta_cov_hospb0hospb1, beta_cov_homeb1hospb1, sigma_beta1_hosp
  ), nrow = 4), corr=TRUE)$mat)
  
  # Fixed beta values instead of random generation
  beta <- c(150, 100, -5, -1)
  
  # Population means
  mu_pop_home <- beta[1] + beta[3] * x_minmax
  mu_pop_hosp <- beta[2] + beta[4] * x_minmax
  
  # Create var-cov matrix for random effects
  Sigma_b <- as.matrix(Matrix::nearPD(matrix(c(
    varhomeb0, cov_homeb0hospb0, cov_homeb0homeb1, cov_homeb0hospb1,
    cov_homeb0hospb0, varhospb0, cov_hospb0homeb1, cov_hospb0hospb1,
    cov_homeb0homeb1, cov_hospb0homeb1, varhomeb1, cov_homeb1hospb1,
    cov_homeb0hospb1, cov_hospb0hospb1, cov_homeb1hospb1, varhospb1
  ), nrow = 4), corr=TRUE)$mat)
  
  # Generate random effects for all individuals at once
  b <- MASS::mvrnorm(n = N, mu = c(0, 0, 0, 0), Sigma = Sigma_b)
  
  # Generate ages
  random_ages <- sample(xmin:xmax, size = N, replace = TRUE)
  
  # we can also make 
  n_home <- 15
  n_ind <- rpois(N, n_home)
  
  # Fixed number of home measurements per individual
  n_home_per_ind <- n_home * years_projection
  n_hosp_per_ind <- n_hosp * years_projection
  
  # Pre-calculate total rows needed
  total_rows <- N * (n_home_per_ind + n_hosp_per_ind)
  
  # Prepare data containers
  all_ids <- vector("integer", length = total_rows)
  all_settings <- vector("integer", length = total_rows)
  all_measurement_nrs <- vector("integer", length = total_rows)
  all_ages <- vector("numeric", length = total_rows)
  all_fev1 <- vector("numeric", length = total_rows)
  all_mu <- vector("numeric", length = total_rows)
  
  # Fill data vectorized way
  idx <- 1
  
  for (i in 1:N) {
    # n_home <- n_ind[i] # uncomment this if I want home measurements to differ
    
    x_start <- random_ages[i]
    x_end <- x_start + years_projection
    
    # HOME measurements
    x_home <- sort(runif(n_home_per_ind, x_start, x_end))
    mu_home <- (beta[1] + b[i, 1]) + (beta[3] + b[i, 3]) * x_home
    y_home <- mu_home + rnorm(n_home_per_ind, sd = residual_error)
    
    # HOSPITAL measurements
    x_hosp <- sort(runif(n_hosp_per_ind, x_start, x_end))
    mu_hosp <- (beta[2] + b[i, 2]) + (beta[4] + b[i, 4]) * x_hosp
    y_hosp <- mu_hosp + rnorm(n_hosp_per_ind, sd = residual_error)
    
    # Indices for this individual
    home_indices <- idx:(idx + n_home_per_ind - 1)
    hosp_indices <- (idx + n_home_per_ind):(idx + n_home_per_ind + n_hosp_per_ind - 1)
    
    # Fill home measurements
    all_ids[home_indices] <- i
    all_settings[home_indices] <- 1
    all_measurement_nrs[home_indices] <- 1:n_home_per_ind
    all_ages[home_indices] <- x_home
    all_fev1[home_indices] <- y_home
    all_mu[home_indices] <- mu_home
    
    # Fill hospital measurements
    all_ids[hosp_indices] <- i
    all_settings[hosp_indices] <- 2
    all_measurement_nrs[hosp_indices] <- 1:n_hosp_per_ind
    all_ages[hosp_indices] <- x_hosp
    all_fev1[hosp_indices] <- y_hosp
    all_mu[hosp_indices] <- mu_hosp
    
    # Update index
    idx <- idx + n_home_per_ind + n_hosp_per_ind
  }
  
  # Create final dataframe in one operation
  df <- tibble::tibble(
    ID = all_ids,
    Setting = all_settings,
    Measurement_nr = all_measurement_nrs,
    Age = all_ages,
    FEV1 = all_fev1,
    Indices = 1:total_rows
  )
  
  return(list(
    df = df, 
    beta_true = beta, 
    b_true = b, 
    mu_vector = all_mu
  ))
}

