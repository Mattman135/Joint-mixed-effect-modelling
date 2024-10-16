# Packages
library(tidyverse)
library(readr)
library(MASS)
library(Matrix)
library(corrplot)
library(spatstat.random)
library(Matrix)
library(corrplot)


# Clear working environment and set seed.
rm(list = ls())
#set.seed(123)

# Specify the age gap
xmin <- 7
xmax <- 18
x_minmax <- c(xmin, xmax)

##### HOME SPIROMETRY Population trend aka fixed effect
sigma_beta0 <- 1
sigma_beta1 <- 1 
rho_home <- -0.5 
Sigma_home <- matrix(c(sigma_beta0^2, rho_home * sigma_beta0 * sigma_beta1, rho_home * sigma_beta0 * sigma_beta1, sigma_beta1^2),
                     nrow = 2)
beta <- MASS::mvrnorm(n = 1, mu = c(100, -1), Sigma = Sigma_home) # If n=1 then return is a vector of same length as mu. mvrnorm.
mu_pop_home <- beta[1] + beta[2] * x_minmax
plot(x_minmax, mu_pop_home, lty = 1, bty = "l", type = "l", xlab = "Age (years)", ylab = "FEV1", lwd = 2, ylim=range(c(0,150)))


##### HOSPITAL SPIROMTERY also population trend aka fixed effect
sigma_beta0_hosp <- 1
sigma_beta1_hosp <- 1 
rho_hosp <- -0.5
Sigma_hosp <- matrix(c(sigma_beta0_hosp^2, rho_hosp * sigma_beta0_hosp * sigma_beta1_hosp, rho_hosp * sigma_beta0_hosp * sigma_beta1_hosp, sigma_beta1_hosp^2),
                     nrow = 2)
beta_hosp <- MASS::mvrnorm(n=1, mu = c(100, -1), Sigma = Sigma_hosp)
mu_pop_hosp <- beta_hosp[1] + beta_hosp[2] * x_minmax
lines(x_minmax, mu_pop_hosp, lty = 1, bty = "l", type = "l", col="blue", lwd=2)
legend("topright", legend = c("Population mean Home", "Population mean Hospital"),
       col = c("black", "blue"), lty = 1, cex = 1)


##### Subject-specific trends HOME SPIROMETRY
N <- 100 # number of subjects
m <- 8 # Average number of measurements per subject
n_ind <- rpoistrunc(N, m, minimum = 1, method=c("harding", "transform"), implem=c("R", "C"))

## Simulating random effect
mu <- c(0, 0, 0, 0)

# Covariance matrix. Change variables here depending on 
Sigma_e <- matrix(c(1, 0.5, -0.5, -0.5,
                    0.5, 1, -0.5, -0.5,
                   -0.5, -0.5, 1, 0.5,
                   -0.5, -0.5, 0.5, 1), 
                  nrow = 4)
column_names <- c("home_b0", "hosp_b0", "home_b1", "hosp_b1")
rownames(Sigma_e) <- column_names
colnames(Sigma_e) <- column_names

Sigma_e <- as.matrix(nearPD(Sigma_e)$mat)
#corrplot(Sigma_e, method = "number")

b <- MASS::mvrnorm(n = N, mu = c(0, 0, 0, 0), Sigma = Sigma_e)

random_ages <- sample(xmin:xmax, size = N, replace = TRUE)
age_vector <- c()
Measurement_nr_vector <- c()
Setting_vector <- c()
ID_vector <- c()
FEV1_data <- c()

for ( i in 1:N ) {
  # HOME
  n <- n_ind[i]
  #x <- seq(from = xmin, to = xmax, length.out = n) # non random time steps
  x <- sort(runif(n, xmin, xmax)) # random time steps
  
  mu <- (beta[1] + b[i, 1]) + (beta[2] + b[i, 2]) * x
  y <- mu + rnorm(n, sd = 1)
  lines(x, y, col = rgb(0.5, 0.5, 0.5, alpha = 0.3))
  
  # HOSPITAL
  n_hosp <- 4
  #x_hosp <- seq(from = xmin, to = xmax, length.out = n_hosp) #  non random time steps
  x_hosp <- sort(runif(n_hosp, xmin, xmax)) # random time steps
  mu_hosp <- (beta_hosp[1] + b[i, 3]) + (beta_hosp[2] + b[i, 4]) * x_hosp
  y_hosp <- mu_hosp + rnorm(n_hosp, sd = 1)
  lines(x_hosp, y_hosp, col=rgb(0.25, 0.88, 0.82, alpha = 0.3))
  
  
  # Columns
  FEV1_data <- c(FEV1_data, y, y_hosp)
  ID_vector <- c(ID_vector, rep(i, n+n_hosp))
  Setting_vector <- c(Setting_vector, rep(1, n), rep(2, n_hosp))
  Measurement_nr_vector <- c(Measurement_nr_vector, c(1:n), c(1:n_hosp))
  age_vector <- c(age_vector, x, x_hosp)
  # round(seq(from = random_ages[i], by = 1/n, length.out = n)
  # round(seq(from = random_ages[i], by = 1/n_hosp, length.out = n_hosp)

  # Check ages. 
  #print(paste("x: ", x, "x hosp", x_hosp))
}


# Saving simulated data in tibble
Age <- age_vector
FEV1 <- FEV1_data
ID <- ID_vector
Setting <- Setting_vector
Measurement_nr <- Measurement_nr_vector
df <- tibble(ID, Setting, Measurement_nr, Age, FEV1)

# Saving parameters in tibble
parameters <- tibble(
  xmin = xmin,
  xmax = xmax,
  sigma_beta0 = sigma_beta0,
  sigma_beta1 = sigma_beta1,
  rho_home = rho_home,
  sigma_beta0_hosp = sigma_beta0_hosp,
  sigma_beta1_hosp = sigma_beta1_hosp,
  rho_hosp,
  N = N,
  m = m
)


#####################################################################################
# This code deletes last row of all the hospital data in df.
# Do not run this code unessesary, only run it when saving data.
# Delete the last row of all hospital data. And save this data. Do not run this again.

nrows <- nrow(df)
df_deleted_rows <- tibble()

for (i in 1:nrows) {
  df_preliminary <- df %>% filter(ID == i)
  prel_length <- nrow(df_preliminary)
  df_preliminary <- df_preliminary %>% slice(-prel_length)
  df_deleted_rows <- bind_rows(df_deleted_rows, df_preliminary)
}
####################################################################################

save(df_deleted_rows, file="simulated_data_deleted_rows.RData")
save(df, file="simulated_data.RData")