# How does the prediction error change if we increase variation between subjects?
# Change varhomeb0 and varhospb0, (1, 1), (2, 2), (3, 3)

# How does the prediction error change when we increase variation within a subjects measurements?
# change varhomeb1 and varhospb1, (1, 1), (2, 2), (3, 3)

# How does the prediction error change when we increase variation between subjects and individual measurements?
# both of the above

library(tidyverse)
library(lme4)
library(dplyr)
rm(list = ls())
source("C:\\Users\\hallb\\R_projects\\mixed models examples\\simulate_data_Analys4.R")
source("C:\\Users\\hallb\\R_projects\\mixed models examples\\performe_LOOCV.R")

### default settings
#varhomeb0 <- 1
#varhospb0 <- 1 
#varhomeb1 <- 1
#varhospb1 <- 1
#cov_homeb0hospb0 <- 0.5
#cov_homeb0homeb1 <- -0.5
#cov_homeb0hospb1 <- -0.5
#cov_hospb0homeb1 <- -0.5
#cov_hospb0hospb1 <- -0.5
#cov_homeb1hospb1 <- 0.5

#simulate_data <- function(varhomeb0, varhospb0,
 #                         varhomeb1, varhospb1,
  #                        cov_homeb0hospb0, cov_homeb0homeb1,
   #                       cov_homeb0hospb1, cov_hospb0homeb1,
    #                      cov_hospb0hospb1, cov_homeb1hospb1)


# Question one
df1 <- simulate_data(1, 1, 1, 1, 0.5, -0.5, -0.5, -0.5, -0.5, 0.5)
df2 <- simulate_data(2, 2, 1, 1, 0.5, -0.5, -0.5, -0.5, -0.5, 0.5)
df3 <- simulate_data(3, 3, 1, 1, 0.5, -0.5, -0.5, -0.5, -0.5, 0.5)

results1 <- performe_LOOCV(df1)
results2 <- performe_LOOCV(df2)
results3 <- performe_LOOCV(df3)

# Question 2
df4 <- simulate_data(1, 1, 1, 1, 0.5, -0.5, -0.5, -0.5, -0.5, 0.5)
df5 <- simulate_data(1, 1, 2, 2, 0.5, -0.5, -0.5, -0.5, -0.5, 0.5)
df6 <- simulate_data(1, 1, 3, 3, 0.5, -0.5, -0.5, -0.5, -0.5, 0.5)

results4 <- performe_LOOCV(df4)
results5 <- performe_LOOCV(df5)
results6 <- performe_LOOCV(df6)

# Question 3
df7 <- simulate_data(1, 1, 1, 1, 0.5, -0.5, -0.5, -0.5, -0.5, 0.5)
df8 <- simulate_data(2, 2, 2, 2, 0.5, -0.5, -0.5, -0.5, -0.5, 0.5)
df9 <- simulate_data(3, 3, 3, 3, 0.5, -0.5, -0.5, -0.5, -0.5, 0.5)

results7 <- performe_LOOCV(df7)
results8 <- performe_LOOCV(df8)
results9 <- performe_LOOCV(df9)