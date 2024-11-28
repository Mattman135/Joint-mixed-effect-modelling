# How does the prediction error change when we increase correlation between subjects specific trends?
# Change: cov_homeb1_hospb1 and cov_homeb0_hospb0

# How does the prediction error change when we increase correlation between individual measurements?
# Change: cov_homeb0_homeb1 and cov_homeb0_hospb0 and cov_homeb1_hospb1 and cov_hospb0_hospb1

# How does the prediction error change when we increase correlation between subject specific trends and individual measurements?
# Change: both of the above

library(tidyverse)
library(lme4)
library(dplyr)
rm(list = ls())
source("C:\\Users\\hallb\\R_projects\\mixed models examples\\simulate_data_Analys5.R")
source("C:\\Users\\hallb\\R_projects\\mixed models examples\\performe_LOOCV.R")

###### Default values
#cov_homeb0hospb0 <- 0.5
#cov_homeb0homeb1 <- -0.5
#cov_homeb0hospb1 <- -0.5
#cov_hospb0homeb1 <- -0.5
#cov_hospb0hospb1 <- -0.5
#cov_homeb1hospb1 <- 0.5

#function(cov_homeb0hospb0,
 #        cov_homeb0homeb1,
  #       cov_homeb0hospb1,
    #     cov_hospb0homeb1,
     #    cov_hospb0hospb1,
      #   cov_homeb1hospb1)


################################################################################
### first question
df1 <- simulate_data(0.1, -0.5, -0.5, -0.5, -0.5, 0.1)
df2 <- simulate_data(0.5, -0.5, -0.5, -0.5, -0.5, 0.5)
df3 <- simulate_data(1  , -0.5, -0.5, -0.5, -0.5,   1)

results1 <- performe_LOOCV(df1)
results2 <- performe_LOOCV(df2)
results3 <- performe_LOOCV(df3)

################################################################################
### second question

df4 <- simulate_data(0.5, -0.1, -0.5, -0.5, -0.1, 0.5)
df5 <- simulate_data(0.5, -0.5, -0.5, -0.5, -0.5, 0.5)
df6 <- simulate_data(0.5, -1  , -0.5, -0.5,   -1, 0.5)

results4 <- performe_LOOCV(df4)
results5 <- performe_LOOCV(df5)
results6 <- performe_LOOCV(df6)

################################################################################
### third question

df7 <- simulate_data(0.1, -0.1, -0.1, -0.1, -0.1, 0.1)
df8 <- simulate_data(0.5, -0.5, -0.5, -0.5, -0.5, 0.5)
df9 <- simulate_data(1  ,   -1,   -1,   -1,   -1,   1)

results7 <- performe_LOOCV(df7)
results8 <- performe_LOOCV(df8)
results9 <- performe_LOOCV(df9)

save(results1, file = "results1.Rdata")
save(results2, file = "results2.Rdata")
save(results3, file = "results3.Rdata")
save(results4, file = "results4.Rdata")
save(results5, file = "results5.Rdata")
save(results6, file = "results6.Rdata")
save(results7, file = "results7.Rdata")
save(results8, file = "results8.Rdata")
save(results9, file = "results9.Rdata")

load("results1.Rdata")
load("results2.Rdata")
load("results3.Rdata")
load("results4.Rdata")
load("results5.Rdata")
load("results6.Rdata")
load("results7.Rdata")
load("results8.Rdata")
load("results9.Rdata")
