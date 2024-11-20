# This file compares LMMs to simple linear regression and univariate mixed-effects model.


library(tidyverse)
library(corrplot)
library(lme4)
library(dplyr)
rm(list = ls())

load("simulated_data.RData")

df <- simulated_data
df_copy <- df # this is only to have access to indices
N <- length(unique(df$ID)) # number of subjects
df_copy$Indices <- c(1:nrow(df)) # we need to have access to indices




##### Joint model
# non modified data
joint_lmm1 <- lmer(FEV1 ~ Setting + Age + Setting * Age + (-1 + as.factor(Setting) + Age:as.factor(Setting) | ID), data = df) # normal data I dont need to fit this every time in the loop
predictions1 <- predict(joint_lmm1) # save predictions here and use index to get the right index, thats why we make a df_copy in order to have access to indices.
randomEffects1 <- ranef(joint_lmm1)
fixedEffects1 <- fixef(joint_lmm1)

#
mspe1_joint <- 0
mspe2_joint <- 0
mspe3_joint <- 0 # mspe3_joint_intercept, random effects
mspe3_joint_slope <- 0 # random effects
mspe4_joint <- 0 # mspe4_joint_intercept, fixed effects
mspe4_joint_slope <- 0 # fixed effects 

for (i in 1:N) {
  indexToSlice <- (df_copy %>% filter(ID == i) %>% slice(n()))$Indices # extracts the index of the last hospital measurement for each individual
  ageToPredict <- (df_copy %>% filter(ID == i) %>% slice(n()))$Age
  prel_df <- df %>% slice(-indexToSlice) # this is the LOOCV data set
  
  # fit model on prel_df
  joint_lmm2 <- lmer(FEV1 ~ Setting + Age + Setting * Age + (-1 + as.factor(Setting) + Age:as.factor(Setting) | ID), data = prel_df) # this is the modified data
  new_prediction <- predict(joint_lmm2, newdata = data.frame(ID = i, Setting = 2, Measurement_nr = 4, Age = ageToPredict), re.form = NA)
  randomEffects2 <- ranef(joint_lmm2)
  fixedEffects2 <- fixef(joint_lmm2)
  
  # Calulating sum's, for now only calculate for intercept, i.e. b0, and beta0
  mspe1_joint <- mspe1_joint + ((df %>% slice(indexToSlice))$FEV1 - (new_prediction))^2
  mspe2_joint <- mspe2_joint + ((predictions1[indexToSlice])-(new_prediction))^2
  
  # random effects
  mspe3_joint <- mspe3_joint + ((randomEffects1$ID[i,2])-(randomEffects2$ID[i,2]))^2 # random effects intercept
  mspe3_joint_slope <- mspe3_joint_slope + ((randomEffects1$ID[i,4])-(randomEffects2$ID[i,4]))^2 # random effects slope
  
  # fixed effects
  mspe4_joint <- mspe4_joint + ((fixedEffects1[2])-(fixedEffects2[2]))^2 # fixed effect intercept
  mspe4_joint_slope <- mspe4_joint_slope + ((fixedEffects1[4])-(fixedEffects2[4]))^2 # fixed effect slope
}

mspe1_joint <- sqrt(mspe1_joint / N)
mspe2_joint <- sqrt(mspe2_joint / N)
mspe3_joint <- sqrt(mspe3_joint / N)
mspe4_joint <- sqrt(mspe4_joint / N)
mspe3_joint_slope <- sqrt(mspe3_joint_slope / N)
mspe4_joint_slope <- sqrt(mspe4_joint_slope / N)

##### Simple linear regression, slr, Note that this will not include a random effect
# hence we are comparing the estimated parameters from slr with fixed effects
mspe1_slr <- 0
mspe2_slr <- 0
mspe3_slr <- 0 # Compare with mspe3_joint, random effects
mspe4_slr <- 0 # Compare with mspe4_joint, fixed effects
mspe3_slr_slope <- 0 # random effects, compare with mspe3_joint_slope
mspe4_slr_slope <- 0 # fixed effect, compare with mspe4_joint_slope

slr_hospital <- lm(FEV1 ~ Age, data = df %>% filter(Setting == 2)) # simple linear regression on hospital data

for (i in 1:N) {
  indexToSlice <- (df_copy %>% filter(ID == i) %>% slice(n()))$Indices # extracts the index of the last hospital measurement for each individual
  ageToPredict <- (df_copy %>% filter(ID == i) %>% slice(n()))$Age
  slr1 <- lm(FEV1 ~ Age, data = (df %>% filter(ID == i, Setting == 2))) # fit hospital data on individual i, i.e. random effects of individual i
  prediction_slr1 <- slr1$coefficients[1] + slr1$coefficients[2]*ageToPredict
  slr2 <- lm(FEV1 ~ Age, data = (df %>% slice(-indexToSlice)) %>% filter(ID == i, Setting == 2)) # fit on modified hospital data
  prediction_slr2 <- slr2$coefficients[1] + slr2$coefficients[2]*ageToPredict
  
  # predictions
  mspe1_slr <- mspe1_slr + (((df %>% slice(indexToSlice))$FEV1) - (prediction_slr2))^2
  mspe2_slr <- mspe2_slr + ((prediction_slr1) - (prediction_slr2))^2
  
  # random effects
  mspe3_slr <- mspe3_slr + ((slr1$coefficients[1]) - (slr2$coefficients[1]))^2
  mspe3_slr_slope <- mspe3_slr_slope + ((slr1$coefficients[2]) - (slr2$coefficients[2]))^2

  # fixed effects, modify data means that we delete/slice last hospital measurement of individual i and efterward we extract all hospital data.
  slr_hospital_modifiedData <- lm(FEV1 ~ Age, data = (df %>% slice(-indexToSlice)) %>% filter(Setting == 2))
  mspe4_slr <- mspe4_slr + ((slr_hospital$coefficients[1])-(slr_hospital_modifiedData$coefficients[1]))^2
  mspe4_slr_slope <- mspe4_slr_slope + ((slr_hospital$coefficients[2])-(slr_hospital_modifiedData$coefficients[2]))^2
}

mspe1_slr <- sqrt(mspe1_slr / N) # compare with mspe1_joint
mspe2_slr <- sqrt(mspe2_slr / N) # compare with mspe2_joint
mspe3_slr <- sqrt(mspe3_slr / N) # compare with mspe3_joint, i.e. compare the intercept of random effects from slr and joint mixed-effects model.
mspe3_slr_slope <- sqrt(mspe3_slr_slope / N) # compare with mspe3_joint_slope
mspe4_slr <- sqrt(mspe4_slr / N)
mspe4_slr_slope <- sqrt(mspe4_slr_slope / N)


##### Univariate mixed-effects model, hospital data
mspe1_uni <- 0
mspe2_uni <- 0
mspe3_uni<- 0
mspe3_uni_slope <- 0
mspe4_uni <- 0
mspe4_uni_slope <- 0

uni_hospital <- lmer(FEV1 ~ Age + (1 + Age | ID), data = df %>% filter(Setting == 2))
predictions_uni1 <- predict(uni_hospital)
randomEffects1_uni <- ranef(uni_hospital)$ID
fixedEffects1_uni <- fixef(uni_hospital)

for (i in 1:1) {
  # extracting index, age, fitting model, modified data, save random effects and fixed effects
  indexToSlice <- (df_copy %>% filter(ID == i) %>% slice(n()))$Indices
  ageToPredict <- (df_copy %>% filter(ID == i) %>% slice(n()))$Age
  uni_hospital_modifiedData <- lmer(FEV1 ~ Age + (1 + Age | ID), data = (df %>% slice(-indexToSlice)) %>% filter(Setting == 2))
  new_prediction <- predict(uni_hospital_modifiedData, newdata = data.frame(ID = i, Setting = 2, Measurement_nr = 4, Age = ageToPredict), re.form = NA)
  randomEffects2_uni <- ranef(uni_hospital_modifiedData)$ID
  fixedEffects2_uni <- fixef(uni_hospital_modifiedData)
  
  # predictions
  mspe1_uni <- mspe1_uni + (((df_copy %>% filter(ID == i) %>% slice(n()))$FEV1)-(new_prediction))^2
  mspe2_uni <- mspe2_uni + ((predictions_uni1[indexToSlice])-(new_prediction))^2
  
  # random effects
  mspe3_uni <- mspe3_uni + ((randomEffects1_uni[i,1])-(randomEffects2_uni[i,1]))^2
  mspe3_uni_slope <- mspe3_uni_slope + ((randomEffects1_uni[i,2])-(randomEffects2_uni[i,2]))^2
  
  # fixed effects
  mspe4_uni <- mspe4_uni + ((fixedEffects1_uni[1])-(fixedEffects2_uni[1]))^2
  mspe4_uni_slope <- mspe4_uni_slope + ((fixedEffects1_uni[2])-(fixedEffects2_uni[2]))^2
}

mspe1_uni <- sqrt(mspe1_uni / N) 
mspe2_uni <- sqrt(mspe2_uni / N)
mspe3_uni<- sqrt(mspe3_uni / N)
mspe3_uni_slope <- sqrt(mspe3_uni_slope / N)
mspe4_uni <- sqrt(mspe4_uni / N)
mspe4_uni_slope <- sqrt(mspe4_uni_slope / N)

resultsAnalys1 <- tibble(
  name = c("mspe1", "mspe2", "mspe3 intercept", "mspe3 slope", "mspe4 intercept", "mspe4 slope"),
  jLMM = round(c(mspe1_joint, mspe2_joint, mspe3_joint, mspe3_joint_slope, mspe4_joint, mspe4_joint_slope), 3),
  SLR = round(c(mspe1_slr, mspe2_slr, mspe3_slr, mspe3_slr_slope, mspe4_slr, mspe4_slr_slope), 3),
  uLMM = round(c(mspe1_uni, mspe2_uni, mspe3_uni, mspe3_uni_slope, mspe4_uni, mspe4_uni_slope), 3)
)

save(resultsAnalys1, file = "resultsAnalys1.RData")
