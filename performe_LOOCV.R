# Jag vill ha en funktion som tar in en tibble och gör loocv och spottar ut de fyra mse värdena jag vill ha.

performe_LOOCV <- function(df) {
  
  #df <- simulated_data
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
  
  msePRED1 <- mspe1_joint
  msePRED2 <- mspe2_joint
  msePRED3_int <- mspe3_joint
  msePRED3_slope <- mspe4_joint
  mseEST_int <- mspe3_joint_slope
  mseEST_slope <- mspe4_joint_slope
  
  return (list(msePRED1, msePRED2, msePRED3_int, msePRED3_slope, mseEST_int, mseEST_slope))
}
