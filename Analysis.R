library(tidyverse)
library(lme4)

load("simulated_data.RData")
df_home <- df %>% filter(Setting == 1)
df_hospital <- df %>% filter(Setting == 2)

load("simulated_data_deleted_rows.RData")
df_home_deleted_rows <- df_deleted_rows %>% filter(Setting == 1)
df_hospital_deleted_rows <- df_deleted_rows %>% filter(Setting == 2)



# Fitting models and save them
# Univariate mixed-effects model: home and hospital spirometry separate.
lmm_home <- lmer(FEV1 ~ Age + (1 + Age | ID), data = df_home) 
lmm_hospital <- lmer(FEV1 ~ Age + (1 + Age | ID), data = df_hospital)
summary_lmm_home <- summary(lmm_home)
summary_lmm_hospital <- summary(lmm_hospital)

randomEffects_lmm_home <- ranef(lmm_home) # this essentially gives the random effects "around the line". You need to add the mean line for each individual to get the correct value.
df_home$fitted <- predict(lmm_home)


randomEffects_lmm_hospital <- ranef(lmm_hospital)
df_hospital$fitted <- predict(lmm_hospital)

# deleted rows data set

lmm_home_deleted_rows <- lmer(FEV1 ~ Age + (1 + Age | ID), data = df_home_deleted_rows)
randomEffects_lmm_deleted_rows_home <- ranef(lmm_home_deleted_rows)
df_home_deleted_rows$fitted <- predict(lmm_home_deleted_rows)

lmm_hospital_deleted_rows <- lmer(FEV1 ~ Age + (1 + Age | ID), data = df_hospital_deleted_rows)
randomEffects_lmm_deleted_rows_hospital <- predict(lmm_hospital_deleted_rows)

# Joint mixed effect
# hospital no deleted rows
joint_lmm_hospital <- lmer(FEV1 ~ Setting + Age + Setting * Age + (-1 + as.factor(Setting) + Age:as.factor(Setting) | ID), data = df_deleted_rows) 
summary(joint_lmm_hospital)
ranef(joint_lmm_hospital) # random effect
predict(joint_lmm_hospital)

# Population average trend
beta_hat <- summary_lmm_home$coefficients[2][1]
beta_tilde <- summary_lmm_hospital$coefficients[2][1]
print(paste("The population average trend is: ", beta_hat - beta_tilde))


# ages hospital ID == 1
ages <- (df_hospital %>% filter(ID == 1))$Age

# Delete a row.
df_hospital_deletedARow <- df_hospital %>% slice(-4)
lmm_hospital_deleted <- lmer(FEV1 ~ Age + (1 + Age | ID), data = df_hospital_deletedARow)
predicted <- predict(lmm_hospital_deleted, newdata = data.frame(ages), re.form = NA)

# Consider the first individual lets illustrate what we should do for all individuals.
df %>% 
  filter(ID == 1 & Setting == 2) %>% 
  ggplot(aes(x = Age, y = FEV1, color = Setting)) +
  geom_smooth(method = "lm", fill = "steelblue", alpha = 0.5) + 
  geom_point(aes(group = Setting)) +
  labs(
    title = "Plot of first subject",    
    subtitle = "Black dots are home spirometry data and blue are hospital. The blue line is regression line for both home and hospital.
    ",
    caption = "Source: Simulated Data"      
  ) +
  theme_minimal()


# Now take away the last point of home spirometry
# fit the model and plot the regression line now. Compare this line with the last point. 
# I.e. subtract and consider the difference. We want it to be small. 
# Do this for all individuals (N). For each ID delete the last row, add to a data set
# Do this for all individuals so you get the same data set as df but 
# Save all differences in a vector as precision_vector




















