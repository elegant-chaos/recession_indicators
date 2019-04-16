library(lme4)

# Pooled OLS (AKA Normal Linear Regression)

# With dividends_per_share_log and dividends_per_share_ind
pooled_ols_1 <- lm(price_log ~ book_value_log + dividends_per_share_log + 
                   dividends_per_share_ind + cashflow_log, data = final)
summary(pooled_ols_1)

# Only dividends_per_share_ind
pooled_ols_2 <- lm(price_log ~ book_value_log +  
                   dividends_per_share_ind + cashflow_log, data = final)
summary(pooled_ols_2)

# Only dividends_per_share
pooled_ols_3 <- lm(price_log ~ book_value_log + dividends_per_share_log + 
                    cashflow_log, data = final)
summary(pooled_ols_3)

# Best model has both, will proceed forward with both for all other models

# Individual Fixed Effects (for Companies)
final <- final %>% mutate(tic = as.factor(tic))
fixed_effects_companies <- lm(price_log ~ book_value_log + dividends_per_share_log +
                                dividends_per_share_ind + cashflow_log + tic, data = final)
summary(fixed_effects_companies) # Crazy high R^2 (~0.82)

# Time Fixed Effects (Fixed Effects for Year)
final <- final %>% mutate(year_factor = as.factor(fyearq))
fixed_effects_time <- lm(price_log ~ book_value_log + dividends_per_share_log +
                                dividends_per_share_ind + cashflow_log + year_factor, data = final)
summary(fixed_effects_time) 

# 2-Way Fixed Effects
fixed_effects_two_way <- lm(price_log ~ book_value_log + dividends_per_share_log +
                              dividends_per_share_ind + cashflow_log + year_factor + tic, data = final)
summary(fixed_effects_two_way) # Slightly higher than company fixed effects R^2 ~0.85

# Individual Random Effects (for Companies)
random_effects_companies = lmer(price_log ~ book_value_log + dividends_per_share_log +
                dividends_per_share_ind + cashflow_log + (1|tic), data = final, REML = FALSE)
summary(random_effects_companies)
# NOTE TO SELF: UNSURE ABOUT HOW TO COMPARE THIS TO THE FIXED EFFECTS
# ANOVA DOESN'T WORK FOR THIS COMPARISON

# Time Random Effects (Randome Effects for Year)
random_effects_time = lmer(price_log ~ book_value_log + dividends_per_share_log +
                                  dividends_per_share_ind + cashflow_log + (1|year_factor), 
                                data = final, REML = FALSE)
summary(random_effects_time)


# 2-Way Random Effects 
random_effects_2_way = lmer(price_log ~ book_value_log + dividends_per_share_log +
                                  dividends_per_share_ind + cashflow_log + (1|tic) + (1|year_factor), 
                            data = final, REML = FALSE)
summary(random_effects_2_way)

# NEXT STEPS: FIGURE OUT HOW TO COMPARE THE RANDOM AND FIXED EFFECTS MODELS
# MAKE SURE THE 2-WAY RANDOM EFFECTS IS FIT CORRECTLY
