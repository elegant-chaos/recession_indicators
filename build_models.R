library(lme4)
library(plm)

# Pooled OLS (AKA Normal Linear Regression)

# With dividends_per_share_log and dividends_per_share_ind
pooled_ols <- lm(price_log ~ book_value_log + dividends_per_share_log + 
                   dividends_per_share_ind + cashflow_log, data = final)
summary(pooled_ols)

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
summary(fixed_effects_companies) # R^2 (~0.7861)

# Time Fixed Effects (Fixed Effects for Year)
final <- final %>% mutate(year_factor = as.factor(year))
fixed_effects_time <- lm(price_log ~ book_value_log + dividends_per_share_log +
                                dividends_per_share_ind + cashflow_log + year_factor, data = final)
summary(fixed_effects_time) 

# 2-Way Fixed Effects
fixed_effects_two_way <- lm(price_log ~ book_value_log + dividends_per_share_log +
                              dividends_per_share_ind + cashflow_log + year_factor + tic, data = final)
summary(fixed_effects_two_way) # Slightly higher than company fixed effects R^2 ~0.80

# Individual Random Effects (for Companies)
random_effects_companies = lmer(price_log ~ book_value_log + dividends_per_share_log +
                dividends_per_share_ind + cashflow_log + (1|tic), data = final, REML = FALSE)
summary(random_effects_companies)


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

# F-tests on fixed and pooled OLS models
anova(pooled_ols, fixed_effects_companies) #significant difference
anova(fixed_effects_time, fixed_effects_two_way) # sig difference
anova(fixed_effects_time, fixed_effects_companies)  # sig difference
anova(fixed_effects_companies, fixed_effects_two_way) #sig difference
# overall best "normal" model is fixed_effects_two_way

# F-tests for random effects models  
anova(random_effects_time, random_effects_companies) # sig difference
anova(random_effects_companies, random_effects_2_way) # sig difference
# overall best random effects model is 2-way

# F-test to compare two best models
anova(random_effects_2_way, fixed_effects_two_way) # sig difference

# Overall best model: 2-way fixed effects
# This makes sense since it's pretty unlikely the various company stock 
# prices come from a normal distribution. The stock market is exponential and stock prices are long tailed.

# Confirm with Hausman test
# This code doesn't work. I think we can skip it. 
# Could also verify in a more machine learning way by do an analysis of the residuals
#best_fixed <- plm(price_log ~ book_value_log + dividends_per_share_log +
#                    dividends_per_share_ind + cashflow_log + year_factor + tic, data = final, model = "within")
#best_random <- plm(price_log ~ book_value_log + dividends_per_share_log +
#                     dividends_per_share_ind + cashflow_log + (1|tic) + (1|year_factor), data = final, model = "random")

#phtest(best_fixed, best_random)
