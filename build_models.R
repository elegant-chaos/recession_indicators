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


# Time Random Effects (Random Effects for Year)
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

for_plm <- final %>% group_by(year, tic) %>% summarise(avg_price_log = mean(price_log),
                                                    avg_book_value_log = mean(book_value_log),
                                                    avg_dividends_log = mean(dividends_per_share_log), 
                                                    avg_cashflow_log = mean(cashflow_log)) #%>%
  mutate(avg_dividends_inc = ifelse(avg_dividends_log == 1, 1, 0)) %>% ungroup()

# Confirm with Hausman test

best_fixed <- plm(avg_price_log ~ avg_book_value_log + avg_dividends_log +
                    avg_dividends_inc + 
                    avg_cashflow_log + tic + year, data = for_plm,
                  index = c("tic", "year"), model = "within")
best_random <- plm(avg_price_log ~ avg_book_value_log + avg_dividends_log +
                     avg_dividends_inc + 
                     avg_cashflow_log + tic + year, data = for_plm,
                   index = c("tic", "year"), model = "random")

phtest(best_fixed, best_random)

# Analysis of residuals for fixed effects with companies only  

load(file = "RDA_files/fixed_effects_companies.rda")
ls()
plot(fixed_effects_companies)

# After analysis of residuals, trying model with normalized data



