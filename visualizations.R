library(tidyverse)
library(broom)

# Viz from paper

# 1. Mean of Share Price over Time, by Year
final %>% group_by(year) %>% summarise(mean_share_price = mean(price)) %>%
  ungroup() %>%
  ggplot() + geom_point(aes(x = year, y = mean_share_price)) +
  geom_line(aes(x = year, y = mean_share_price)) +
  xlab("Year") +
  ylab("Mean Share Price") +
  ggtitle("Mean Share Price by Year")

# 2. Share Price and Theoretical Value

# CURRENTLY USING THE TWO WAY FIXED EFFECTS MODEL
# UPDATE THIS MODEL IF TESTS SHOW DIFFERENT RESULTS THAN THE PAPER HAS
# PAPER USES TWO WAY FIXED EFFECTS 

# Note: augment() call takes about 20 minutes to run
two_way_fixed_fitted <- fixed_effects_two_way %>% augment(final)

two_way_fixed_fitted %>% ggplot() +
  geom_point(aes(x = .fitted, y = price_log), alpha = 0.2) + 
  xlab("Predicted Price Values (log scale)") +
  ylab("True Price Values (log scale)") +
  ggtitle("Fitted Versus True Prices")

# Messing around with a line for every company 
# two_way_fixed_fitted %>% ggplot() +
#   geom_line(aes(x = year, y = .fitted, group = tic, color = tic)) +
#   theme_void() + theme(legend.position="none")


# 3. Individual Fixed Effects (AKA Company Fixed Effects)

# Using two way fixed effects model for this viz

two_way_fixed_broom <- fixed_effects_two_way %>% tidy()

two_way_fixed_broom %>% filter(str_detect(term, 'tic')) %>% 
  ggplot() + geom_density(aes(x = estimate)) + 
  xlab("Coefficient") +
  ggtitle("Distribution of Company Fixed Effects Coefficients")

# 4. Time Fixed Effects

# Using two way fixed effects model for this viz

two_way_fixed_broom %>% filter(str_detect(term, 'year')) %>% 
  mutate(year = as.numeric(str_extract(term, '\\d+'))) %>% 
  ggplot() + geom_point(aes(x = year, y = estimate)) +
  geom_line(aes(x = year, y = estimate)) + 
  xlab("Coefficient") +
  ggtitle("Time Fixed Effects Coefficients")

# 5. Mean of Divergence Rate over Time, by Year

for_divergence %>% group_by(year) %>% 
  summarise(mean_divergence = mean(divergence)) %>% 
  ggplot() + geom_point(aes(x = year, y = mean_divergence)) +
  geom_line(aes(x = year, y = mean_divergence)) + 
  xlab("Year") +
  ylab("Mean Divergence Rate") +
  ggtitle("Mean Divergence Rate by Year")

# 6. Distribution of Divergence Rate, 2006-2008  

# THIS PLOT CONFIRMS WHAT IS OBSERVED IN PART 5
# OUR MODEL FLIPS THE SIGN ON DIVERGENCE
# HIGHER DIVERGENCE VALUES CORRESPOND TO 2008 RECESSION

for_divergence %>% filter(year %in% c(2006, 2007, 2008)) %>% 
  mutate(year = as.factor(year)) %>%
  ggplot() + geom_density(aes(x = divergence, group = year, color = year)) +
  ggtitle("Divergence Distribution by Year (2006-2008)")

# 7. Distribution of Divergence Rate, 2009-2010 

# NEED TO PULL THROUGH 2013 TO MATCH THE PAPER ON THIS PLOT


for_divergence %>% filter(year %in% c(2009:2013)) %>%
  mutate(year = as.factor(year)) %>%
  ggplot() + geom_density(aes(x = divergence, group = year, color = year)) +
  ggtitle("Divergence Distribution by Year (2009-2013)") 


# Our unique or changed plots:

# 8. Distribution of Divergence over all Years

for_divergence %>% 
  mutate(year = as.factor(year)) %>%
  ggplot() + geom_density(aes(x = divergence, group = year, color = year)) +
  ggtitle("Divergence Distribution by Year")
