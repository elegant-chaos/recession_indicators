library(tidyverse)
library(ggthemes)
library(wesanderson)
library(broom)

# Viz from paper

# 1. Mean of Share Price over Time, by Year
final %>% group_by(year) %>% summarise(mean_share_price = mean(price)) %>%
  ungroup() %>%
  ggplot() + geom_point(aes(x = year, y = mean_share_price)) +
  geom_line(aes(x = year, y = mean_share_price)) +
  xlab("Year") +
  ylab("Mean Share Price") +
  ggtitle("Mean Share Price by Year") +
  theme_economist_white()

ggsave("mean_share_price_over_time.jpg")
# 2. Share Price and Theoretical Value

# Note: augment() call takes about 20 minutes to run
two_way_fixed_fitted <- fixed_effects_two_way %>% augment(final)

two_way_fixed_fitted %>% ggplot() +
  geom_point(aes(x = .fitted, y = price_log), alpha = 0.2) + 
  xlab("Predicted Price Values (log scale)") +
  ylab("True Price Values (log scale)") +
  ggtitle("Fitted Versus True Prices") +
  theme_economist_white()

ggsave("true_vs_predicted.jpg")

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
  ggtitle("Distribution of Company Fixed Effects Coefficients") +
  theme_economist_white()

ggsave("company_fixed_effects_coeffs_distribution.jpg")

# 4. Time Fixed Effects

# Using two way fixed effects model for this viz

two_way_fixed_broom %>% filter(str_detect(term, 'year')) %>% 
  mutate(year = as.numeric(str_extract(term, '\\d+'))) %>% 
  ggplot() + geom_point(aes(x = year, y = estimate)) +
  geom_line(aes(x = year, y = estimate)) + 
  xlab("Coefficient") +
  ggtitle("Time Fixed Effects Coefficients") +
  theme_economist_white()

ggsave("time_fixed_effects_coefficients.jpg")

# 5. Mean of Divergence Rate over Time, by Year

for_divergence %>% group_by(year) %>% 
  summarise(mean_divergence = mean(divergence)) %>% 
  ggplot() + geom_point(aes(x = year, y = mean_divergence)) +
  geom_line(aes(x = year, y = mean_divergence)) + 
  xlab("Year") +
  ylab("Mean Divergence Rate") +
  ggtitle("Mean Divergence Rate by Year") +
  theme_economist_white()

ggsave("mean_divergence_rate_by_year.jpg")

# 6. Distribution of Divergence Rate, 2006-2008  

for_divergence %>% filter(year %in% c(2006, 2007, 2008)) %>% 
  mutate(year = as.factor(year)) %>%
  ggplot() + geom_density(aes(x = divergence, group = year, color = year)) +
  ggtitle("Divergence Distribution by Year (2006-2008)") +
  theme_economist_white()

ggsave("divergence_distribution_2006_to_2008.jpg")

# 7. Distribution of Divergence Rate, 2009-2013

for_divergence %>% filter(year %in% c(2009:2013)) %>%
  mutate(year = as.factor(year)) %>%
  ggplot() + geom_density(aes(x = divergence, group = year, color = year)) +
  ggtitle("Divergence Distribution by Year (2009-2013)") +
  theme_economist_white()

ggsave("divergence_dist_2009_to_2013.jpg")


# Our unique or changed plots:

# Distribution of Divergence over all Years

for_divergence %>% 
  mutate(year = as.factor(year)) %>%
  ggplot() + geom_density(aes(x = divergence, group = year, color = year)) +
  ggtitle("Divergence Distribution by Year") +
  theme_economist_white()

ggsave("divergence_dist_all_year.jpg")

# Recent Divergence
for_divergence %>% filter(year %in% c(2015:2019)) %>%
  mutate(year = as.factor(year)) %>%
  ggplot() + geom_density(aes(x = divergence, group = year, color = year)) +
  ggtitle("Divergence since 2015") +
  theme_economist_white()

ggsave("divergence_since_2015.jpg")


# FAANG Stocks
for_divergence %>% 
  filter(tic %in% c("FB", "AMZN", "NFLX", "GOOG", "GOOGL", "AAPL")) %>% 
  mutate(stock = ifelse(tic == "FB", "Facebook", 
                        ifelse(tic == "AMZN", "Amazon", 
                               ifelse(tic == "NFLX", "Netflix",
                                      ifelse(tic == "GOOG" | tic == "GOOGL", "Google", "Apple"))))) %>%
  mutate(Stock = as.factor(stock)) %>% group_by(Stock, year) %>%
  summarise(mean_divergence = mean(divergence)) %>% ungroup() %>%
  ggplot() + geom_point(aes(x = year, y = mean_divergence, color = Stock)) +
  geom_line(aes(x = year, y = mean_divergence, color = Stock)) + 
  xlab("Year") +
  ylab("Mean Divergence Rate") +
  ggtitle("FAANG Stocks Mean Divergence Rate") + 
  theme_economist_white()

ggsave("faang_stocks_divergence_dist.jpg")

# Plots for different sectors go here

  