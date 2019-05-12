library(tidyverse)

# Price
final %>% ggplot() + geom_density(aes(x = price))
# log transforming price
final <- final %>% mutate(price_log = log(price))
final %>% ggplot() + geom_density(aes(x = price_log))

# Dividends Per Share
# Notice: dividends_per_share is mostly zero values, 
#so need to handle differently (maybe zero and non-zero?)
final %>% ggplot() + geom_density(aes(x = dividends_per_share))
final %>% summarise(mean_dividends_per_share = mean(dividends_per_share),
                    zero_count = sum(dividends_per_share == 0),
                    zero_prop = zero_count/n())
# Plan for dividends per share:
# 1. create an indicator for zero
# 2. add 1 to mean_dividends_per_share, so that a transform can be applied
# Can try models with both and with just the indicator
final <- final %>% mutate(dividends_per_share_ind = ifelse(dividends_per_share == 0, 1, 0),
                          dividends_per_share = dividends_per_share + 1)
final <- final %>% mutate(dividends_per_share_log = log(dividends_per_share))
final %>% ggplot() + geom_density(aes(x = dividends_per_share_log))

# Cashflow
final <- final %>% mutate(cashflow_log = log(cashflow))
final %>% ggplot() + geom_density(aes(x = cashflow_log))

# Book Value
final %>% ggplot() + geom_density(aes(x = book_value))
final <- final %>% mutate(book_value_log = log(book_value))
final %>% ggplot() + geom_density(aes(x = book_value_log))

# Summary post transforms
summary(final)

# Dealing with the -inf values in book_value_share_log
final %>% summarise(total_neg_inf = sum(book_value_log == -Inf))
final %>% filter(book_value_log != -Inf) %>% summarise(min_book_value_log = min(book_value_log))
final <- final %>% mutate(book_value_log = ifelse(book_value_log == -Inf, -10, book_value_log))

# Removing single NA value in cashflow_log introduced with log transform
final <- final %>% filter(!is.na(cashflow_log)) 

# Dealing with the -Inf values in cashflow_log
final %>% summarise(total_neg_inf = sum(cashflow_log == -Inf))
final %>% filter(cashflow_log != -Inf) %>% summarise(min_cashflow_log = min(cashflow_log))
final <- final %>% mutate(cashflow_log = ifelse(cashflow_log == -Inf, -7, cashflow_log))

# Summary post fix 
summary(final)

# Correlation Plot
chart.Correlation(final[,c('price_log', 'dividends_per_share_log', 
                           'book_value_log', 'cashflow_log')], histogram=TRUE, pch=19)
