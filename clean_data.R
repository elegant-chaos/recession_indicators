library(tidyverse)
library(stringr)
library(lubridate)
library(PerformanceAnalytics)

#dat <- read_csv("data.csv") %>% 
#  filter(curcdq == "USD")
#prices <- read_csv("prices.csv") %>% 
#  select(tic, datacqtr, prccq)

dat <- read_csv("full_data.csv") %>% 
  filter(curcdq == "USD")

dat <- dat %>% mutate(gsector = as.factor(gsector))

codes <- read_csv("gics_codes.csv")
codes <- codes %>% mutate(Code = as.factor(Code))

dat <- dat %>% left_join(codes, by = c("gsector" = "Code"))
dat <- dat %>% mutate(sector = Name) 

#clean data

#change str()
cols <- c("indfmt", "consol", "popsrc", "datafmt", "tic", "curcdq",
          "datacqtr", "datafqtr", "costat")
dat[cols] <- lapply(dat[cols], factor)
#dat$datadate <- as.Date(parse_date_time(dat$datadate,"ymd"))
#dat <- dat %>%
#  mutate(datadate = as.Date(datadate, format = "%Y-%m-%d"))

#select applicable rows
dat <- dat %>%
  mutate(book_value = cstkcvq, #carrying values / common shares = book value per share
         cashflow = cheq) %>% #cashflow/csh12q = approx. cashflow per share
  dplyr::select(-cstkcvq, -cheq,  #removing all variables changed
                -indfmt, -consol, -popsrc, -datafmt) #removing all factors with 1 level

dat <- dat %>% 
  mutate(earnings_per_share = epspxq, 
         dividends_per_share = dvpspq) %>% 
  select(-epspxq, -dvpspq)  %>% 
  mutate(price = prccq) %>% select(-prccq)

dat <- dat %>% separate(datacqtr, c("year", "quarter"), "Q")
dat <- dat %>% mutate(year = as.numeric(year),
                      quarter = as.numeric(quarter))

#prices <- prices %>% mutate(price = prccq) %>% select(-prccq)

#dat <- dat %>% inner_join(prices, by = c("tic", "datacqtr"))


#for analysis
final <- dat %>% select(cashflow, book_value, dividends_per_share, price,
                        tic, year, quarter, sector) %>%
  filter(!is.na(cashflow)) %>% filter(!is.na(book_value)) %>% filter(!is.na(dividends_per_share)) %>%
  filter(!is.na(price)) %>% filter(!is.na(year))
rm(dat)
#rm(prices)



