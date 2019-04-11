library(tidyverse)
library(readr)
library(lubridate)

dat <- read_csv("~/Desktop/backup downloaded recession_indicators-master/data_05_10.csv")

#clean data

#change str()
cols <- c("indfmt", "consol", "popsrc", "datafmt", "tic", "conm", "curcdq",
          "datacqtr", "datafqtr", "costat")
dat[cols] <- lapply(dat[cols], factor)
dat$datadate <- as.Date(parse_date_time(dat$datadate,"ymd"))
dat <- dat %>%
  mutate(datadate = as.Date(datadate, format = "%Y-%m-%d"))

#select applicable rows
dat <- dat %>%
  mutate(book_value_per_share = cstkcvq/csh12q, #carrying values / common shares = book value per share
         cashflow_per_share = cheq/csh12q) %>% #cashflow/csh12q = approx. cashflow per share
  dplyr::select(-cstkcvq, -cheq, -csh12q, #removing all variables changed
         -indfmt, -consol, -popsrc, -datafmt) #removing all factors with 1 level

names(dat)[12] <- "dividends_per_share"
names(dat)[10] <- "earnings_per_share"
