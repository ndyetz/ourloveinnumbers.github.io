#This program was created on 9/26/24 by Neil Yetz
#The purpose of this program is to calculate the beta values for a given portfolio over a specified time compared to an index (I.E. S&P 500)
#The end product is an excel file with betas of all stocks indicated in "symbols" and corresponding fit statistics.
# I mimicked code from this helpful guide: https://rviews.rstudio.com/2018/02/08/capm-beta/  

#Clear environment
rm(list = ls())

#Load Libraries
library(tidyquant)
library(tidyverse)
library(timetk)
library(tibbletime)
library(broom)
library(writexl)


#Custom functions
myfun <- function(x) {
  df <- get(x)
  return(df)
}

sp500 <- read_csv("D:/Git/ourloveinnumbers.github.io/sp500.csv")

sp500 <- sp500 %>% 
  mutate(ticker = ifelse(ticker == "BRK.B", "BRK-B", ticker),
         ticker = ifelse(ticker == "BF.B", "BF-B", ticker)
         )



#Add stock ticker names. Ticker name must be the EXACT name (case sensitive)
#symbols <- c(sp500$ticker)

symbols <- c("SPY",
             "AMZN",
             "ABNB",
             "UBER",
             "CMG",
             "ALK",
             "MODG",
             "GOLF",
             "MTN",
             "SBUX",
             "MTCH"
)

# Descriptive names of portfolio - must match order of symbols
Asset_name <- as_tibble(c("S&P 500", "Amazon", "AirBnB", "Uber", "Chipotle", "Alaska Airlines", "Callaway", "Titleist", "Vail Resorts","Starbucks", "Tinder"))
Asset_name <- Asset_name %>% rename(`Asset Name` = "value")


#Start and end dates of interest, must be in "YYYY-MM-DD" format
start_date <- "2022-10-03"
end_date   <- "2024-09-20"


#Pull Prices
prices <- 
  getSymbols(symbols, src = 'yahoo', 
             from = start_date ,
             to = end_date,
             auto.assign = TRUE, warnings = FALSE) %>% 
  map(~Cl(get(.))) %>%                          # <- Currently getting the adjusted closing price... Would "Close" (Cl) be better?
  reduce(merge) %>% 
  `colnames<-`(symbols)


# Stocks to remove because they provide incomplete data.
remove <- prices %>% as_tibble() %>%  select_if(~  any(is.na(.))) 


#Removes any columns with an NA value
prices <- prices[ , colSums(!is.na(prices)) == nrow(prices)] 

#Check if NA remove worked. If correct, This should output a nrowsx0 table
prices %>% as_tibble() %>%  select_if(~  any(is.na(.))) 
  
#yahoo_symbols <- ls()
#lapply(yahoo_symbols, myfun)
#yahoo_symbols <- as_tibble(yahoo_symbols)
#
#yahoo_symbols <- yahoo_symbols %>% 
#  rename(ticker = "value")
#
#sp500_check <- sp500 %>%
#  anti_join(yahoo_symbols, by = "ticker")
#
#

# Set to monthly... I don't think we need to do it monthly, I am only doing that because the example was that way.
prices_monthly <- to.monthly(prices, indexAt = "last", OHLC = FALSE)

#Return calculator function (Bit of a black box to me, Karina can help?)
asset_returns_xts <- na.omit(Return.calculate(prices_monthly, method = "log"))

portfolio_returns_xts_rebalanced_monthly <- 
  Return.portfolio(asset_returns_xts, rebalance_on = "months") %>%
  `colnames<-`("returns") 


# Getting indexes at monthly and Returns = log(returns) - log(lag(returns)) lag is currently lagging by 1 (deafult). Ask Karina if this makes sense?
asset_returns_long <-  
  prices %>% 
  to.monthly(indexAt = "last", OHLC = FALSE) %>% 
  tk_tbl(preserve_index = TRUE, rename_index = "date") %>%
  gather(asset, returns, -date) %>% 
  group_by(asset) %>%  
  mutate(returns = (log(returns) - log(lag(returns)))) %>% 
  na.omit()

# Aggregate the group of returns by asset
portfolio_returns_tq_rebalanced_monthly <- 
  asset_returns_long %>%
  tq_portfolio(assets_col  = asset, 
               returns_col = returns,
               col_rename  = "returns",
               rebalance_on = "months")


#SP500 Index comparison
spy_monthly_xts <- 
  getSymbols("SPY", 
             src = 'yahoo', 
             from = start_date ,
             to = end_date,
             auto.assign = TRUE, 
             warnings = FALSE) %>% 
  map(~Ad(get(.))) %>%                                   # <- Currently getting the adjusted closing price... Would "Close" (Cl) be better?
  reduce(merge) %>%
  `colnames<-`("SPY") %>% 
  to.monthly(indexAt = "last", OHLC = FALSE)

market_returns_xts <-
  Return.calculate(spy_monthly_xts, method = "log") %>% 
  na.omit()


market_returns_tidy <-
  market_returns_xts %>% 
  tk_tbl(preserve_index = TRUE, rename_index = "date") %>% 
  na.omit() %>%
  select(date, returns = SPY)

#
#portfolio_returns_tq_rebalanced_monthly %>% 
#  mutate(market_returns = market_returns_tidy$returns) %>%
#  head()


#Get overall portfolio beta
#cov(portfolio_returns_xts_rebalanced_monthly,market_returns_tidy$returns)/var(market_returns_tidy$returns)


#Run models, tidy betas, keep fit statistics
beta_assets <- 
  asset_returns_long %>% 
  na.omit() %>% 
  nest(data = -asset) %>% 
  mutate(model = map(data, ~ lm(returns ~ market_returns_tidy$returns, data = .))) %>%
  mutate(model = map(model, tidy)) %>% 
  unnest(model) %>% 
  filter(term == "market_returns_tidy$returns") %>% 
  select(-data)

#Adds in descriptive names of tickers
beta_assets <- bind_cols(Asset_name, beta_assets) 

#Format final dataset
final_betas <- beta_assets %>% 
  mutate(term = "Market Returns",
         Dates = paste(start_date, "-", end_date)) %>% 
  select(`Asset Name`, Ticker = "asset", term, Dates, `Beta` = "estimate", "Standard Error" = std.error, `t-value` = "statistic", "p-value" = p.value)



#Output Betas to excel & .csv
write_xlsx(final_betas, "D:/Git/ourloveinnumbers.github.io/final_betas.xlsx")
write_csv(final_betas, "D:/Git/ourloveinnumbers.github.io/final_betas.csv")
#write_csv(final_betas, "D:/Git/ourloveinnumbers.github.io/SP500_final_betas.csv")
