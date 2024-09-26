#Clear environment
rm(list = ls())


#Load Libraries
library(tidyquant)
library(tidyverse)
library(timetk)
library(tibbletime)
library(broom)


#Our "Portfolio"
symbols <- c("SPY", "AMZN", "ABNB", "UBER","CMG", "ALK", "MODG", "GOLF", "MTN" , "SBUX")

prices <- 
  getSymbols(symbols, src = 'yahoo', 
             from = "2022-06-27" ,
             to = "2024-09-23",
             auto.assign = TRUE, warnings = FALSE) %>% 
  map(~Ad(get(.))) %>%
  reduce(merge) %>% 
  `colnames<-`(symbols)



prices_monthly <- to.monthly(prices, indexAt = "last", OHLC = FALSE)

asset_returns_xts <- na.omit(Return.calculate(prices_monthly, method = "log"))

portfolio_returns_xts_rebalanced_monthly <- 
  Return.portfolio(asset_returns_xts,, rebalance_on = "months") %>%
  `colnames<-`("returns") 


asset_returns_long <-  
  prices %>% 
  to.monthly(indexAt = "last", OHLC = FALSE) %>% 
  tk_tbl(preserve_index = TRUE, rename_index = "date") %>%
  gather(asset, returns, -date) %>% 
  group_by(asset) %>%  
  mutate(returns = (log(returns) - log(lag(returns)))) %>% 
  na.omit()

portfolio_returns_tq_rebalanced_monthly <- 
  asset_returns_long %>%
  tq_portfolio(assets_col  = asset, 
               returns_col = returns,
               col_rename  = "returns",
               rebalance_on = "months")




#SP500
spy_monthly_xts <- 
  getSymbols("SPY", 
             src = 'yahoo', 
             from = "2022-06-27" ,
             to = "2024-09-23",
             auto.assign = TRUE, 
             warnings = FALSE) %>% 
  map(~Ad(get(.))) %>% 
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


cov(portfolio_returns_xts_rebalanced_monthly,market_returns_tidy$returns)/var(market_returns_tidy$returns)



beta_assets <- 
  asset_returns_long %>%
  na.omit() %>% 
  nest(data = -asset)




beta_assets <- 
  asset_returns_long %>% 
  na.omit() %>% 
  nest(data = -asset) %>% 
  mutate(model = map(data, ~ lm(returns ~ market_returns_tidy$returns, data = .))) 



beta_assets <- 
  asset_returns_long %>% 
  na.omit() %>% 
  nest(data = -asset) %>% 
  mutate(model = map(data, ~ lm(returns ~ market_returns_tidy$returns, data = .))) %>%
  mutate(model = map(model, tidy))



beta_assets <- 
  asset_returns_long %>% 
  na.omit() %>% 
  nest(data = -asset) %>% 
  mutate(model = map(data, ~ lm(returns ~ market_returns_tidy$returns, data = .))) %>%
  mutate(model = map(model, tidy)) %>% 
  unnest(model) %>% 
  filter(term == "market_returns_tidy$returns") %>% 
  select(-data)


`Asset_name` <- as_tibble(
                  c("S&P 500",
                   "Amazon",
                   "AirBnB",
                   "Uber",
                   "Chipotle",
                   "Alaska Airlines",
                   "Callaway",
                   "Titleist",
                  "Vail Resorts",
                  "Starbucks")
                  )

`Asset_name` <- Asset_name %>% 
  rename(`Asset Name` = "value")

beta_assets <- bind_cols(`Asset_name`, beta_assets)

final_betas <- beta_assets %>% 
  mutate(term = "Market Returns",
         Dates = "2022-06-27 - 2024-09-23") %>% 
  select(`Asset Name`, Ticker = "asset", term, Dates, `Beta` = "estimate", "Standard Error" = std.error, `t-value` = "statistic", "p-value" = p.value)
  
  


library(writexl)


write_xlsx(final_betas, "D:/Git/ourloveinnumbers.github.io/final_betas.xlsx")


