# ---- Initialsing R packages ----

packages <- c("quantmod", "ucimlrepo", "frenchdata")
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) {
  install.packages(new_packages)
}
library(quantmod)
library(ucimlrepo)
library(frenchdata)

# ---- Stock price data ----

tickers <- c(
  # 1. AI & Data Infrastructure
  "NVDA", "MSFT", "GOOGL", "AMZN", "META", "AVGO", "AMD", "ORCL", "SAP", "SMCI",
  
  # 2. Consumer Staples
  "PG", "KO", "PEP", "WMT", "COST", "PM", "MO", "MDLZ", "CL", "TGT",
  
  # 3. Healthcare & Biotech
  "LLY", "JNJ", "ABBV", "PFE", "MRK", "UNH", "TMO", "ABT", "AMGN", "DHR",
  
  # 4. Financials & Banking
  "JPM", "BAC", "WFC", "C", "GS", "MS", "V", "MA", "AXP", "BLK",
  
  # 5. Technology Hardware & Software
  "AAPL", "CRM", "ADBE", "INTC", "CSCO", "QCOM", "TXN", "IBM", "NOW", "PANW",
  
  # 6. Industrials & Aerospace
  "CAT", "GE", "HON", "BA", "LMT", "RTX", "DE", "UNP", "UPS", "MMM",
  
  # 7. Consumer Discretionary
  "TSLA", "HD", "NKE", "MCD", "SBUX", "LOW", "BKNG", "TJX", "CMG", "MAR",
  
  # 8. Energy & Oil
  "XOM", "CVX", "COP", "SLB", "EOG", "MPC", "PSX", "VLO", "OXY", "HAL",
  
  # 9. Communication Services & Entertainment
  "NFLX", "DIS", "CMCSA", "T", "VZ", "TMUS", "DIS", "TTWO", "SPOT", "CHTR",
  
  # 10. Utilities & Real Estate
  "NEE", "DUK", "SO", "AEP", "SRE", "PLD", "AMT", "CCI", "EQIX", "O"
)



start_date <- "2020-01-01"
end_date <- "2026-10-30"
df_prices <- do.call(merge, lapply(tickers, \(x) Ad(getSymbols(x, from = start_date, to = end_date, auto.assign = FALSE))))
colnames(df_prices) <- tickers
df_prices <- data.frame(Date = index(df_prices), coredata(df_prices))
df_prices <- na.omit(df_prices)
df_prices$Date <- as.Date(df_prices$Date)

# ---- Fama French data ----

ff_3_daily_raw <- download_french_data("Fama/French 3 Factors [Daily]")
ff_3_daily <- as.data.frame(ff_3_daily_raw$subsets$data[[1]])

ff_3_daily$date <- as.Date(as.character(ff_3_daily$date), format = "%Y%m%d")

factor_cols_names <- c("Date", "Mkt-RF", "SMB", "HML", "RF")
names(ff_3_daily) <- factor_cols_names

factor_cols <- c("Mkt-RF", "SMB", "HML", "RF")

for (col in factor_cols) {
  ff_3_daily[[col]] <- ff_3_daily[[col]] / 100
}

ff <- ff_3_daily[ff_3_daily$Date >= as.Date(start_date) & ff_3_daily$Date <= as.Date(end_date), ]

rownames(ff) <- NULL

# ----Final df and returns  ----

prices <- df_prices[, -1]
returns <- data.frame(
  Date = df_prices$Date[-1],
  prices[-1, ] / prices[-nrow(prices), ] - 1
)


data <- merge(returns, ff, by = "Date")

data <- na.omit(data)
