#####################################################################################
# Step 1
# Download data for last 3 years for the DJIA (Dow Jones Industrial Average) and each
# of the 30 component stocks.
#####################################################################################
# First date = 2013-07-30, Last date = 2016-07-30

print("===== Download DJIA and 30 compound stocks =====")
download_data = function(url) {
  raw = read.table(url, header=TRUE, sep=",")
  raw = raw[, c(1, 7)] # 1 is Date column, 7 is Adj.Close column
  raw = raw[nrow(raw):1, ] # Sort oldest to newest
  return(raw)
}
djia_url = "http://chart.finance.yahoo.com/table.csv?s=^DJI&a=7&b=30&c=2013&d=7&e=30&f=2016&g=m&ignore=.csv"
compounds = c("CVX", "INTC", "MMM", "DIS", "GE", "V", "TRV", "DD", "WMT", "IBM",
  "XOM", "AXP", "HD", "MCD", "CSCO", "MRK", "JPM", "PFE", "JNJ", "UTX",
  "UNH", "PG", "KO", "CAT", "MSFT", "GS", "AAPL", "NKE", "VZ", "BA")

# DJIA
djia = download_data(djia_url)

# 30 compound stocks
compound_data = numeric(30)
for (i in 1: 30) {
  url = paste("http://chart.finance.yahoo.com/table.csv?s=", compounds[i], "&a=7&b=30&c=2013&d=7&e=30&f=2016&g=m&ignore=.csv", sep="")
  compound_data[i] = download_data(url)
}
print("Done")
print("")
print("")
#####################################################################################
# Step 2
# Calculate Monthly returns of the DJIA index and the downloaded stocks over the
# period under study.
#####################################################################################
print("===== Monthly Returns of each stock =====")
monthly_returns = function(data) {
  close_values = data[, 2] # 2 is Adj.Close column
  num = length(close_values) - 1
  returns = numeric(num)
  for (i in 1:num) {
    returns[i] = (close_values[i + 1] - close_values[i]) / close_values[i]
  }
  return(returns)
}

print("DJIA Monthly Returns")
djia_returns = monthly_returns(djia)

print("30 Compound Stock Returns")
compound_returns = numeric(30)
for (i in 1: 30) {
  compound_returns[i] = monthly_returns(compound_data[i])
}

print("")
print("")
#####################################################################################
# Step 3
# Calculate mean and standard deviation of monthly returns for the DJIA index
#####################################################################################
print("==== Mean of DJIA ====")
djia_mean = mean(djia_returns)
djia_mean
print("==== SD of DJIA ====")
djia_sd = sd(djia_returns)
djia_sd

#####################################################################################
# Step 4
# Choose an equal weighted portfolio consisting of any 5 random stocks from the DJIA,
# calculate the mean monthly returns and its standard deviation. Do the same for
# portfolios of 10,15, 20 and 25 random stocks from the DJIA universe.
#####################################################################################
mean_monthly_returns = function(n) {
  cases = combn(seq(1,30), n)
  cum = numeric(length(compound_returns[1]))
  for (i in 1: n) {
    cum = cum + compound_returns[cases[i]]
  }
  return(cum / n)
}

mean_of_randoms = numeric(5)
sd_of_randoms = numeric(5)
for (i in 1: 5) {
  mean_of_randoms[i] = mean_monthly_returns(5 * i)
  sd_of_randoms[i] = sd(mean_of_randoms[i])
  print(paste("==== Mean of ", 5 * i, " random stocks from DJIA ===="))
  print(mean_of_randoms[i])
  print(paste("==== SD of ", 5 * i, " random stocks from DJIA ===="))
  print(sd_of_randoms[i])
}
print("")
print("")

#####################################################################################
# Step 5
# Calculate tracking errors for each of the portfolios i.e. the margin by which the
# mean and standard deviation of the portfolio returns diverge from those of DJIA.
#####################################################################################
mean_tracking_errors = numeric(5)

for (i in 1: 5) {
  mean_tracking_errors[i] = mean(mean_of_randoms[i] - djia_returns)
  print(paste("==== Tracking Error of ", 5 * i, " random stocks portfolio"))
  print(mean_tracking_errors[i])
}

print("")
print("")
#####################################################################################
# Step 6
# Graphically represent the tracking error for returns and risk (standard deviation
# of returns used as a proxy for risk) on y-axis against the sample size of portfolio
# on the x-axis.
#####################################################################################

plot(mean_tracking_errors,
  c("5", "10", "15", "20", "25"),
  xlab="Sample size",
  ylab="Average returns")
