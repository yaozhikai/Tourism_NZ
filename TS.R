options(scipen = 999)

library(ggplot2)

ts_df <- read.csv('Timespan.csv')

colnames(ts_df)[1] <- "Year Month"
colnames(ts_df) <- ts_df[1, ]  
ts_df <- ts_df[-1, ] 

ts_df_total <- ts_df[, -c(2:64)]

ts_df_total$`Year Month` <- as.Date(paste(ts_df_total$`Year Month`, "-01", sep=""), format="%Y-%m-%d")

###I only put visitor/tourist visa and total visitor(no matter the purpose/visa) here.
ts_df_total$Visitor <- as.numeric(ts_df_total$Visitor)
ts_df_total$TOTAL <- as.numeric(ts_df_total$TOTAL)

start_year <- 2003
start_month <- 7
ts_visitor <- ts(ts_df_total$Visitor, start = c(start_year, start_month), frequency = 12)
ts_total <- ts(ts_df_total$TOTAL, start = c(start_year, start_month), frequency = 12)

plot(ts_visitor, col = "blue", ylim = range(c(ts_visitor, ts_total)),
     ylab = "Number of Visitors", xlab = "Year", main = "Time Series of Visitors to New Zealand",
     xaxt = "n", yaxt = "n")  

axis(2, at = pretty(range(c(ts_visitor, ts_total))), 
     labels = format(pretty(range(c(ts_visitor, ts_total))), big.mark = ","), las = 1)

years <- seq(start_year, 2024, by = 1)
axis(1, at = years, labels = years)

lines(ts_total, col = "red")
legend("topright", legend = c("Visitors Visa", "Total Visitors"),
       col = c("blue", "red"), lty = 1)
