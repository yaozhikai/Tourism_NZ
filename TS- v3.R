options(scipen = 999)

library(ggplot2)

ts_df <- read.csv('Timespan.csv')

colnames(ts_df)[1] <- "Year Month"
colnames(ts_df) <- ts_df[1, ]
ts_df <- ts_df[-1, ]

ts_df_total <- ts_df[, -c(2:64)]

colnames(ts_df_total)[1] <- "Year_Month"

ts_df_total$`Year_Month` <- as.character(ts_df_total$`Year_Month`)

ts_df_total$Year <- as.numeric(substr(ts_df_total$`Year_Month`, 1, 4))
ts_df_total$Month <- as.numeric(substr(ts_df_total$`Year_Month`, 6, 7))

ts_df_total$`Year Month` <- as.Date(paste(ts_df_total$Year, ts_df_total$Month, "01", sep = "-"), format = "%Y-%m-%d")

ts_df_total$Visitor <- as.numeric(ts_df_total$Visitor)
ts_df_total$TOTAL <- as.numeric(ts_df_total$TOTAL)

start_year <- 2003
start_month <- 7
ts_visitor <- ts(ts_df_total$Visitor, start = c(start_year, start_month), frequency = 12)
ts_total <- ts(ts_df_total$TOTAL, start = c(start_year, start_month), frequency = 12)

# ts plot
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

#recovery percentage


# lm on season (whole time span)
get_season <- function(month) {
  if (month %in% c(12, 1, 2)) {
    return("Summer")
  } else if (month %in% c(3, 4, 5)) {
    return("Autumn")
  } else if (month %in% c(6, 7, 8)) {
    return("Winter")
  } else {
    return("Spring")
  }
}

ts_df_total$Season <- sapply(ts_df_total$Month, get_season)

ts_df_total$Season <- factor(ts_df_total$Season, levels = c("Summer", "Autumn", "Winter", "Spring"))

visitor_lm <- lm(Visitor ~ Season, data = ts_df_total)

summary(visitor_lm)

ggplot(ts_df_total, aes(x = Season, y = Visitor)) +
  geom_boxplot(fill = "lightblue") + 
  geom_point(position = position_jitter(width = 0.1), alpha = 0.5) +  
  geom_smooth(method = "lm", se = FALSE, col = "red") + 
  labs(title = "Visitors vs. Season in New Zealand",
       x = "Season",
       y = "Number of Visitors") +
  theme_minimal()


##lm prior covid
ts_df_filtered <- subset(ts_df_total, `Year Month` <= as.Date("2020-03-01"))
ts_df_filtered$Season <- sapply(ts_df_filtered$Month, get_season)


ts_df_filtered$Season <- factor(ts_df_filtered$Season, levels = c("Summer", "Autumn", "Winter", "Spring"))

visitor_lm_filtered <- lm(Visitor ~ Season, data = ts_df_filtered)

summary(visitor_lm_filtered)

ggplot(ts_df_filtered, aes(x = Season, y = Visitor)) +
  geom_boxplot(fill = "lightblue") +  
  geom_point(position = position_jitter(width = 0.1), alpha = 0.5) +  
  geom_smooth(method = "lm", se = FALSE, col = "red") +  
  labs(title = "Visitors vs. Season in New Zealand (Before March 2020)",
       x = "Season",
       y = "Number of Visitors") +
  theme_minimal()
