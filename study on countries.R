library(readxl)
library(ggplot2)

#read total visitors of each country
countrydata <- read_excel("international-visitor-arrivals-to-new-zealand-may-2024.xlsx", 
                          sheet = "Table 4", range = "A6:G42")

colnames(countrydata) <- as.character(countrydata[1, ]) 
countrydata <- countrydata[-1, ] 
countrydata <- countrydata[rowSums(is.na(countrydata)) != ncol(countrydata), ]  
countrydata <- countrydata[, colSums(is.na(countrydata)) != nrow(countrydata)] 
countrydata <- countrydata[-c(1, 2), ]
colnames(countrydata)[1] <- "Home Countries"

#find top 10 home countries of visitors
countrydata[, 2:6] <- lapply(countrydata[, 2:6], as.numeric)

countrydata$Total <- rowSums(countrydata[, 2:6], na.rm = TRUE)
top10 <- countrydata[order(-countrydata$Total), ][1:10, ]
top10[, 2:6] <- lapply(top10[, 2:6], as.numeric)

#plotting changes in trends among top 10 countries
years <- 2020:2024
top10$`Home Countries` <- as.factor(top10$`Home Countries`)

library(tidyr)
top10_long <- pivot_longer(top10, cols = c("2020", "2021", "2022", "2023", "2024"),
                           names_to = "Year", values_to = "Arrivals")

top10_long$Year <- as.numeric(top10_long$Year)

ggplot(top10_long, aes(x = Year, y = Arrivals, color = `Home Countries`, group = `Home Countries`)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +  
  labs(title = "Trends of Top 10 Home Countries of Visitors（2020~2024）",
       x = "Year", y = "Number of Visitors",
       color = "Home Countries") +
  scale_y_continuous(labels = scales::comma) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###investigate growth rate among top 10 countries
top10_growth <- top10 

top10_growth$`2021_growth` <- (top10$`2021` - top10$`2020`) / top10$`2020` * 100
top10_growth$`2022_growth` <- (top10$`2022` - top10$`2021`) / top10$`2021` * 100
top10_growth$`2023_growth` <- (top10$`2023` - top10$`2022`) / top10$`2022` * 100
top10_growth$`2024_growth` <- (top10$`2024` - top10$`2023`) / top10$`2023` * 100

growth_rate_data <- top10_growth[, c("Home Countries", "2021_growth", "2022_growth", "2023_growth", "2024_growth")]

print(growth_rate_data)

growth_rate_long <- pivot_longer(growth_rate_data, cols = c("2021_growth", "2022_growth", "2023_growth", "2024_growth"),
                                 names_to = "Year", values_to = "Growth Rate")

growth_rate_long$Year <- as.numeric(sub("_growth", "", growth_rate_long$Year))

ggplot(growth_rate_long, aes(x = Year, y = `Growth Rate`, color = `Home Countries`, group = `Home Countries`)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) + 
  labs(title = "Top 10 Home Countries (2021~2024) Growth Rate of Visitors",
       x = "Year", y = "Growth Rate (%)",
       color = "Home Countries") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

