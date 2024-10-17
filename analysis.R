library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(openxlsx)

# Load the cleaned data from the Excel file instead of RDS
combined_data <- read.xlsx("cleaned_combined_data_23.xlsx")
print(colnames(combined_data))

# Rename "Travel.purpose" to "Travel purpose"
combined_data <- combined_data %>%
  rename(`Travel purpose` = `Travel.purpose`)

# Convert columns to numeric
combined_data <- combined_data %>%
  mutate(across(c("2019", "2020", "2021", "2022", "2023"), ~ as.numeric(as.character(.))))

# Ensure 'Month' is treated as a factor with correct order
combined_data$Month <- factor(combined_data$Month, levels = c("January", "February", "March", "April", "May", "June", 
                                                              "July", "August", "September", "October", "November", "December"))

# Step 1: Pivot data into long format for plotting
long_data <- combined_data %>%
  pivot_longer(cols = c("2019", "2020", "2021", "2022", "2023"), names_to = "Year", values_to = "Value")

# Step 2: Plot Monthly Travel Purpose Trends by Year 2019-2023
ggplot(long_data, aes(x = Month, y = Value, color = Year, group = Year)) +
  geom_line() +
  facet_wrap(~ `Travel purpose`, scales = "free_y") +
  labs(title = "Monthly Travel Purpose Trends by Year 2019-2023", 
       x = "Month", 
       y = "Number of Visitors", 
       color = "Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = comma)

# Step 3: Plot Holiday vs Other Travel Purposes by Year and Month, 2019-2023
ggplot(long_data, aes(x = Month, y = Value, color = `Travel purpose`, group = interaction(Year, `Travel purpose`))) +
  geom_line(size = 0.8) +  
  scale_color_manual(values = c("Holiday" = "red", "Business" = "blue", 
                                "Conferences & conventions" = "green", 
                                "Visiting friends & relatives" = "purple", 
                                "Education" = "orange")) +
  facet_wrap(~ Year, scales = "free_y") +
  labs(title = "Holiday vs Other Travel Purposes by Year and Month, 2019-2023", 
       x = "Month", 
       y = "Number of Visitors", 
       color = "Travel Purpose") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::comma)

# Step 4: Bar Chart for Seasonal Travel Purpose Trends by Year, 2019-2023
ggplot(long_data, aes(x = Season, y = Value, fill = Year)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ `Travel purpose`, scales = "free_y") +
  labs(title = "Seasonal Travel Purpose Trends by Year, 2019-2023", 
       x = "Season", 
       y = "Number of Visitors", 
       fill = "Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::comma)

# Step 5: Bar Chart for Holiday vs Other Travel Purposes by Season and Year, 2019-2023
ggplot(long_data, aes(x = Season, y = Value, fill = `Travel purpose`)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Holiday" = "red", "Business" = "blue", "Conferences & conventions" = "green",
                               "Visiting friends & relatives" = "purple", "Education" = "orange")) +
  facet_wrap(~ Year, scales = "free_y") +
  labs(title = "Holiday vs Other Travel Purposes by Season and Year, 2019-2023", 
       x = "Season", 
       y = "Number of Visitors", 
       fill = "Travel Purpose") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::comma)
