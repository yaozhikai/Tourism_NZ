library(readxl)
library(openxlsx)
library(dplyr)

# 2020-2024
# Define a list of file paths for each month
monthly_list <- c("1.xlsx", "2.xlsx", "3.xlsx", "4.xlsx", "5.xlsx", "6.xlsx", "7.xlsx")

# Read and clean the "Table 5" data from all files
all_tables <- lapply(monthly_list, function(file) {
  read_excel(file, sheet = "Table 5")
})

# Write the cleaned data into a new Excel file with multiple sheets
sheet_names <- c("January", "February", "March", "April", "May", "June", "July")
output_file <- "final_combined_table_5.xlsx"
wb <- createWorkbook()

# Write data to different sheets
for (i in seq_along(all_tables)) {
  addWorksheet(wb, sheet_names[i])
  writeData(wb, sheet = sheet_names[i], all_tables[[i]])
}
saveWorkbook(wb, output_file, overwrite = TRUE)

# Extract rows 7-11, clean, and combine data from all sheets
all_data <- lapply(seq_along(sheet_names), function(i) {
  table_5 <- read.xlsx(output_file, sheet = sheet_names[i])
  filtered_rows <- table_5[7:11, -1]
  filtered_rows <- cbind(Month = sheet_names[i], filtered_rows)
  filtered_rows <- filtered_rows[, -c(ncol(filtered_rows)-1, ncol(filtered_rows))]
  filtered_rows
})

combined_data <- do.call(rbind, all_data)

# Rename columns
colnames(combined_data)[2:7] <- c("Travel purpose", "2020", "2021", "2022", "2023", "2024")

# Write the combined data to a new sheet
addWorksheet(wb, "Filtered_Data")
writeData(wb, sheet = "Filtered_Data", combined_data)
saveWorkbook(wb, output_file, overwrite = TRUE)

# Print combined data
print(head(combined_data, 10))

# Save the cleaned data as an Excel file
write.xlsx(combined_data, "cleaned_combined_data_24.xlsx", overwrite = TRUE)

# 2019-2023
# Define a list of file paths for each month in 2023
monthly_list_23 <- paste0(1:12, "-2023.xlsx")
sheet_names <- c("January", "February", "March", "April", "May", "June", 
                 "July", "August", "September", "October", "November", "December")
output_file <- "table_5_2023.xlsx"
wb <- createWorkbook()

# Read and clean the "Table 5" data from all files
all_tables <- lapply(monthly_list_23, function(file) {
  read_excel(file, sheet = "Table 5")
})

# Write data to different sheets
for (i in seq_along(all_tables)) {
  addWorksheet(wb, sheet_names[i])
  writeData(wb, sheet = sheet_names[i], all_tables[[i]])
}
saveWorkbook(wb, output_file, overwrite = TRUE)

# Extract rows 7-11 and clean data
all_data <- lapply(seq_along(sheet_names), function(i) {
  table_5 <- read.xlsx(output_file, sheet = sheet_names[i])
  filtered_rows <- table_5[7:11, -1]
  filtered_rows <- cbind(Month = sheet_names[i], filtered_rows)
  filtered_rows <- filtered_rows[, -c(ncol(filtered_rows)-1, ncol(filtered_rows))]
  filtered_rows
})

# Combine data
combined_data <- do.call(rbind, all_data)

# Rename columns
colnames(combined_data)[2:7] <- c("Travel purpose", "2019", "2020", "2021", "2022", "2023")

# Add a season column
combined_data <- combined_data %>%
  mutate(Season = case_when(
    Month %in% c("December", "January", "February") ~ "Summer",
    Month %in% c("March", "April", "May") ~ "Autumn",
    Month %in% c("June", "July", "August") ~ "Winter",
    Month %in% c("September", "October", "November") ~ "Spring"
  ))

# Write the cleaned data to a new sheet
addWorksheet(wb, "Filtered_Data")
writeData(wb, sheet = "Filtered_Data", combined_data)

# Print combined data
print(head(combined_data, 10))

# Save the seasonal data
addWorksheet(wb, "Seasonal")
writeData(wb, sheet = "Seasonal", combined_data)
saveWorkbook(wb, output_file, overwrite = TRUE)

# Save the cleaned data as an Excel file
write.xlsx(combined_data, "cleaned_combined_data_24.xlsx", overwrite = TRUE)
