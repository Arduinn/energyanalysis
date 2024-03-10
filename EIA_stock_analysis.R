rm(list=ls())
setwd("/Users/victorarduinwecki/Documents/03. Projects/02. Data Analysis/03. EIA Stocks")

# Packages ----------------------------------------------------------------
packages = c("ggplot2","readxl","dplyr","tidyr","lubridate",
             "reshape2","data.table","xts","forecast","zoo",
             "scales") 
lapply(packages, require, character.only = TRUE)

# Data --------------------------------------------------------------------

# Importing Data
gl_stock_data = read_excel("./data/PET_STOC_WSTK_A_EPM0_SAE_MBBL_W.xls", sheet = "Data 1", skip = 2)

# Selecting Columns
gl_stock_data = gl_stock_data %>% select(`Date`,`Weekly U.S. Ending Stocks of Total Gasoline  (Thousand Barrels)`)

# Getting Differences
gl_stock_data = gl_stock_data %>% mutate(stk_change = `Weekly U.S. Ending Stocks of Total Gasoline  (Thousand Barrels)` - lag(`Weekly U.S. Ending Stocks of Total Gasoline  (Thousand Barrels)`))

# Filter Data
# gl_stock_data = gl_stock_data %>% filter(`Date` >= '2021-01-01')
  
# Preparign Data
gl_stock_data = gl_stock_data %>% select("Date", "stk_change")
gl_stock_data$week = week(gl_stock_data$Date)
gl_stock_data$year = year(gl_stock_data$Date)

# Filter Data
gl_stock_data = gl_stock_data %>% filter(gl_stock_data$week < 10)


# Descriptive Analysis ----------------------------------------------------
# Create a boxplot using ggplot2
ggplot(gl_stock_data, aes(x = as.factor(week), y = stk_change)) +
  geom_boxplot(fill = "#69b3a2", color = "#2c3e50", alpha = 0.8) +  # Adjusting boxplot aesthetics
  geom_point(data = filter(gl_stock_data, year == 2024), aes(color = "2024"), size = 2, shape = 18) +  # Adjusted size of points
  labs(x = "Week (Jan-Mar 2024)", y = "Stock Change", title = "Gasoline Boxplot of Stock Changes by Week") +  # Adjusted axis labels
  theme_minimal() +  
  scale_y_continuous(limits = c(-7500, 10000), labels = label_number_si()) + # Format y-axis labels with thousand separators
  scale_color_manual(values = c("2024" = "red"), labels = c("2024")) +
  guides(color = guide_legend(title = "Year"))  # Adding legend with appropriate title