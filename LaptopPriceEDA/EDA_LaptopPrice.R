install.packages("tidyverse")
# Load necessary libraries
library(tidyverse)

# 1. Load the dataset
df <- read.csv("C:/Users/aqilr/Downloads/LaptopPriceEDA/laptop_price1.csv")

# 2. Basic Inspection
str(df)
summary(df)
head(df)

# 3. Data Cleaning
# Remove 'GB' from Ram and 'kg' from Weight and convert to numeric
df$Ram <- as.numeric(gsub("GB", "", df$Ram))
df$Weight <- as.numeric(gsub("kg", "", df$Weight))

# 4. Check for missing values
colSums(is.na(df))

# 5. Visualizations

# Distribution of Laptop Prices
ggplot(df, aes(x = Price_euros)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of Laptop Prices", x = "Price (Euros)", y = "Frequency")

# Count of Laptops by Company
df %>%
  count(Company) %>%
  arrange(desc(n)) %>%
  ggplot(aes(x = reorder(Company, -n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Number of Laptops by Company", x = "Company", y = "Count")

# Price Distribution by Laptop Type
ggplot(df, aes(x = TypeName, y = Price_euros, fill = TypeName)) +
  geom_boxplot() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Price Distribution by Laptop Type", x = "Type", y = "Price (Euros)")

# Relationship between RAM and Price
ggplot(df, aes(x = Ram, y = Price_euros)) +
  geom_point(alpha = 0.5, color = "darkblue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  theme_minimal() +
  labs(title = "Relationship Between RAM and Price", x = "RAM (GB)", y = "Price (Euros)")

# 6. Correlation Analysis
# Correlation between numeric variables
cor_matrix <- cor(df %>% select(Inches, Ram, Weight, Price_euros))

print(cor_matrix)
