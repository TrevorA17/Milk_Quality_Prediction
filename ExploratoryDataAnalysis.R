# Load the dataset
MilkData <- read.csv("data/milknew.csv", colClasses = c(
  pH = "numeric",
  Temprature = "numeric",
  Taste = "factor",
  Odor = "factor",
  Fat = "factor",
  Turbidity = "factor",
  Colour = "numeric",
  Grade = "factor"
))

# Display structure to verify data types
str(MilkData)

# Display first few rows to ensure data is loaded correctly
head(MilkData)

View(MilkData)

# Frequency tables for categorical variables
table(MilkData$Taste)
table(MilkData$Odor)
table(MilkData$Fat)
table(MilkData$Turbidity)
table(MilkData$Grade)

# Mean
mean_pH <- mean(MilkData$pH, na.rm = TRUE)
mean_Temprature <- mean(MilkData$Temprature, na.rm = TRUE)
mean_Colour <- mean(MilkData$Colour, na.rm = TRUE)

# Median
median_pH <- median(MilkData$pH, na.rm = TRUE)
median_Temprature <- median(MilkData$Temprature, na.rm = TRUE)
median_Colour <- median(MilkData$Colour, na.rm = TRUE)

# Mode function (for simplicity, it assumes one mode)
get_mode <- function(v) {
  uniq_vals <- unique(v)
  uniq_vals[which.max(tabulate(match(v, uniq_vals)))]
}

mode_pH <- get_mode(MilkData$pH)
mode_Temprature <- get_mode(MilkData$Temprature)
mode_Colour <- get_mode(MilkData$Colour)

# Display results
mean_pH; mean_Temprature; mean_Colour
median_pH; median_Temprature; median_Colour
mode_pH; mode_Temprature; mode_Colour

# Variance
var_pH <- var(MilkData$pH, na.rm = TRUE)
var_Temprature <- var(MilkData$Temprature, na.rm = TRUE)
var_Colour <- var(MilkData$Colour, na.rm = TRUE)

# Standard Deviation
sd_pH <- sd(MilkData$pH, na.rm = TRUE)
sd_Temprature <- sd(MilkData$Temprature, na.rm = TRUE)
sd_Colour <- sd(MilkData$Colour, na.rm = TRUE)

# Range
range_pH <- range(MilkData$pH, na.rm = TRUE)
range_Temprature <- range(MilkData$Temprature, na.rm = TRUE)
range_Colour <- range(MilkData$Colour, na.rm = TRUE)

# Quantiles
quantiles_pH <- quantile(MilkData$pH, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
quantiles_Temprature <- quantile(MilkData$Temprature, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
quantiles_Colour <- quantile(MilkData$Colour, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)

# Display results
var_pH; var_Temprature; var_Colour
sd_pH; sd_Temprature; sd_Colour
range_pH; range_Temprature; range_Colour
quantiles_pH; quantiles_Temprature; quantiles_Colour

# Correlation between numeric variables
cor_matrix <- cor(MilkData[, c("pH", "Temprature", "Colour")], use = "complete.obs")

# Contingency tables for categorical variables
table_taste_grade <- table(MilkData$Taste, MilkData$Grade)
table_odor_grade <- table(MilkData$Odor, MilkData$Grade)

# Display results
cor_matrix
table_taste_grade
table_odor_grade

