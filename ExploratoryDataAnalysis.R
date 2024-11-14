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

# ANOVA for pH across different Grade levels
anova_pH <- aov(pH ~ Grade, data = MilkData)
summary(anova_pH)

# ANOVA for Temprature across different Grade levels
anova_Temprature <- aov(Temprature ~ Grade, data = MilkData)
summary(anova_Temprature)

# ANOVA for Colour across different Grade levels
anova_Colour <- aov(Colour ~ Grade, data = MilkData)
summary(anova_Colour)

# Post-hoc test for pH if ANOVA is significant
TukeyHSD(anova_pH)

# Post-hoc test for Temprature if ANOVA is significant
TukeyHSD(anova_Temprature)

# Post-hoc test for Colour if ANOVA is significant
TukeyHSD(anova_Colour)

library(ggplot2)

# Histogram for pH
ggplot(MilkData, aes(x = pH)) +
  geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black") +
  labs(title = "Distribution of pH Levels", x = "pH", y = "Frequency")

# Boxplot for Temperature
ggplot(MilkData, aes(y = Temprature)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Boxplot of Temperature", y = "Temperature")

# Bar plot for categorical variables (e.g., Taste)
ggplot(MilkData, aes(x = Taste)) +
  geom_bar(fill = "coral") +
  labs(title = "Frequency of Taste", x = "Taste", y = "Count")

# Bar plot for Grade
ggplot(MilkData, aes(x = Grade)) +
  geom_bar(fill = "lightblue") +
  labs(title = "Frequency of Milk Quality Grades", x = "Grade", y = "Count")

# Scatter plot of pH vs. Colour, colored by Grade
ggplot(MilkData, aes(x = pH, y = Colour, color = Grade)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "pH vs. Colour by Grade", x = "pH", y = "Colour") +
  theme_minimal()

# Box plot of Temperature by Grade
ggplot(MilkData, aes(x = Grade, y = Temprature, fill = Grade)) +
  geom_boxplot() +
  labs(title = "Temperature by Milk Quality Grade", x = "Grade", y = "Temperature")

# Scatter plot matrix for numeric variables (pH, Temperature, Colour)
pairs(MilkData[, c("pH", "Temprature", "Colour")], 
      main = "Scatter Plot Matrix of Numeric Variables", 
      col = as.numeric(MilkData$Grade))

# Faceted histogram of pH by Grade
ggplot(MilkData, aes(x = pH, fill = Grade)) +
  geom_histogram(binwidth = 0.5, color = "black") +
  facet_wrap(~ Grade) +
  labs(title = "Distribution of pH by Milk Quality Grade", x = "pH", y = "Frequency")


