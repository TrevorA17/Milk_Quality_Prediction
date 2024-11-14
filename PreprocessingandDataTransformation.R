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

# Check for missing values in each column
colSums(is.na(MilkData))

# Alternatively, use the 'summary' function to get an overview, which also indicates missing values
summary(MilkData)

# Display rows with any missing values (if any exist)
MilkData[!complete.cases(MilkData), ]
