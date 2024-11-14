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
