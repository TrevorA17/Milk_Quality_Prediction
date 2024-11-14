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

set.seed(123)  # For reproducibility
sample_index <- sample(seq_len(nrow(MilkData)), size = 0.7 * nrow(MilkData))
train_data <- MilkData[sample_index, ]
test_data <- MilkData[-sample_index, ]

# Check the size of the splits
nrow(train_data)
nrow(test_data)
