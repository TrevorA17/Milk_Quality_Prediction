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

library(boot)

# Define a simple function to calculate the mean of pH as an example statistic
boot_mean <- function(data, indices) {
  sample_data <- data[indices, ]  # Resample data with replacement
  return(mean(sample_data$pH))
}

library(boot)
# Perform bootstrapping with 1000 resamples
set.seed(123)
boot_results <- boot(data = MilkData, statistic = boot_mean, R = 1000)

# View bootstrapped results
print(boot_results)
plot(boot_results)

library(caret)

# Train a Logistic Regression model
set.seed(123)
log_reg_model <- train(
  Grade ~ ., 
  data = MilkData, 
  method = "multinom", 
  family = "binomial", 
  trControl = trainControl(method = "cv", number = 10)
)

# Print summary of model
print(log_reg_model)

library(randomForest)
set.seed(123)
rf_model <- train(
  Grade ~ ., 
  data = MilkData, 
  method = "rf", 
  trControl = trainControl(method = "cv", number = 10)
)
print(rf_model)

set.seed(123)
svm_model <- train(
  Grade ~ ., 
  data = MilkData, 
  method = "svmRadial", 
  trControl = trainControl(method = "cv", number = 10)
)
print(svm_model)

# Combine models into a list
models <- resamples(list(Logistic_Regression = log_reg_model,
                         Random_Forest = rf_model,
                         SVM = svm_model))

# Summary of the resamples to view performance metrics
summary(models)

# Boxplots to visualize the comparison of models
bwplot(models)

# Additional metrics comparison (Accuracy, Kappa, etc.)
dotplot(models)

