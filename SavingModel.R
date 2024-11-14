# Saving the model
saveRDS(rf_model, "./models/saved_rf_model.rds")

# Load the saved model
loaded_rf_model <- readRDS("./models/saved_rf_model.rds")

# Model predicts Grade (milk quality)
new_data <- data.frame(
  pH = 6.6,
  Temprature = 35,
  Taste = factor(1, levels = c(0, 1)),  # Adjust levels 
  Odor = factor(0, levels = c(0, 1)),    # Adjust levels 
  Fat = factor(1, levels = c(0, 1)),     # Adjust levels 
  Turbidity = factor(0, levels = c(0, 1)),# Adjust levels 
  Colour = 254
)

# Use the loaded model to make predictions
predictions_loaded_model <- predict(loaded_rf_model, newdata = new_data)

# Print predictions
print(predictions_loaded_model)
