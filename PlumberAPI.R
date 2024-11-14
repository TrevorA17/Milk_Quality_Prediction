# Plumber API
# Load the saved RandomForest model
loaded_rf_model <- readRDS("./models/saved_rf_model.rds")

#* @apiTitle Milk Quality Prediction Model API

#* @apiDescription Used to predict the quality (Grade) of milk based on various features.

#* @param pH pH level of milk
#* @param Temprature Temperature of milk
#* @param Taste Taste factor (0 or 1)
#* @param Odor Odor factor (0 or 1)
#* @param Fat Fat factor (0 or 1)
#* @param Turbidity Turbidity factor (0 or 1)
#* @param Colour Colour of the milk

#* @post /predict
predict_milk_quality <- function(pH, Temprature, Taste, Odor, Fat, Turbidity, Colour) {
  
  # Create a data frame using the arguments
  to_be_predicted <- data.frame(
    pH = as.numeric(pH),
    Temprature = as.numeric(Temprature),
    Taste = factor(as.numeric(Taste), levels = c(0, 1)),
    Odor = factor(as.numeric(Odor), levels = c(0, 1)),
    Fat = factor(as.numeric(Fat), levels = c(0, 1)),
    Turbidity = factor(as.numeric(Turbidity), levels = c(0, 1)),
    Colour = as.numeric(Colour)
  )
  
  # Use the loaded model to make predictions
  prediction <- predict(loaded_rf_model, newdata = to_be_predicted)
  
  # Return the prediction as a response
  return(list(prediction = prediction))
}

