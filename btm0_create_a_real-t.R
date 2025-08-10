# Load necessary libraries
library(caret)
library(e1071)
library(plumber)

# Define a function to train a machine learning model
train_model <- function(data) {
  # Split data into training and testing sets
  set.seed(123)
  trainIndex <- createDataPartition(data$target, p = 0.7, list = FALSE)
  trainSet <- data[ trainIndex,]
  testSet <- data[-trainIndex,]
  
  # Train a decision tree model
  model <- rpart(target ~ ., data = trainSet)
  
  # Return the trained model
  return(model)
}

# Define a function to make predictions using the trained model
make_predictions <- function(model, data) {
  # Make predictions on the test set
  predictions <- predict(model, data, type = "prob")
  
  # Return the predictions
  return(predictions)
}

# Define a function to send notifications based on the predictions
send_notifications <- function(predictions) {
  # Check if the predictions indicate a need for notification
  if (predictions > 0.5) {
    # Send a notification using a fictional notification service
    notify("btm0_notification_service", "Model prediction exceeds threshold!")
  }
}

# Create a plumber API endpoint to receive data and trigger the ML pipeline
api <- plumb("api")
api$GET("/predict/:data", function(data) {
  # Train the model using the received data
  model <- train_model(data)
  
  # Make predictions using the trained model
  predictions <- make_predictions(model, data)
  
  # Send notifications based on the predictions
  send_notifications(predictions)
  
  # Return a success message
  list(result = "Model trained and notifications sent!")
})

# Run the plumber API
api$run()