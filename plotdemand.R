plot_demand <- function(model, data) {
  # Predict demand using the model
  predicted_values <- predict(model)
  
  # Check if the length of the data and predicted values differ
  if (length(data$demand) != length(predicted_values)) {
    # If they differ, add NAs to the tail of the predicted values to match the length of the data
    num_nas <- length(data$demand) - length(predicted_values)
    predicted_values <- c(rep(NA, num_nas), predicted_values)
  }
  
  # Calculate the residuals
  residuals <- data$demand - predicted_values
  
  # Create a new data frame with the actual, predicted values, residuals, and time variable
  df <- data.frame(actual = data$demand, 
                   predicted = predicted_values, 
                   residuals = residuals, 
                   time = seq_along(data$demand))
  
  # Plot the actual and predicted values over time and the residuals as bars at the bottom of the plot
  ggplot(df, aes(x = time)) +
    geom_line(aes(y = actual, color = "Actual")) +
    geom_line(aes(y = predicted, color = "Predicted")) +
    geom_bar(aes(y = residuals), stat = "identity", fill = "gray", alpha = 0.5) +
    labs(title = "Actual vs. Predicted Demand with Residuals",
         x = "Time", y = "Demand") +
    scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red"))
}
