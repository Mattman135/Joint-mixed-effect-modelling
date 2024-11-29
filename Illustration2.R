# Illustration motivating example LMM

# Clear working environment.
rm(list = ls())

# Define the intercepts, slopes, and x-axis ranges
intercepts <- c(100, 95, 90, 85)
slopes <- c(-1, -0.5, 0.1, 0.5)
x_ranges <- list(c(5, 6), c(5, 7), c(6.5, 8), c(7, 9))

# Number of points per line
n_points <- 10

# Storage for the simulated data
set.seed(42)  # Set seed for reproducibility
simulated_data <- data.frame(ID = integer(), X = numeric(), Y = numeric())

# Loop through each line
for (i in seq_along(intercepts)) {
  # Generate random x-values within the specific range for each line
  x_values <- runif(n_points, x_ranges[[i]][1], x_ranges[[i]][2])
  
  # Calculate y-values based on the line equation
  y_values <- slopes[i] * x_values + intercepts[i]
  
  # Add random noise to y-values
  noise <- rnorm(n_points, mean = 0, sd = 1)  # Adjust sd for variability
  y_values_noisy <- y_values + noise
  
  # Store the results in the data frame
  simulated_data <- rbind(simulated_data, 
                          data.frame(ID = i, X = x_values, Y = y_values_noisy))
}

# View the first few rows
head(simulated_data)

# Plot the simulated points
library(ggplot2)
ggplot(simulated_data, aes(x = X, y = Y, color = factor(ID))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  geom_smooth(data = simulated_data %>% filter(ID == 1), method = "lm", se = FALSE) +
  geom_smooth(data = simulated_data %>% filter(ID == 2), method = "lm", se = FALSE) +
  geom_smooth(data = simulated_data %>% filter(ID == 3), method = "lm", se = FALSE) +
  geom_smooth(data = simulated_data %>% filter(ID == 4), method = "lm", se = FALSE) +
  labs(color = "ID", title = "") +
  theme_minimal()
