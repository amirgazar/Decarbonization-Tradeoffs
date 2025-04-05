# Load required library
library(ggplot2)

# Mean estimates for each class
mean_class_3 <- 1.24
mean_class_4 <- 1.40
mean_class_5 <- 1.79

# Standard deviations calculated from provided percentiles using the log-normal distribution
sd_class_3 <- (log(1.63) - log(0.99)) / (qnorm(0.9) - qnorm(0.1))
sd_class_4 <- (log(2.09) - log(1.06)) / (qnorm(0.9) - qnorm(0.1))
sd_class_5 <- (log(3.01) - log(1.27)) / (qnorm(0.9) - qnorm(0.1))

# Function to calculate the density for a given class
calculate_density <- function(x, mean, sd) {
  dlnorm(x, meanlog = log(mean), sdlog = sd)
}

# Define a range of Actual/Base Estimate values for the plot
x_values <- seq(0.6, 3.4, length.out = 100)

# Calculate densities for each class
density_class_3 <- calculate_density(x_values, mean_class_3, sd_class_3)
density_class_4 <- calculate_density(x_values, mean_class_4, sd_class_4)
density_class_5 <- calculate_density(x_values, mean_class_5, sd_class_5)

# Create a data frame for plotting
plot_data <- data.frame(
  ActualBaseEstimate = rep(x_values, 3),
  Density = c(density_class_3, density_class_4, density_class_5),
  Class = rep(c("Class 3", "Class 4", "Class 5"), each = length(x_values))
)

# Plot the density curves to match the initial plot
ggplot(plot_data, aes(x = ActualBaseEstimate, y = Density, color = Class)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Dataset Actual/Base Estimate Metrics Fitted to Lognormal Distributions",
    x = "Actual/Base Estimate",
    y = "Relative Frequency of Occurrence",
    color = "Class"
  ) +
  theme_minimal()


## Estimate the costs
# Define the base cost estimate for the dam
base_cost <- 7144530 # CAPEX_mean 2024-USD/MW from 12_Other_S3_CAN_Hydro.R

# Define a range of percentiles (e.g., P10, P50, P90)
percentiles <- c(0.1, 0.5, 0.9)  

# Function to calculate costs for a given class and percentile range
calculate_costs <- function(base_cost, mean, sd, percentiles) {
  costs <- base_cost * qlnorm(percentiles, meanlog = log(mean), sdlog = sd)
  return(data.frame(Percentile = percentiles * 100, Cost = costs))
}

# Calculate costs for each class
costs_class_3 <- calculate_costs(base_cost, mean_class_3, sd_class_3, percentiles)
costs_class_4 <- calculate_costs(base_cost, mean_class_4, sd_class_4, percentiles)
costs_class_5 <- calculate_costs(base_cost, mean_class_5, sd_class_5, percentiles)

# Print the results for each class
print("Costs for Class 3:")
print(costs_class_3)

print("Costs for Class 4:")
print(costs_class_4)

print("Costs for Class 5:")
print(costs_class_5)

# Combine data for plotting costs
cost_data <- rbind(
  data.frame(costs_class_3, Class = "Class 3"),
  data.frame(costs_class_4, Class = "Class 4"),
  data.frame(costs_class_5, Class = "Class 5")
)

# Plot the costs across percentiles
ggplot(cost_data, aes(x = Percentile, y = Cost, color = Class)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Cost Estimates at Different Percentiles for Given Base Estimate",
    x = "Percentile",
    y = "Cost ($)",
    color = "Class"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::dollar)

