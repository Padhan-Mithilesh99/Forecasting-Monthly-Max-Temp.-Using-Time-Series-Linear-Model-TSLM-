# ---------------------------------------------
# STEP 0: Load Required Libraries
# ---------------------------------------------
library(forecast)
library(ggplot2)
library(dplyr)
library(lubridate)

# ---------------------------------------------
# STEP 1: Read and Clean the Data
# ---------------------------------------------
data <- read.csv("C:/Users/MITHILESH/Desktop/AEGD_PROJECT/portblair.csv")
data$DATE <- as.Date(data$DATE)
data <- na.omit(data[, c("DATE", "TMAX")])

# ---------------------------------------------
# STEP 2: Aggregate Daily Data to Monthly Average
# ---------------------------------------------
data$year_month <- format(data$DATE, "%Y-%m")

monthly_avg <- data %>%
  group_by(year_month) %>%
  summarise(TMAX = mean(TMAX, na.rm = TRUE)) %>%
  ungroup()

# Extract year, month and time index
monthly_avg$Year <- as.numeric(substr(monthly_avg$year_month, 1, 4))
monthly_avg$Month <- as.numeric(substr(monthly_avg$year_month, 6, 7))
monthly_avg$TimeIndex <- 1:nrow(monthly_avg)

# ---------------------------------------------
# STEP 3: Split into Training (up to 2023) and Testing (2024)
# ---------------------------------------------
train_data <- monthly_avg %>% filter(Year <= 2023)
test_data <- monthly_avg %>% filter(Year == 2024)

# Show first and last 5 rows of train_data
rbind(head(train_data, 5), tail(train_data, 5))
# Show first and last 5 rows of test_data
rbind(head(test_data, 5), tail(test_data, 5))



# Add one-hot encoded month variables
train_data <- cbind(train_data, model.matrix(~ as.factor(Month) - 1, train_data))
test_data <- cbind(test_data, model.matrix(~ as.factor(Month) - 1, test_data))

# ---------------------------------------------
# STEP 4: Fit Linear Regression Model (Trend + Seasonality)
# ---------------------------------------------
model_lm <- lm(TMAX ~ TimeIndex + ., data = train_data[, c("TMAX", "TimeIndex", paste0("as.factor(Month)", 1:11))])
summary(model_lm)

# ---------------------------------------------
# STEP 5: Forecast 2024 Using the Model
# ---------------------------------------------
test_data$Predicted_TMAX <- predict(model_lm, newdata = test_data)

# ---------------------------------------------
# STEP 6: Accuracy Assessment
# ---------------------------------------------
rmse <- sqrt(mean((test_data$TMAX - test_data$Predicted_TMAX)^2, na.rm = TRUE))
mae <- mean(abs(test_data$TMAX - test_data$Predicted_TMAX), na.rm = TRUE)

cat("RMSE for 2024:", round(rmse, 2), "\n")
cat("MAE for 2024:", round(mae, 2), "\n")

# ---------------------------------------------
# STEP 7: Plot Full Trend + Forecast + Observed
# ---------------------------------------------
# Merge full data and predict on all points
full_data <- bind_rows(
  train_data %>% mutate(Set = "Train"),
  test_data %>% mutate(Set = "Test")
)

full_data$Date <- as.Date(paste0(full_data$year_month, "-01"))
full_data$Fitted_TMAX <- predict(model_lm, newdata = full_data)

# Make sure Date column exists in test_data
test_data$Date <- as.Date(paste0(test_data$year_month, "-01"))

# Final Plot with Year-Only X Axis
ggplot(full_data, aes(x = Date)) +
  geom_line(aes(y = Fitted_TMAX, color = "Fitted Trend"), size = 1.2) +
  geom_line(data = test_data, aes(x = Date, y = Predicted_TMAX, color = "Forecast (2024)"), 
            size = 1.2, linetype = "dashed") +
  geom_point(data = test_data, aes(x = Date, y = TMAX, color = "Observed (2024)"), size = 2) +
  scale_color_manual(name = "Legend", values = c(
    "Fitted Trend" = "steelblue",
    "Forecast (2024)" = "red",
    "Observed (2024)" = "black"
  )) +
  labs(
    title = "Monthly Tmax Trend & Forecast (Port Blair)",
    x = "Year", y = "TMAX (°F)"
  ) +
  theme_minimal() +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1.0))



# STEP 7: Plot Observed vs Predicted for 2024
# ---------------------------------------------
# Create Date column
test_data$Date <- as.Date(paste0(test_data$year_month, "-01"))

# Create the base plot
ggplot() +
  geom_line(data = test_data, aes(x = Date, y = TMAX, color = "Observed"), size = 1.2) +
  geom_line(data = test_data, aes(x = Date, y = Predicted_TMAX, color = "Predicted"), size = 1.2, linetype = "dashed") +
  scale_color_manual(name = "Legend", values = c("Observed" = "blue", "Predicted" = "red")) +
  labs(title = "Validation: Forecasted vs Observed Monthly Tmax for 2024",
       x = "Month", y = "TMAX (°F)") +
  theme_minimal() +
  scale_x_date(date_labels = "%b-%Y", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
