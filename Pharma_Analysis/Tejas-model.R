require(data.table)
require(readxl)
library(zoo)
library(greybox)
library(smooth)
library(foreach)
library(ProbCast)
library(dplyr)

# Read the dataset
dataset_pharma <- read_excel("C:/Users/91976/Desktop/dataset_pharma.xlsx")
date_time <- data.table(date_time = as.POSIXct(dataset_pharma$date,tz='UTC', format="%Y-%m-%dT%H:%M:%SZ")) # nolint
# Convert the 'date' column to POSIXct and create a new 'targetTime' column
dataset_pharma$date <- date_time$date_tim

dataset_pharma_M01AB <- dataset_pharma[, c("M01AB",  "hour","date", "is_working_hours","time_of_day")] # nolint #TODO day of week
dataset_pharma_M01AB$M01AB <- zoo(dataset_pharma_M01AB$M01AB, order.by=dataset_pharma_M01AB$date) # nolint
# View(dataset_pharma_M01AB) # nolint
obs <- length(time(dataset_pharma_M01AB[[1]]))
test_set <- 365 * 24
#old fucntion #xreg
# oesModel <- oes(as.vector(coredata(dataset_pharma_M01AB$M01AB)), model="ANA", h=test_set, holdout=TRUE, occurrence="direct")
# Fit the ADAM model
# Add occurrence model
adam_model <- adam(dataset_pharma_M01AB, model="ANA", lags=c(1, 24, 24*7), h=test_set, holdout=TRUE, initial="b" ,regressors="use",distribution="default", maxeval=10000) # nolint
plot(adam_model,main="",which=7) # nolint
adam_model

adam_model_forecast <- forecast(adam_model, h = test_set, )
adam_model_forecast_values <- adam_model_forecast$mean

write.csv(adam_model_forecast_values, "adam_model_forecast.csv")
write.csv(dataset_pharma_M01AB, "dataset_pharma_M01AB.csv")

