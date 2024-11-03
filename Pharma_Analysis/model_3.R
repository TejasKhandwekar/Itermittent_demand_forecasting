
# remotes::install_github("config-i1/greybox", upgrade="never", dependencies=FALSE) # nolint
# remotes::install_github("config-i1/smooth", upgrade="never", dependencies=TRUE) # nolint

require(data.table)
require(readxl)
library(zoo)
library(greybox)
library(smooth)
library(foreach)
library(ProbCast)
library(dplyr)
packageVersion("smooth") #update smooth 
# remove.packages("greybox")
test_set <- 365 * 24

dataset_pharma <- read_excel("C:/Users/91976/Desktop/dataset_pharma.xlsx")
date_time <- data.table(date_time = as.POSIXct(dataset_pharma$date,tz='UTC', format="%Y-%m-%dT%H:%M:%SZ")) # nolint
dataset_pharma$date <- date_time$date_time
dataset_pharma_xreg <- dataset_pharma[, c("hour","date", "is_working_hours","time_of_day")] # nolint #TODO day of week

xreg <- dataset_pharma_xreg$is_working_hours
xreg <- xregExpander(xreg, lags=c(-2:1)*24, gaps="NAs")     # nolint #TODO remove 
xreg <- as.data.table(xreg)
xreg$x[is.na(xreg$x)] <- "none"
xreg$xLag24[is.na(xreg$xLag24)] <- "none"
xreg$xLag48[is.na(xreg$xLag48)] <- "none"
xreg$xLead24[is.na(xreg$xLead24)] <- "none"
xreg$x <- factor(xreg$x)
xreg$xLag24 <- factor(xreg$xLag24)
xreg$xLag48 <- factor(xreg$xLag48)
xreg$xLead24 <- factor(xreg$xLead24)


columns <- c("M01AB","M01AE","N02BA","N02BE","N05B","N05C","R03","R06") # nolint
n_rows <- length(columns)  # nolint
adamModeliETS_forecast_values <- data.table(matrix(nrow = test_set, ncol = 1))  # nolint

col <- "R06"

y <- zoo(dataset_pharma[[col]], order.by=dataset_pharma_xreg$date) # nolint
xregData <- data.table(y=y,xreg)      # nolint
xregExpanded <- data.table(y=y,xreg)  # nolint
xregExpanded <- cbind(xregExpanded,   # nolint
                    hourOfDay=as.factor(temporaldummy(y,type="hour",of="day",factors=TRUE)),   # nolint
                    dayOfWeek=as.factor(temporaldummy(y,type="day",of="week",factors=TRUE)),   # nolint
                    weekOfYear=as.factor(temporaldummy(y,type="week",of="year",factors=TRUE))) # nolint

View(xregExpanded)  # nolint


xregDummies <- cbind(xregExpanded[,1],                                            # nolint
                    model.matrix(~x+xLag48+xLag24+xLead24,xregExpanded)[,-1],    # nolint
                    xregExpanded[,6:8])                                          # nolint

colnames(xregDummies) <- make.names(colnames(xregDummies), unique=TRUE)           # nolint
xregDummiesShort <- xregDummies                                                   # nolint
xregDummiesShort <- model.matrix(~.,xregExpanded)[,-1]  # nolint
colnames(xregDummiesShort) <- make.names(colnames(xregDummiesShort), unique=TRUE)# nolint

# nolint

oesModel <- oes(as.vector(xregExpanded[[1]]), "MNN", h=test_set, holdout=TRUE, occurrence="direct" # nolint
,xreg = dataset_pharma_xreg$is_working_hours, regressors = 'use')# nolint

xregExpanded #ts object adn has frequncy  #MNM # nolint

plot(oesModel,main="",which=7) # nolint

adamModeliETS <- adam(xregExpanded, "MNN", lags=c(1,24,24*7), h=test_set, holdout=TRUE, initial="b",      # nolint
                    occurrence=oesModel, regressors="use", maxeval=1000)# nolint #try MNN   or exclued dummies from , try initial o

plot(adamModeliETS,main="",which=7) # nolint
adamModeliETS

adamModeliETS_forecast <- forecast(adamModeliETS, h = test_set, )# nolint
adamModeliETS_forecast_values[[col]] <- adamModeliETS_forecast$mean # nolint

View(adamModeliETS_forecast_values) # nolint


write.csv(adamModeliETS_forecast_values, "adamModeliETS_forecast_4_R06.csv")# nolint
