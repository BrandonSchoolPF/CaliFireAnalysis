library(dplyr)
library(e1071)
library(kernlab)
library(ggplot2)

#Cleaning the data to only include the columns I need
data <- read.csv("California_Fire_incidents.csv")
data2 <- read.csv("California_Fire_incidents.csv")
data <- data %>%
        select(AcresBurned, ArchiveYear, CalFireIncident, Started, MajorIncident)
data$CalFireIncident <- as.factor(data$CalFireIncident)


#Removing the the year and day from the date so that I can only use month
data$month <- strftime(data$Started,"%m")
data$day <- strftime(data$Started, "%j")
data$month <- as.numeric(data$month)
data$day <- as.numeric(data$day)

#season column
data$seasons <- as.numeric(data$month)

#Creating a separate df for seasons
spring <- c(03, 04, 05)
fall <- c(09, 10 , 11)
summer <- c(06, 07, 08)
winter <- c(12, 01, 02)

seasons <- data.frame(
  seasons = c("spring","spring","spring", "fall","fall","fall", "summer","summer","summer", "winter","winter","winter"),
  month = c(spring, fall, summer, winter)
)

data <- data %>%
        full_join(seasons, data, by = "month")

#Creating final dataset
data_fin <- data %>%
            select(AcresBurned, ArchiveYear, CalFireIncident, "season" = seasons.y, month, day, Started, MajorIncident)
data_fin$Started <- as.Date(data_fin$Started)
data_fin$Day <- strftime(data_fin$Started,"%m-%d")
data_fin$MajorIncident <- as.factor(data_fin$MajorIncident)
data_fin$AcresBurned <- as.numeric(data_fin$AcresBurned)

data_fin$AcresBurned[data_fin$AcresBurned == 0] <- NA
data_fin$AcresBurned[is.na(data_fin$AcresBurned)] <- mean(data$AcresBurned, na.rm = TRUE)


#################################################################################################
#Graphing to see if there are any known trends
ggplot(data_fin, aes(x = month, 
                     y = AcresBurned, 
                     color = season)) +
                geom_point() +
                geom_jitter() + 
                ylim(0, 2000)

#################################################################################################
#Models for ML
#Train and Test set
set.seed(123)
indices <- sample(nrow(data_fin), .70 * nrow(data_fin))
train <- data_fin[indices, ]
train <- train %>%
         select(AcresBurned, CalFireIncident, season, MajorIncident, month)

test <- data_fin[-indices,]
test <- test %>%
        select(AcresBurned, CalFireIncident, season, MajorIncident, month)

##################Creating svm Model###########################################
svm_model <- svm(month ~., data = train)
svm_model

#Predicting dataset to compare with test dataset
svm_pred_month <- predict(svm_model, test)
svm_pred <- test
svm_pred$predmonth <- svm_pred_month

#rounding the months
svm_pred$predmonth <- round(svm_pred$predmonth)

#Checking accuracy
sum(test$month == svm_pred$predmonth) / nrow(svm_pred) * 100
#93.27902% wowowoowwoowowo

###############################################################################
#lm() model 
lm_model <- lm(month ~., data = train)
lm_model

#Predicting and adding to new df
lm_pred_month <- predict(lm_model, test)
lm_pred <- test
lm_pred$predmonth <- lm_pred_month

#rounding the months
lm_pred$predmonth <- round(lm_pred$predmonth)

#Checking accuracy
sum(lm_pred$month == lm_pred$predmonth) / nrow(lm_pred) * 100
#96.53768 :(

