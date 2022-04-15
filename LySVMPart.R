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
data_fin$AcresBurned <- as.numeric(data_fin$AcresBurned)\
data_fin$season <- as.factor(data_fin$season)
data_fin$month <- as.factor(data_fin$month)

data_fin$AcresBurned[data_fin$AcresBurned == 0] <- NA
data_fin$AcresBurned[is.na(data_fin$AcresBurned)] <- mean(data$AcresBurned, na.rm = TRUE)


#################################################################################################
#Graphing to see if there are any known trends
ggplot(data_fin, aes(x = season, 
                     y = AcresBurned, 
                     fill = season)) +
                geom_histogram(stat = 'identity') +
                geom_jitter()

#################################################################################################
#Models for ML
#Train and Test set
set.seed(123)
indices <- sample(nrow(data_fin), .70 * nrow(data_fin))
train <- data_fin[indices, ]
train <- train %>%
         select(AcresBurned, CalFireIncident, season, MajorIncident)

test <- data_fin[-indices,]
test <- test %>%
        select(AcresBurned, CalFireIncident, season, MajorIncident)

##################Creating svm Model###########################################
#I want to see if my model could predict the season using the variables
svm_model <- svm(season ~., data = train)
svm_model

#Predicting dataset to compare with test dataset
svm_pred_season <- predict(svm_model, test)
svm_pred <- test
svm_pred$predseason <- svm_pred_month

#Checking accuracy
sum(test$season == svm_pred$predseason) / nrow(svm_pred) * 100
#~98% accuracy, therefore if the model could predict the season with these variables, this shows that there is a correlation between the size of the fire vs the seasons. 

###############################################################################
#lm() model 
lm_model <- lm(season ~., data = train)
lm_model

lm_model_season <- predict(lm_model, test)
lm_pred <- test
lm_pred$predseason <- lm_model_season

#Realized here that a linear model would not work because there is no linear correlation between the data, only clustering.

