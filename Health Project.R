### Health Project ###

# Read file and work with data. The following health data was found online at:
# https://data.cms.gov/Medicare/Inpatient-Prospective-Payment-System-IPPS-Provider/97k6-zzx3
data <- read.csv("Inpatient_Prospective_Payment_System__IPPS__Provider_Summary_for_the_Top_100_Diagnosis-Related_Groups__DRG__-_FY2011.csv")

# Import correct packages.
library(dplyr); library(ggplot2); library(caret)

# First convert all of the payment columns to numeric values.
data <- (data %>% mutate(Average.Covered.Charges = as.numeric(Average.Covered.Charges))
         %>% mutate(Average.Total.Payments = as.numeric(Average.Total.Payments))
         %>% mutate(Average.Medicare.Payments = as.numeric(Average.Medicare.Payments)))

# Split data into training and testing sets.
set.seed(156)
inTrain <- createDataPartition(y=data$Average.Total.Payments,
                               p=0.7, list=FALSE)
training <- data[inTrain,]
testing <- data[-inTrain,]


# Motivating Question 1: What are the most frequent procedures?

# Group data by procedure and then find total number of discharges.
important1 <- c("DRG.Definition", "Total.Discharges")
training1 <- training[important1]
training1 <- (training1 %>% group_by(DRG.Definition)
              %>% summarise_each(funs(sum)))

# Build histogram of the total number of discharges.
par(mfcol=c(1,1))
hist(training1$Total.Discharges, xlab="Total Discharges", main="Histogram of Total Discharges")

# Look at the procedures by descending popularity.
training1 <- (training1 %>% arrange(desc(Total.Discharges)))
head(training1, 15)

# The most frequent procedures are the ones in the tail of the histogram above.
# Based on the information above, it seems reasonable to consider procedures with
# 'Total.Discharges' values greater than 100000.
freq_procedures <- filter(training1, Total.Discharges>100000)


# Motivating Question 2: Do different states have drastically different prices
# for the various procedures?

# Group data by procedure and then by state.
important2 <- c("DRG.Definition", "Provider.State",
               "Average.Covered.Charges", "Average.Total.Payments",
               "Average.Medicare.Payments")
training2 <- training[important2]
training2 <- (training2 %>% group_by(DRG.Definition, Provider.State)
              %>% summarise_each(funs(mean)))

# Build plots of all the relevant data.
par(mfcol=c(3,1))
plot(training2$DRG.Definition, training2$Average.Covered.Charges, xlab="Procedures",
     main="State Mean of Average Covered Charges by Procedure")
plot(training2$DRG.Definition, training2$Average.Total.Payments, xlab="Procedures",
     main="State Mean of Average Total Payments by Procedure")
plot(training2$DRG.Definition, training2$Average.Medicare.Payments, xlab="Procedures",
     main="State Mean of Average Medicare Payments by Procedure")

# Filter on only the most frequent procedures and build plots as before.
new_training2 <- filter(training2, DRG.Definition %in% unique(freq_procedures$DRG.Definition))
par(mfcol=c(3,1))
plot(new_training2$DRG.Definition, new_training2$Average.Covered.Charges,
     xlab="Frequent Procedures", main="State Mean of Average Covered Charges by Procedure")
plot(new_training2$DRG.Definition, new_training2$Average.Total.Payments,
     xlab="Frequent Procedures", main="State Mean of Average Total Payments by Procedure")
plot(new_training2$DRG.Definition, new_training2$Average.Medicare.Payments,
     xlab="Frequent Procedures", main="State Mean of Average Medicare Payments by Procedure")

# It is difficult to have a straightforward interpretation of the plots above.
# However, it does seem like many procedures have noticeable outliers. In other words,
# for many procedures, there is at least one state where the mean of the average costs
# is signficiantly different from that of other states.


# Motivating Question 3: Is it possible to predict what the 'Average.Total.Payments'
# will be given the 'DRG.Definition', 'Provider.State', and 'Total.Discharges'?

# Split training set into further train and test sets.
set.seed(377)
inTrain2 <- createDataPartition(y=training$Average.Total.Payments,
                               p=0.7, list=FALSE)
train <- training[inTrain2,]
test <- training[-inTrain2,]

# Try basic general linear model. (Takes some time to compile.)
set.seed(442)
glm_model <- train(Average.Total.Payments ~ DRG.Definition + Provider.State + Total.Discharges,
                   data=train, preProcess=c("center","scale"), method="glm")
glm_predictions <- predict(glm_model, newdata=test)
summary(glm_model$finalModel)

# Notice that the 'Provider.State' information is not often significant. 'DRG.Definition'
# values and 'Total.Discharges' were much more useful predictors.

# Look at how the predicted values compare to the real values of 'Average.Total.Payments'
# on the test set.

new_data <- data.frame(glm_predictions, test$Average.Total.Payments, test$Provider.State)
par(mfrow=c(1,1))
g <- ggplot(new_data, aes(glm_predictions, test.Average.Total.Payments))
g + geom_point(color="blue", aes(alpha=0.8)) +
  geom_abline(intercept=0, slope=1) +
  labs(x = "Predictions", y = "Actual Values", title = "Average Total Payments") +
  theme(legend.position="none")

# Values closer to the black line correspond to better predictions. Clearly, this
# prediction model leaves something to be desired.