# Predict Customer Churn with R

library(plyr)
library(corrplot)
library(ggplot2)
library(gridExtra)
library(ggthemes)
library(caret)
library(MASS)
library(randomForest)
library(party)

churn <- read.csv('Telco-Customer-Churn.csv')
str(churn)

# customerID
# gender (female, male)
# SeniorCitizen (Whether the customer is a senior citizen or not (1, 0))
# Partner (Whether the customer has a partner or not (Yes, No))
# Dependents (Whether the customer has dependents or not (Yes, No))
# tenure (Number of months the customer has stayed with the company)
# PhoneService (Whether the customer has a phone service or not (Yes, No))
# MultipleLines (Whether the customer has multiple lines r not (Yes, No, No phone service))
# InternetService (Customer’s internet service provider (DSL, Fiber optic, No))
# OnlineSecurity (Whether the customer has online security or not (Yes, No, No internet service))
# OnlineBackup (Whether the customer has online backup or not (Yes, No, No internet service))
# DeviceProtection (Whether the customer has device protection or not (Yes, No, No internet service))
# TechSupport (Whether the customer has tech support or not (Yes, No, No internet service))
# streamingTV (Whether the customer has streaming TV or not (Yes, No, No internet service))
# streamingMovies (Whether the customer has streaming movies or not (Yes, No, No internet service))
# Contract (The contract term of the customer (Month-to-month, One year, Two year))
# PaperlessBilling (Whether the customer has paperless billing or not (Yes, No))
# PaymentMethod (The customer’s payment method (Electronic check, Mailed check, Bank transfer (automatic), Credit card (automatic))
# MonthlyCharges (The amount charged to the customer monthly—numeric)
# TotalCharges (The total amount charged to the customer—numeric)
# Churn ( Whether the customer churned or not (Yes or No))

# how many missing value?
sapply(churn, function(x) sum(is.na(x)))

# remove it
churn <- churn[complete.cases(churn), ]

# change “No internet service” to “No” for “OnlineSecurity”, “OnlineBackup”, “DeviceProtection”, “TechSupport”, “streamingTV”, “streamingMovies”.
cols_recode1 <- c(10:15)
for(i in 1:ncol(churn[,cols_recode1])) {
  churn[,cols_recode1][,i] <- as.factor(mapvalues
                                        (churn[,cols_recode1][,i], from =c("No internet service"),to=c("No")))
}

# change “No phone service” to “No” for column “MultipleLines”
churn$MultipleLines <- as.factor(mapvalues(churn$MultipleLines, 
                                           from=c("No phone service"),
                                           to=c("No")))



