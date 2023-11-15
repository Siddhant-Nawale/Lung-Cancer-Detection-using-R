setwd("E:/VIT/2_SECOND YEAR/SEM_4/DS/CP")

library(datasets)
library(caTools)
library(party)
library(dplyr)
library(magrittr)

data <- read.csv("survey_lung_cancer.csv")
data$LUNG_CANCER <- as.factor(data$LUNG_CANCER)
# Splitting data in train and test data 
set.seed(120)
sample_data = sample.split(data, SplitRatio = 0.75)
train_data <- subset(data, sample_data == TRUE)
test_data <- subset(data, sample_data == FALSE)

#tuningn parameters
model<- ctree(LUNG_CANCER~., train_data)
plot(model)


predict_model<-predict(model, test_data)

m_at <- table(test_data$LUNG_CANCER, predict_model)
m_at

accdt <- sum(diag(m_at)) / sum(m_at)
cat('Accuracy for test is found to be',accdt)

