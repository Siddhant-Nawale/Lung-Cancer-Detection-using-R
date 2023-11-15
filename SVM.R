setwd("E:/VIT/2_SECOND YEAR/SEM_4/DS/CP")
#4000
data <- read.csv("survey_lung_cancer1.csv")
data$LUNG_CANCER <- as.factor(data$LUNG_CANCER)

set.seed(120)
library(caTools)

set.seed(123)
split = sample.split(data$LUNG_CANCER, SplitRatio = 0.80)

training_set = subset(data, split == TRUE)
test_set = subset(data, split == FALSE)

#type = 'C-classification'
classifier = svm(LUNG_CANCER~.,data = training_set,kernel = 'radial')
predict_model<-predict(classifier, test_set)

m_at <- table(test_set$LUNG_CANCER, predict_model)
m_at

accdt <- sum(diag(m_at)) / sum(m_at)
cat('Accuracy for test is found to be',accdt)


#accuracy
#presition
#Recall

#in paper
#sencitivity specificity
#accuracy  roc curve auc