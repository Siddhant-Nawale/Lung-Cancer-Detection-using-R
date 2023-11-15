
setwd("E:/VIT/2_SECOND YEAR/SEM_4/DS/CP")

library(caTools) 
library(randomForest) 

data <- read.csv("survey_lung_cancer.csv")
data$LUNG_CANCER <- as.factor(data$LUNG_CANCER)
set.seed(120)
split <- sample.split(data, SplitRatio = 0.75) 


train <- subset(data, split == "TRUE") 
test <- subset(data, split == "FALSE")
#mtry

#train and traincontrol in caret

#classifier_RF = randomForest(x = train,  y = train$LUNG_CANCER, ntree = 500) 
classifier_RF = randomForest(LUNG_CANCER~.,  data = train ,importance = TRUE, ntree = 100) 

classifier_RF 

y_pred = predict(classifier_RF, newdata = test[,-16]) 
y_pred

# Confusion Matrix 
confusion_mtx = table(test[, 16], y_pred) 

confusion_mtx 



#print(table(test[, 16], y_pred))
acc=sum(diag(confusion_mtx ))/sum(confusion_mtx )
cat("acc : ")
print(acc)
