library(caret)
library(randomForest)


loandata<- loan_default_data_set
# Load dataset
data <- read.csv('data.csv')

## Do EDA and report you findings.
View(loandata)

#1 There are 20,000 rows of data and 21 columns or different variables. 

loandata1<-na.omit(loandata)
colSums(is.na(loandata))

#2 The columns that have missing data are pct_card_over_50_uti which has 1958, rep_income which has 1559 and rep_education that has 1.
# we created a new data set that excludes all the missing values named loandata1 to combat not working with missing values.


library(dplyr)


duplicates <- loandata1 %>%
  group_by_all() %>%
  filter(n() > 1) %>%
  ungroup()

print(duplicates)

#3 there are no duplicates in the data set. 

#4 if there were duplicates in the data we could drop them or even combine them throughout the data using the drop function
# and then for wrong classified data types you can convert the data using the as.factor or as.numeric functions

#5
plot(loandata1$avg_bal_cards,loandata1$rep_income,main= "Avg balance vs income level",xlab="Avg Balance", ylab = "Income level")

#6 
count(loandata1,rep_education)
# in this case the classification "other" would be underrepresented in the data set with a level of 120 
#7
count(loandata1,Def_ind)
# the data is not balanced for 14956 accounts did not default and 1697 did default
# I would use the F1 score to correct this imbalance. 

#8
summary(loandata1$rep_income)
# it is technically slightly left skewed but I would say it is approximately normal because the difference is only about 120

#9

library(dplyr)

default_by_education <- loandata1 %>%
  group_by(rep_education) %>%
  summarize(
    total_count = n(),                
    default_count = sum(Def_ind),     
    default_rate = mean(Def_ind) * 100 
  )
default_by_education

print(default_by_education)
# The education level that is most likely to default is high school with a 11.8% rate

#10 nothing else really stood out to me in the data set. 


# Split dataset
set.seed(42)
trainIndex <- createDataPartition(loandata1$Def_ind, p=0.8, list=FALSE)
train <- loandata1[trainIndex, ]
test <- loandata1[-trainIndex, ]

# Train and evaluate KNN
train$Def_ind<-as.factor(train$Def_ind)
test$Def_ind<-as.factor(test$Def_ind)
knn_model <- train(Def_ind ~ ., data=train, method='knn', tuneLength=5) 
pred_knn <- predict(knn_model, test)
print(confusionMatrix(pred_knn, test$Def_ind))


#Accuracy, precision, recall for knn
# accuracy= 89.52%
2962+19/2962+339+10+19
2981/3330

#precision = 65.51%
19/29

#recall = 5.30%
19/(19+339)

#2 interpreting confusion matrix
#the confusion matrix has 2962 true negatives, 19 true postives, 339 false negatives and 10 false postives

library(pROC)

roc_curve <- roc(test$Def_ind, pred_knn)
plot(roc_curve, col = "blue", main = "Roc Curve")
auc(roc_curve)

#3 AUC= .6434


# Train and evaluate Decision Tree
# Check column names
dt_model <- train(Def_ind~.,data=train,method='rpart')
pred_dt <- predict(dt_model, test)
print(confusionMatrix(pred_dt, test$Def_ind))
#Accuracy, precision, recall
#accuracy:
(2966+17)/(2962+335+12+17)

#Accuracy=89.69%

17/29
#Precision=58.62%

17/(17+335)
#Recall is 4.83%
#4 
#The number of cards opened in the last 12 months is the most important variable as it is significant at the 0.1% level

#5
#f1 score of KNN
2*(0.6551724*0.05307263)/(0.6551724+0.05307263)
#F1= 9.82%

#F1 of DT
2*(0.5862069*0.04829545)/(0.5862069+0.04829545)
#0.08923884

#The KNN model is better as it has a higher F1 score