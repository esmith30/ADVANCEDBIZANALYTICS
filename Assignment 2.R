# Load libraries
library(tidyverse)
library(caTools)

# https://www.rdocumentation.org/packages/datasets/versions/3.6.2/topics/mtcars 
# We'll use the mtcars dataset and predict whether a car is automatic (am = 0) or manual (am = 1).




View(admit) #View data set.

#Exploratory Data Analysis. 
# Shapes of the data set- rows and columns 
# Datatypes
# Plot the variables
# Find the missing values
# Duplicates

# Convert 'am' to a factor (categorical variable)
admit$admit <- as.factor(as.factor(admit$admit))

# View structure of dataset
str(admit)

# Summary statistics
summary(admit)

#1 Are the classes balanced(admit/ don't admit) in the data set
# it is not balanced as there are 273 not admits and 127 admits
#2 It is technically skewed as the mean is higher than the median, 
# but you could say it is approximately normal because it is only about 7 points off
hist(admit$gre)
#Check for class balance
table(admit$admit)

class_proportions <- prop.table(table(admit$admit))
class_proportions


# Split the data into training and testing sets.
# Set seed for reproducibility
set.seed(1)

# Split the dataset into training (70%) and testing (30%)
split <- sample.split(admit$admit, SplitRatio = 0.7)

# Create training and testing sets
# https://search.r-project.org/CRAN/refmans/caTools/html/sample.split.html
train_data <- subset(admit, split == TRUE)
test_data <- subset(admit, split == FALSE)

# Check dataset dimensions
dim(train_data)
dim(test_data)


#Fit the Logistic Regression Model

# Train the logistic regression model
log_model <- glm(admit ~ gre + gpa + rank, data = train_data, family = binomial) # Binomial Distribution, Y variable is binary
# https://www.datacamp.com/doc/r/glm 

# Display model summary
summary(log_model)
#3 Gpa and rank are the only significant variables, while gpa is significant at the .05 level and the rank is significant at the .001 level
#  for the intercept every -3.09 every 1 i ncrease leads to a .008 increase in probabaility of admission
# for gpa every 1 increase leads to a .09 increase in proabability of admission
# every  class ranks adds a .05 increase to admissison




#The bigger the number in linear regression or logistic regression is the more important number

# glm() fits a generalized linear model.
# am ~ mpg + hp + wt means we predict am based on mpg (miles per gallon), hp (horsepower), and wt (weight).
# family = binomial specifies logistic regression (since it models probabilities).
# summary(log_model) displays coefficients, significance levels, and model fit statistics.

# Predict probabilities on the test dataset
pred_probs <- predict(log_model, test_data, type = "response")
pred_probs
# Convert probabilities to binary predictions (threshold = 0.5)
pred_classes <- ifelse(pred_probs > 0.5, 1, 0)

# Convert to factor for comparison
pred_classes <- as.factor(pred_classes)

# Display predictions
head(pred_probs)
head(pred_classes)

### Print predictions and true y values as dataframe
do.call(rbind, Map(data.frame, predicted_classes=pred_classes, admit=test_data$admit))

#Evaluate model performance

# Create confusion matrix
conf_matrix <- table(Predicted = pred_classes, Actual = test_data$admit)
conf_matrix

# Compute accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
accuracy
#4 The accuracy for the data is .7166667, we also have 77 true negatives and 9 true postives with 29 false negatives and 5 false postives
# recall = 77/106 = 0.72
# precision 77/82 = 0.94 

#5 Rank is the most important variable when predicting admission as it is the most significant varable in the data set. 
# Print results
print(conf_matrix)
print(paste("Accuracy:", round(accuracy, 4)))


# Visualizing predictions vs. actuals
ggplot(test_data, aes(x = gre, y = as.numeric(as.character(admit)), color = as.factor(pred_classes))) +
  geom_point(size = 3) +
  labs(title = "Predicted vs Actual admission",
       x = "Graduated Record Exam Scores (GRE)",
       y = "Admission(0 = Rejection, 1 = Admission)") +
  scale_color_manual(values = c("red", "blue"), name = "Prediction")
#5 



#Same code but different data set 
# Follow this code as is just change data set
# fit the model and explain what were doing
# evaluate the model by precsion, recall, specificity
# More about understanding the code than it is writing it




