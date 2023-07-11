library(dplyr)
library(tidyverse)
library(ggpubr)
library(randomForest)
library(caret)
library(class)
library(boot)
library(tree)

# Processing the data
setwd("/Users/camelliasun/Desktop/textbook/stat362/final project/ObesityDataSet_raw_and_data_sinthetic (2)/final upload")
obesity <- read.csv("ObesityDataSet.csv")
obesity <- rename(obesity, result = NObeyesdad)

# BMI = Weight / (height * height)
# BMI is the only criterion to determine a person's obesity level, and it contains
# the information of height and weight, so we create a new variable BMI to replace
# weight and height, i.e., add BMI and remove weight & height.
obesity$BMI <- obesity$Weight / (obesity$Height * obesity$Height)
obesity <- obesity[, -c(3,4)]

# Also, notice that there are some messy data existing in variables FCVC, NCP,
# CH2O, FAF, TUE. So, we processed with these data.
obesity$FCVC <- round(obesity$FCVC) - 1
obesity$NCP <- round(obesity$NCP) - 1
obesity$CH2O <- round(obesity$CH2O) - 1
obesity$FAF <- round(obesity$FAF)
obesity$TUE <- round(obesity$TUE)
str(obesity)
view(obesity)
# Above is the processed data set.

# Data visualization(not included in the powerpoint)
# Bar chart
# Binary variables
g1 <- ggplot(obesity, aes(x = Gender)) + 
  geom_bar(fill = "antiquewhite3")+
  labs(y = "Number of Records")
g2 <- ggplot(obesity, aes(x = family_history_with_overweight)) + 
  geom_bar(fill = "antiquewhite3")+
  labs(y = "Number of Records", x = "Family History With Overweight \n(family_history_with_overweight)")
g3 <- ggplot(obesity, aes(x = FAVC)) + 
  geom_bar(fill = "antiquewhite3")+
  labs(y = "Number of Records", x = "Frequent Consumption of High Caloric Food \n(FAVC)")
g4 <- ggplot(obesity, aes(x = SMOKE)) + 
  geom_bar(fill = "antiquewhite3")+
  labs(y = "Number of Records", x = "Smoking or Not \n(SMOKE)" )
g5 <- ggplot(obesity, aes(x = SCC)) + 
  geom_bar(fill = "antiquewhite3")+
  labs(y = "Number of Records", x = "Calories Consumption Monitoring \n(SCC)")
# categorical variables
binary <- ggarrange(g1, g2, g3, g4, g5, ncol = 5, nrow = 1)
annotate_figure(binary, top = text_grob("Binary Variables",
                                             face = "bold", size = 14))
# Bar chart
# Categorical variables
g6 <- ggplot(obesity, aes(x = CAEC)) + 
  geom_bar(fill = "antiquewhite3")+
  labs(y = "Number of Records", x = "Consumptin of Food Between Meals \n(CAEC)")
g7 <- ggplot(obesity, aes(x = CALC)) + 
  geom_bar(fill = "antiquewhite3")+
  labs(y = "Number of Records", x = "Consumption of Alcohol \n(CALC)")
g8 <- ggplot(obesity, aes(x = MTRANS)) + 
  geom_bar(fill = "antiquewhite3")+
  labs(y = "Number of Records", x = "Transportation Used \n(MTRANS)")+
  theme(axis.text.x = element_text(angle = 20, vjust = 1, hjust = 1))
# Ordinal Variables
g9 <- ggplot(obesity, aes(x = factor(FCVC))) + 
  geom_bar(fill = "antiquewhite3")+
  labs(x = "Freqeuncy of Consumption of Vegetables \n (FCVC)",
       y = "Number of Records")
g10 <- ggplot(obesity, aes(x = factor(NCP))) + 
  geom_bar(fill = "antiquewhite3")+
  labs(x = "Number of Main Meals \n (NCP)",
       y = "Number of Records")
g11 <- ggplot(obesity, aes(x = factor(CH2O))) + 
  geom_bar(fill = "antiquewhite3")+
  labs(x = "Consumption of Water Daily \n (CH2O)",
       y = "Number of Records")
g12 <- ggplot(obesity, aes(x = factor(FAF))) + 
  geom_bar(fill = "antiquewhite3")+
  labs(x = "Frequency of phsical activity \n (FAF)",
       y = "Number of Records")
g13 <- ggplot(obesity, aes(x = factor(TUE))) + 
  geom_bar(fill = "antiquewhite3")+
  labs(x = "Time Using Technology Devices \n (TUE)",
       y = "Number of Records")
g14 <- ggplot(obesity, aes(x = result)) + 
  geom_bar(fill = "antiquewhite3")+
  labs(y = "Number of Records")+
  theme(axis.text.x = element_text(angle =20, vjust = 1, hjust = 1))
categorical <- ggarrange(g6, g7, g8, g9, g10, g11, g12, g13, g14, ncol = 3, nrow = 3)
annotate_figure(categorical, top = text_grob("Categorical Variables",
                                             face = "bold", size = 14))
# Continuous variables
# Histogram
g15 <- ggplot(obesity, aes(x = Age)) +
  geom_histogram(binwidth = 2, fill = "antiquewhite3", aes(y = ..density..)) +
  geom_density() +
  scale_x_continuous(breaks = seq(10, 70, 5), limits = c(10, 65))
g16 <- ggplot(obesity, aes(x = BMI)) +
  geom_histogram(binwidth = 2, fill = "antiquewhite3", aes(y = ..density..)) +
  geom_density() +
  labs(x = "Body Mass Index (BMI)")
continuous <- ggarrange(g15, g16, ncol = 2, nrow = 1)
annotate_figure(continuous, top = text_grob("Continuous Variables",
                                            face = "bold", size = 14))


# Graph - data visualization(display in the powerpoint)
#(1)
gender <- ggplot(obesity, aes(x = Gender)) + 
  geom_bar(fill = "antiquewhite3")+
  labs(y = "Number of Records")+
  theme(text = element_text(size = 15))
box_gender <- ggplot(obesity, aes(x = Gender, y = BMI)) +
  geom_boxplot()+
  theme(text = element_text(size = 15))

ggarrange(gender, box_gender, ncol = 2, nrow = 1)

#(2)
scatter <- ggplot(obesity, aes(x = Age, y = BMI, color = result)) +
  geom_point(position = position_jitter(width = 0.5, height = 0))+
  theme(text = element_text(size = 15))
box <- ggplot(obesity, aes(x = Age, y = result)) +
  geom_boxplot()+
  theme(text = element_text(size = 15))
ggarrange(box, scatter, ncol = 2, nrow = 1)

#(3)
x1 <- ggplot(obesity, aes(x = FAVC, fill = result))+
  geom_bar()+
  scale_x_discrete(name = "Frequent Consumption of High Caloric Food (FAVC)")+
  theme(text = element_text(size = 25))
x2 <- ggplot(obesity, aes(x = as.factor(FCVC), fill = result))+
  geom_bar(position = "dodge")+
  scale_x_discrete(name = "Frequency of Consumption of Vegetables (FCVC)",
                   labels = c("Never", "Sometimes", "Always"))+
  theme(text = element_text(size = 25))
x3 <- ggplot(obesity, aes(x = as.factor(CAEC), fill = result))+
  geom_bar()+
  scale_x_discrete(name = "Comsumption of food bettwen meals (CAEC)",
                   labels = c("No", "Sometimes", "Frequently", "Always"))+
  theme(text = element_text(size = 25))
ggarrange(x1, x2, x3, nrow = 1, ncol = 3)

#(4)
x4 <- ggplot(obesity, aes(x = as.factor(FAF), fill = result))+
  geom_bar(position = "dodge")+
  scale_x_discrete(name = "Frequency of Physical Activity per week (FAF)",
                   labels = c("0", "1-2", "2-4", "4-5"))+
  theme(text = element_text(size = 25))

#(5)
ggplot(obesity, aes(x = MTRANS, fill = factor(result))) + 
  geom_bar(fill = "antiquewhite3")+
  labs(y = "Number of Records", x = "Transportation Used \n(MTRANS)")+
  theme(axis.text.x = element_text(angle = 20, vjust = 1, hjust = 1))

#(6)
# Bar plot of Automobile and public transportation
g17 = ggplot(obesity %>% mutate(result = factor(result, levels = c("Insufficient_Weight","Normal_Weight","Overweight_Level_I", "Overweight_Level_II", "Obesity_Type_I", "Obesity_Type_II", "Obesity_Type_III"))) %>% filter(MTRANS == "Automobile"), 
             aes(x = result)) +
  geom_bar(fill = "antiquewhite3")+
  labs(y = "Number of Records", x = "", title = "Automobile")+
  theme(axis.text.x = element_text(angle = 20, vjust = 1, hjust = 1))
g18 = ggplot(obesity %>% mutate(result = factor(result, levels = c("Insufficient_Weight","Normal_Weight","Overweight_Level_I", "Overweight_Level_II", "Obesity_Type_I", "Obesity_Type_II", "Obesity_Type_III"))) %>% filter(MTRANS == "Public_Transportation"), 
             aes(x = result)) +
  geom_bar(fill = "antiquewhite3")+
  labs(y = "Number of Records", x = "", title = "Public Transportation")+
  theme(axis.text.x = element_text(angle = 20, vjust = 1, hjust = 1))
ggpubr::ggarrange(g17, g18, ncol = 2, nrow = 1)
obesity %>% 
  filter(result == "Obesity_Type_III", MTRANS == "Automobile")

#(7)
# Family History and harmful behaviours histogram
his_obesity <- obesity
str(his_obesity)
View(his_obesity)
his_obesity$result <- replace(his_obesity$result, his_obesity$result == "Obesity_Type_I", "Obesity")
his_obesity$result <- replace(his_obesity$result, his_obesity$result == "Obesity_Type_II", "Obesity")
his_obesity$result <- replace(his_obesity$result, his_obesity$result == "Obesity_Type_III", "Obesity")
his_obesity$result <- replace(his_obesity$result, his_obesity$result == "Overweight_Level_I", "Overweight")
his_obesity$result <- replace(his_obesity$result, his_obesity$result == "Overweight_Level_II", "Overweight")
his_obesity$result <- factor(his_obesity$result)

g19 <- his_obesity %>% 
  ggplot(aes(x = CALC, fill = result)) +
  geom_bar(position = "dodge")+
  scale_x_discrete(name = "Consumption of Alcohol (CALC)")+
  theme(text = element_text(size = 20))

g20 <- his_obesity %>% 
  ggplot(aes(x = SMOKE, fill = result)) +
  geom_bar(position = "dodge")+
  scale_x_discrete(name = "Smoking or Not (SMOKE)")+
  theme(text = element_text(size = 20))
g21 <- his_obesity %>% 
  ggplot(aes(x = family_history_with_overweight, fill = result)) +
  geom_bar(position = "dodge")+
  scale_x_discrete(name = "Family History With Overweight")+
  theme(text = element_text(size = 20))

graph <- ggarrange(g19, g20, g21, ncol = 3, nrow = 1)


# Random Forest

# The most important features that decide whether a person is in any of 
# obesity type or not, the response variable(result) is now binary.

# make a copy of the original data
new_obesity <- obesity
# replace people with obesity to 1, 0 otherwise and delete the column of BMI
new_obesity$result <- replace(new_obesity$result, new_obesity$result == "Obesity_Type_I", 1)
new_obesity$result <- replace(new_obesity$result, new_obesity$result == "Obesity_Type_II", 1)
new_obesity$result <- replace(new_obesity$result, new_obesity$result == "Obesity_Type_III", 1)
new_obesity$result <- ifelse(new_obesity$result==1, 1, 0)
new_obesity <- new_obesity[, -ncol(new_obesity)]
View(new_obesity)

# Change variables into categorical variables except Age
for (i in c(1, 3:15)){
  new_obesity[, i] <- factor(new_obesity[, i])
}
str(new_obesity)

# Use 10-fold cross validation method to find out the average accuracy
# and the important features that predict which category a person is among 2
# types of levels：obesity or not for a new given data
folds <- createFolds(new_obesity$result, k = 10)
accuracy <- rep(0, 10)
important_features <- matrix(0, nrow = 10, ncol = 14)
importance_graph <- rep(0, 10)
for(i in 1:10){
  # Fit the random forest model by training data
  rf <- randomForest(result~., data = new_obesity[-folds[[i]], ],
                     mtry = round((ncol(new_obesity) - 1) / 3), ntree = 500,
                     importance = TRUE)
  
  # Predict the test value
  rf_predict <- predict(rf, new_obesity[folds[[i]], ])
  
  # Making confusion matrix and find out the accuracy percentage
  confusion_matrix <- table(new_obesity$result[folds[[i]]], rf_predict)
  accuracy[i] <- sum(diag(confusion_matrix)) / length(folds[[i]])
  
  # Record the MeanDecreaseAccuracy for each feature
  important_features[i, ] <- importance(rf)[, 3]
  varImpPlot(rf)
}
# Average Accuracy
mean(accuracy)

# The MeanDecreaseAccuracy values of one feature in ten random forests are listed
# in one column inside important_features matrix, so we can use column sum to 
# obtain MeanDecreaseAccuracy sum for that feature.
# The larger the sum, the more important that feature is.
# Find the most important features in the dataset
sum_important <- colSums(important_features)
order(sum_important, decreasing = TRUE)
names(new_obesity)
# Variables are listed starting from the most important one:
# family_history_with_overweight, Age, CAEC, Gender, FCVC, NCP, FAVC, MTRANS

# Check the variable importance plot by setting i <- # inside the for loop
varImpPlot(rf)


# KNN
# make a copy of the original data
knn_obesity <- obesity
# replace people with obesity to 1, 0 otherwise
knn_obesity$result <- replace(knn_obesity$result, knn_obesity$result == "Obesity_Type_I", 1)
knn_obesity$result <- replace(knn_obesity$result, knn_obesity$result == "Obesity_Type_II", 1)
knn_obesity$result <- replace(knn_obesity$result, knn_obesity$result == "Obesity_Type_III", 1)
knn_obesity$result <- ifelse(knn_obesity$result==1, 1, 0)
View(knn_obesity)

# one-hot encoding to variable: gender, famly_history_with_overweight, FAVC, 
# CAEC, SMOKE, SCC, CALC, MTRANS
dummy <- dummyVars("~.", data = knn_obesity)
final_obesity <- data.frame(predict(dummy, newdata = knn_obesity))
# View(final_obesity)

# Make result become a factor
final_obesity$result <- factor(final_obesity$result)
str(final_obesity)
# Make the Age become integer
final_obesity$Age <- round(final_obesity$Age)

# function that calculates the knn with a specified k value and run knn
# algorithm 100 times to find out the average accuracy, average precision
# and average recall value from 100 simulations.
knn_func <- function(data, k){
  accuracy_sum <- 0
  precision_sum <- 0
  recall_sum <- 0
  for (i in 1:100){
    random_index <- sample(nrow(final_obesity), nrow(final_obesity) * 0.7)
    train <- final_obesity[random_index, -30]
    test <- final_obesity[-random_index, -30]
    train_labels <- final_obesity[random_index, ]$result
    test_labels <- final_obesity[-random_index, ]$result
    
    min_age <- min(train$Age)
    min_bmi <- min(train$BMI)
    max_age <- max(train$Age)
    max_bmi <- max(train$BMI)
    
    train$Age <- (train$Age - min_age) / (max_age - min_age)
    train$BMI <- (train$BMI - min_bmi) / (max_bmi - min_bmi)
    
    test$Age <- (test$Age - min_age) / (max_age - min_age)
    test$BMI <- (test$BMI - min_bmi) / (max_bmi - min_bmi)
    
    knn_predicted <- knn(train = train, test = test, cl = train_labels, k = k)
    accuracy <- mean(knn_predicted == test_labels)
    accuracy_sum <- accuracy + accuracy_sum
    precision_sum <- precision_sum + sum(knn_predicted == "1" & test_labels == "1") / sum(knn_predicted=="1")
    recall_sum = recall_sum + sum(knn_predicted == "1" & test_labels == "1") / sum(test_labels=="1")
  }
  return(list(round(accuracy_sum / 100, 4),
              round(precision_sum / 100, 4),
              round(recall_sum / 100, 4)))
}

# Find out the average accuracy, average precision and average recall
# for k between 4 to 15
accuracy_vector <- c()
precision_vector = c()
recall_vector = c()
for (k in seq(4,15)){
  res = knn_func(final_obesity, k)
  accuracy_vector <- c(accuracy_vector, res[[1]])
  precision_vector <- c(precision_vector, res[[2]])
  recall_vector = c(recall_vector, res[[3]])
}

#plot the average accuracy corresponding to the k value
plot(x = seq(4, 15), y = accuracy_vector*100, type = "b", xlab = "K Nearest Neighbors",
     ylab = "Accuracy Percentage", main = "Average Accuracy Percentage for K")
axis(side = 1, at = seq(4, 15, by = 1))

print(accuracy_vector)
print(precision_vector)
print(recall_vector)


# Logistic regression

# Processing data set
# Logistic regression requires our response to be 0 or 1, so we classify people
# with obesity to be 1, and people without obesity to be 0.
# We have three levels of obesity, and four levels of without obesity.

# make a copy of the original data
new_obesity <- obesity
# replace people with obesity to 1, 0 therwise
new_obesity$result <- replace(new_obesity$result, new_obesity$result == "Obesity_Type_I", 1)
new_obesity$result <- replace(new_obesity$result, new_obesity$result == "Obesity_Type_II", 1)
new_obesity$result <- replace(new_obesity$result, new_obesity$result == "Obesity_Type_III", 1)
new_obesity$result <- ifelse(new_obesity$result==1, 1, 0)

# Then we factor all categorical variables.
for (i in c(1, 3:15)){
  new_obesity[, i] <- as.factor(new_obesity[, i])
}
str(new_obesity)

# Also, we need to remove the row with CALC equals to "Always".Why?
# Note that "Always" only appears once in variable CALC, so that it will make the
# factor levels in training data and test data unbalanced, and this will lead to
# an error when we are modeling. So, we will remove such row.
filter(new_obesity, CALC == "Always")  # only one row
new_obesity <- new_obesity[new_obesity$CALC != "Always", ]  # remove this row

# Simulation - Fit a model that will predict if a person has obesity based on
# his/her habits and physical condition.
# The covariates we selected are based on the result of second type of random 
# forest.
accuracy1 <- rep(0, 1000)
for (i in 1:1000){
  random_index <- sample(nrow(new_obesity), size = nrow(new_obesity) * 0.7)
  train <- new_obesity[random_index, ]
  test <- new_obesity[-random_index, ]
  model <- glm(formula = result ~ Age + family_history_with_overweight +  
                 CAEC + FAVC + NCP + FCVC + Gender + MTRANS,
               family = binomial, data = train)
  prob <- predict(model, test, type = "response")
  predicted_class <- ifelse(prob > 0.5, 1, 0)
  accuracy1[i] <- mean(predicted_class == test$result)
}
mean(accuracy1)


# Decision Tree

# make a copy of the original data
new_obesity <- obesity
# replace people with obesity to 1, 0 oherwise
new_obesity$result <- replace(new_obesity$result, new_obesity$result == "Obesity_Type_I", 1)
new_obesity$result <- replace(new_obesity$result, new_obesity$result == "Obesity_Type_II", 1)
new_obesity$result <- replace(new_obesity$result, new_obesity$result == "Obesity_Type_III", 1)
new_obesity$result <- ifelse(new_obesity$result==1, "obese", "not obese")

# Take a copy of new_obesity dataset
obesity1 <- new_obesity

# Change all categorical variables to binary variables with value "FreqHigh" & "Freqlow"
for(i in c(5, 6, 7, 8, 10, 11)){
  obesity1[,i] <- ifelse(obesity1[,i] > 1, "FreqHigh", "Freqlow")
}

for(i in c(9, 12)){
  obesity1[,i] <- ifelse(obesity1[,i] >= 1, "FreqHigh", "Freqlow")
}
obesity1[, 13] <- replace(obesity1[, 13], obesity1[, 13] == "Frequently", 1)
obesity1[, 13] <- replace(obesity1[, 13], obesity1[, 13] == "Always", 1)
obesity1[, 13] <- ifelse(obesity1[13] == 1, "FreqHigh", "Freqlow")

# Then we factor all categorical variables.
for (i in c(1, 3:15)){
  obesity1[, i] <- as.factor(obesity1[, i])
}

# Simulation - Fit a tree that will predict if a person has obesity based on
# his/her habits and physical condition.
accuracy <- rep(0, 50)
for (j in 1:50){
  random_index <- sample(nrow(obesity1), size = nrow(obesity1) * 0.7)
  train1 <- obesity1[random_index, ]
  test1 <- obesity1[-random_index, ]
  
  # our tree
  tree_obesity<- tree(result ~ ., data = train1[, -16])
  table1 <- table(test1$result, predict(tree_obesity,test1, type = "class"))
  accuracy[j] <- sum(diag(table1))/nrow(test1)
}
mean(accuracy)

# Plot the tree 
plot(tree_obesity)
text(tree_obesity, pretty = 0, cex = 1.5)
# We want to prune the tree because there are too many tree leaves. (usually 12)
cv_fit <- cv.tree(tree_obesity, FUN = prune.misclass)
# But, from the cv_fit, we find that the smallest error usually occur at size = 12.
# So, we will change the number of tree leaves to 12. (Most of the time, this is 
# just the original tree.)
prune_fit <- prune.misclass(tree_obesity, best = 12)
table(test1$result, predict(prune_fit,test1, type = "class"))
