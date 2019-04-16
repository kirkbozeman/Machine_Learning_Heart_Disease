require(caTools)
require(e1071)
require(caret)

############################# GET DATASET #############################

# works on mac
data <- read.csv("~//git repos//Source-CCM//_DSU//DSU_R//view_processed_cleveland.txt")
data$num <- NULL  # only use num_binary

# set categorical as factors 
data$sex <- as.factor(data$sex)
data$cp <- as.factor(data$cp)
data$fbs <- as.factor(data$fbs)
data$restecg <- as.factor(data$restecg)
data$exang <- as.factor(data$exang)
data$slope <- as.factor(data$slope)
data$thal <- as.factor(data$thal)
data$num_binary <- as.factor(data$num_binary)


############################# CREATE TRAIN/TEST #############################

set.seed(101)

split <- sample.split(data$num_binary, SplitRatio = 0.7)
train <- subset(data, split == TRUE)
test <- subset(data, split == FALSE)

length(train$num_binary)
length(test$num_binary)

# check train/test results for consistency in result

prop.table(table(data$num_binary))
prop.table(table(train$num_binary))
prop.table(table(test$num_binary))
# it's ok...


############################# SVM #############################

# https://www.r-bloggers.com/machine-learning-using-support-vector-machines/

fit <- svm(num_binary ~ ., 
                 data=train, 
                 method="C-classification", 
                 kernel="linear",
                 scale=TRUE)  # scaling makes it worse/not work

summary(fit)
fit$SV
fit$gamma

pred <- predict(fit, newdata = test)  # fit prediction

############################# CONFUSION MATRIX #############################

confusionMatrix(pred, test$num_binary)  # caret



