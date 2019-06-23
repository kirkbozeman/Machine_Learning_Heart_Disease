require(randomForest)
require(ggplot2)
require(rminer)
require(caret)
require(inTrees)
require(gdata)
require(caTools)
require(ROCR)


############################# GET DATASET #############################

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

set.seed(103) # set sample seed for reproducibility, must run w/ all code

split <- sample.split(data$num_binary, SplitRatio = 0.7)
train <- subset(data, split == TRUE)
test <- subset(data, split == FALSE)

length(train$num_binary)
length(test$num_binary)

# check train/test results for consistency in result

prop.table(table(data$num_binary))
prop.table(table(train$num_binary))
prop.table(table(test$num_binary))

############################# CREATE RANDOM FOREST #############################

# random forest on train
rf <- randomForest(as.factor(num_binary) ~ ., data = train, 
                   ntree = 200, # Hyperparameter: Nbr of trees in forest
                   maxnodes = 10, # Hyperparameter: Max nbr of terminal nodes (leaves or depth)
                   nodesize = 100, # Hyperparameter: minimum size of terminal nodes
                   # seems to do better w/ higher nodesize
                   importance = TRUE
)

rf  # contains a confusion matrix (on the fit)
plot(rf)  # this is test error

pred <- predict(rf, newdata = test)  # fit prediction

# confusion matrix
confusionMatrix(as.factor(test$num_binary), pred)  # caret

# interesting
summary(rf)
importance(rf)
varImpPlot(rf)
rf$forest
str(rf)

############################# extreme random forest #############################

# forked from https://daviddalpiaz.github.io/stat430fa17/labs/enslab/enslab.html

require(rJava)
require(extraTrees)

cv_5 = trainControl(method = "cv", number = 5)
et_grid =  expand.grid(mtry = 4:7, numRandomCuts = 1:100)

erf <- train(as.factor(num_binary) ~ ., 
             data = train,
             method = "extraTrees",
             trControl = cv_5,
             tuneGrid = et_grid,
             numThreads = 4             
)

plot(erf)

pred <- predict(erf, newdata = test)  # fit prediction

# confusion matrix
confusionMatrix(as.factor(test$num_binary), pred)  # caret


