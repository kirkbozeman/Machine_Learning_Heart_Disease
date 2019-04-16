require("rpart.plot")
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

set.seed(100)

split <- sample.split(data$num_binary, SplitRatio = 0.7)
train <- subset(data, split == TRUE)
test <- subset(data, split == FALSE)

length(train$num_binary)
length(test$num_binary)

# check train/test results for consistency in result

prop.table(table(data$num_binary))
prop.table(table(train$num_binary))
prop.table(table(test$num_binary))


############################# DECISION TREES #############################

# look at https://www.statmethods.net/advstats/cart.html
# http://uc-r.github.io/regression_trees
# classification tree
class_tree <- rpart(num_binary ~ ., 
                    data=train, 
                    method="class"
)  # looks a little better visually w/ data vs datac


class_tree

# type and extra: https://www.rdocumentation.org/packages/rpart.plot/versions/3.0.6/topics/rpart.plot
rpart.plot(class_tree,
           type=2,  # 0-5 possible
           extra=101,  # 0-11 + 100
           box.palette="GnBu",
           #      branch.lty=3, # set branch line type
           shadow.col="gray"#, 
           #           nn=TRUE
)

# pruning needed?
printcp(class_tree)  # display cp tabl
class_tree$cptable[which.min(class_tree$cptable[,"xerror"]),"CP"]
plotcp(class_tree)

# pruned model
pruned <- prune(class_tree, cp=class_tree$cptable[which.min(class_tree$cptable[,"xerror"]),"CP"])

rpart.plot(pruned,
           type=2,  # 0-5 possible
           extra=101,  # 0-11 + 100
           box.palette="GnBu",
           #      branch.lty=3, # set branch line type
           shadow.col="gray"#, 
           #           nn=TRUE
)


pred <- predict(pruned, newdata = test)  # fit prediction

as.factor(pred)
test$num_binary
as.factor(as.integer(pred[,2] > .65))

############################# ROC / CONFUSION MATRIX #############################

require(ROCR)
ROCRpred = prediction(pred[,2], test$num_binary)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf)
plot(ROCRperf, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))  # can add  colorize=TRUE

# get AUC
auc_ROCR <- performance(ROCRpred, measure = "auc")
auc_ROCR@y.values[[1]]

confusionMatrix(test$num_binary, as.factor(as.integer(pred[,2] > 0.8)))  # caret


