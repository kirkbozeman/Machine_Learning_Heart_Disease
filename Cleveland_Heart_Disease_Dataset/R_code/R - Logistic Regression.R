require(caTools)
library(pscl)
require(ROCR)
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


############################# (BINOMIAL) LOGISTIC REGRESSION #############################

# must use binary, logit demands y E [0,1]
fit <- glm(as.factor(num_binary) ~ ., 
           family = binomial(link = 'logit'),
           data=train)

summary(fit) # most show as stat insig
pR2(fit)  # mcfadden R**2 index

# remove features that are not stat sig
fit <- glm(as.factor(num_binary) ~ sex + cp + thalach + trestbps 
                                     + exang + slope + ca + thal, 
           family = binomial(link = 'logit'),
           data=train)

summary(fit)  # AUC better than above
pR2(fit)  # mcfadden R**2 index

# predict
pred <- predict(fit, newdata=test, type="response")
summary(pred)

pred
test$num_binary

############################# ROC / CONFUSION MATRIX #############################

# sensitivity = true positive rate of model
# specificity = true negative rate of the model

# confusion matrix:
# TP | FP
# FN | TN

# create ROC
ROCRpred = prediction(pred, test$num_binary)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf)
plot(ROCRperf, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))  # can add colorize=TRUE

# get AUC
auc_ROCR <- performance(ROCRpred, measure = "auc")
auc_ROCR@y.values[[1]]


# basic confusion matrix
#table(test$num_binary, pred > 0.7)  # note: test actual response vs test pred response

# detailed confustion matrix (from caret)
# both must be factors w/ overlap in values (i.e. both 1/0 or T/F)
# must add threshold for predictor
# THIS MAY NOT WORK WITH THE FAKE DATA ON THIS THRESHOLD
confusionMatrix(as.factor(test$num_binary), as.factor(as.integer(as.logical(pred > 0.3))))

