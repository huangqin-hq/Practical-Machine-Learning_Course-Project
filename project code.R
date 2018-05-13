# import caret package
library(caret)

# load training and testing data sets
training <- read.csv("pml-training.csv")
testing <- read.csv("pml-testing.csv")

# extract the indices of relevant predictors to be used in builidng the prediction model
predictorIdx <- c(grep("^roll", names(training)), grep("^pitch", names(training)), grep("^yaw", names(training)), grep("^accel", names(training)), grep("^gyros", names(training)), grep("^magnet", names(training)), grep("^total", names(training)))

# subset training and testing data sets with only relevant data, including predictors and outcome columns
# column 160 is 'class' variable for training set and 'problem_id' for testing set
trainPredSet <- training[, c(predictorIdx, 160)]
testPredSet <- testing[, c(predictorIdx, 160)]

# partition training data set into sub training set and sub testing set for cross-validation
set.seed(123)
inTrain <- createDataPartition(y = trainPredSet$classe, p = 0.8, list = FALSE)
cvTrain <- trainPredSet[inTrain, ]
cvTest <- trainPredSet[-inTrain, ]

# train a model based on Quadratic Discriminant Analysis method with 10-fold cross-validation
fitCtrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
modFit <- train(classe ~ ., data = cvTrain, method = "qda", trControl = fitCtrl)

# verify model accuracy with cross-validationg training set data
predTrain <- predict(modFit, newdata = cvTrain)
equalPredTrain <- (predTrain == cvTrain$classe)
print(sum(equalPredTrain)/length(equalPredTrain))
confusionMatrix(data = predTrain, reference = cvTrain$classe)

# verify model accuracy with cross-validationg test set data
predTest <- predict(modFit, newdata = cvTest)
equalPredTest <- (predTest == cvTest$classe)
print(sum(equalPredTest)/length(equalPredTest))
confusionMatrix(data = predTest, reference = cvTest$classe)

# apply model to testing set to generate predition results
testPrediction <- predict(modFit, newdata = testing)

# display prediction results
table(testing$problem_id, testPrediction)
