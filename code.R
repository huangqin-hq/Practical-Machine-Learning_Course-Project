## import caret package
library(caret)

training <- read.csv("~herb/Desktop/pml-training.csv")
testing <- read.csv("~herb/Desktop/pml-testing.csv")

predictorIdx <- c(grep("^roll", names(training)), grep("^pitch", names(training)), grep("^yaw", names(training)), grep("^accel", names(training)), grep("^gyros", names(training)), grep("^magnet", names(training)), grep("^total", names(training)))

trainPredSet <- training[, c(predictorIdx, 160)]

testPredSet <- testing[, c(predictorIdx, 160)]

set.seed(123)
inTrain <- createDataPartition(y = trainPredSet$classe, p = 0.8, list = FALSE)
cvTrain <- trainPredSet[inTrain, ]
cvTest <- trainPredSet[-inTrain, ]
fitCtrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
modFit <- train(classe ~ ., data = cvTrain, method = "qda", trControl = fitCtrl)

predTrain <- predict(modFit, newdata = cvTrain)
equalPredTrain <- (predTrain == cvTrain$classe)
print(sum(equalPredTrain)/length(equalPredTrain))
confusionMatrix(data = predTrain, reference = cvTrain$classe)

predTest <- predict(modFit, newdata = cvTest)
equalPredTest <- (predTest == cvTest$classe)
print(sum(equalPredTest)/length(equalPredTest))
confusionMatrix(data = predTest, reference = cvTest$classe)

testPrediction <- predict(modFit, newdata = testing)

testPrediction
table(testing$problem_id, testPrediction)
