#using cars dataset

head(cars)


linearMod <- lm(dist ~ speed, cars)
linearMod
summary(linearMod)



scatter.smooth(x=iris$Petal.Length, y=iris$Sepal.Length, main="Sepal Length ~ Petal Length")

iris_linMod <- lm(iris$Sepal.Length ~ iris$Petal.Length, iris)
summary(iris_linMod)
cor(cars$speed, cars$dist)

##### Using CARS Dataset #####


# Create Training and Test data 
set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(cars), 0.8*nrow(cars))  # row indices for training data
trainingData <- cars[trainingRowIndex, ]  # model training data
testData  <- cars[-trainingRowIndex, ]   # test data

# Build the model on training data -
lmMod <- lm(dist ~ speed, data=trainingData)  # build the model
distPred <- predict(lmMod, testData)  # predict distance

summary(lmMod)
AIC (lmMod)

#scatter.smooth(x=testData$speed, y=testData$dist, main="Actua-Dist ~ Actual-Speed")  # scatterplot
actuals_preds <- data.frame(cbind(testData, predicteds=distPred))  # make actuals_predicteds dataframe.
cor(actuals_preds$dist, actuals_preds$predicteds)  # 82.7%
head(actuals_preds)
#scatter.smooth(x=actuals_preds$speed, y=actuals_preds$predicteds, main="Pred_Dist ~ Speed")  # scatterplot

min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))

mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)


plot(x=actuals_preds$dist, y = actuals_preds$predicteds,
     xlab='Predicted Values',
     ylab='Actual Values',
     main='Predicted vs. Actual Values')
abline(a = 0, b =1)

##### Using IRIS dataset #####
##### Sepal and Petal Length

set.seed(100)  # setting seed to reproduce results of random sampling
trainingIrisIndex <- sample(1:nrow(iris), 0.8*nrow(iris))  # row indices for training data
trainingIrisData <- iris[trainingIrisIndex, ]  # model training data
trainingIrisData <- trainingIrisData[, -c(2,4,5)]
testIrisData  <- iris[-trainingIrisIndex, ]   # test data
testIrisData <- testIrisData[,-c(2,4,5)]
# Build the model on training data -
lmModIris <- lm(Sepal.Length ~ Petal.Length, data=trainingIrisData)  # build the model
predIris <- predict(lmModIris, testIrisData)  # predict distance

summary(lmModIris)
AIC (lmModIris)

scatter.smooth(x=testIrisData$Petal.Length, y=testIrisData$Sepal.Length, main="Actual")  # scatterplot
actuals_preds_Iris <- data.frame(cbind(testIrisData, predicteds=predIris))  # make actuals_predicteds dataframe.
cor_accu_Iris <- cor(testIrisData$Sepal.Length, actuals_preds_Iris$predicteds)  # 82.7%
correhead(actuals_preds)
scatter.smooth(x=actuals_preds_Iris$Sepal.Length, y=actuals_preds_Iris$predicteds, main="Petal ~ Sepal")  # scatterplot

min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))
# => 58.42%, min_max accuracy
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)
# => 48.38%, mean absolute percentage deviation



##### Using IRIS dataset #####
##### Sepal and Petal width

set.seed(100)  # setting seed to reproduce results of random sampling
trainingIrisIndex_width <- sample(1:nrow(iris), 0.8*nrow(iris))  # row indices for training data
trainingIrisData_width <- iris[trainingIrisIndex_width, ]  # model training data
trainingIrisData_width <- trainingIrisData_width[, -c(2,4,5)]
testIrisData  <- iris[-trainingIrisIndex, ]   # test data
testIrisData <- testIrisData[,-c(2,4,5)]
# Build the model on training data -
lmModIris <- lm(Sepal.Length ~ Petal.Length, data=trainingIrisData)  # build the model
predIris <- predict(lmModIris, testIrisData)  # predict distance

summary(lmModIris)
AIC (lmModIris)

scatter.smooth(x=testIrisData$Petal.Length, y=testIrisData$Sepal.Length, main="Actual")  # scatterplot
actuals_preds_Iris <- data.frame(cbind(testIrisData, predicteds=predIris))  # make actuals_predicteds dataframe.
cor_accu_Iris <- cor(testIrisData$Sepal.Length, actuals_preds_Iris$predicteds)  # 82.7%
correhead(actuals_preds)
scatter.smooth(x=actuals_preds_Iris$predicteds, y=actuals_preds_Iris$Sepal.Length, main="Petal ~ Sepal")  # scatterplot

plot(x=actuals_preds_Iris$Sepal.Length, y=actuals_preds_Iris$predicteds,
     xlab='Predicted Values',
     ylab='Actual Values',
     main='Predicted vs. Actual Values')
abline(a = 0, b =1)
x = predict(lmModIris, testIrisData)
plot(x=x, y=actuals_preds_Iris$Sepal.Length,
     xlab='Predicted Values',
     ylab='Actual Values',
     main='Predicted vs. Actual Values')
abline(lmModIris, col = "blue")

min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))
# => 58.42%, min_max accuracy
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)
# => 48.38%, mean absolute percentage deviation

############ Example with Confidence Interval ###########
#### With iris Sepal and Petal Length


set.seed(100)  # setting seed to reproduce results of random sampling
trainingIrisIndex <- sample(1:nrow(iris), 0.8*nrow(iris))  # row indices for training data
trainingIrisData <- iris[trainingIrisIndex, ]  # model training data
trainingIrisData <- trainingIrisData[, -c(2,4,5)]
testIrisData  <- iris[-trainingIrisIndex, ]   # test data
testIrisData <- testIrisData[,-c(2,4,5)]
# Build the model on training data -
iris_fit <- lm(Petal.Length ~ Sepal.Length, data = trainingIrisData)
iris_test <- predict(iris_fit, testIrisData, interval = "confidence", level = 0.95)
iris_test
summary(iris_fit)
plot(trainingIrisData$Sepal.Length, trainingIrisData$Petal.Length, main = "Regression Test")
abline(iris_fit, col = "red")

plot(testIrisData$Sepal.Length, testIrisData$Petal.Length, main = "Regression Test")
abline(iris_fit, col = "red")

lines(testIrisData$Sepal.Length, iris_test[,2], col = "blue", lty = 2)

lines(testIrisData$Sepal.Length, iris_test[,3], col = "blue", lty = 2)

#Prediction Interval
iris_pred_int <- predict(iris_fit, testIrisData, interval = "prediction", level = 0.95)
lines(testIrisData$Sepal.Length, iris_pred_int[,2], col = "orange", lty = 2)

lines(testIrisData$Sepal.Length, iris_pred_int[,3], col = "orange", lty = 2)


####Using ggplot2
library(ggplot2)
ggplot(testIrisData, aes(x=Sepal.Length, y=Petal.Length))+
        geom_point()+
        geom_smooth(method=lm, se=TRUE, level = 0.90)


iris_test_90 <- predict(iris_fit, testIrisData, interval = "confidence", level = 0.90)

plot(testIrisData$Sepal.Length, testIrisData$Petal.Length, main = "Regression Test")
abline(iris_fit, col = "red")

lines(testIrisData$Sepal.Length, iris_test_90[,2], col = "blue", lty = 2)

lines(testIrisData$Sepal.Length, iris_test_90[,3], col = "blue", lty = 2)
