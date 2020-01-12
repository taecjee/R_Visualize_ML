## iris Data
# 데이터 준비
data(iris)
str(iris)

# 학습 데이터와 테스트 데이터로 나눔
set.seed(1234)
ind <- sample(2, nrow(iris), replace = TRUE, prob = c(0.7, 0.3))
train.data <- iris[ind ==1,]
test.data <- iris[ind == 2,]

table(train.data$Species)
table(test.data$Species)

## Build a ctree
#install.packages("party")
library(party)

myFormula <- Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
iris_ctree <- ctree(myFormula, data = train.data)

(trTable <- table(predict(iris_ctree), train.data$Species))
(trAccuracy <- sum(diag(trTable)) / sum(trTable))

## Result and Predict ctree
print(iris_ctree)

plot(iris_ctree)
plot(iris_ctree, type = 'simple')

testPred <- predict(iris_ctree, newdata = test.data)
(teTable <- table(testPred, test.data$Species))
(teAccuracy <- sum(diag(teTable)) / sum(teTable))

## Train a Decision Tree with rpart
set.seed(1234)
ind2 <- sample(2, nrow(bodyfat), replace=T, prob = c(0.7, 0.3))
bodyfat.train <- bodyfat[ind2 == 1,]
bodyfat.test <- bodyfat[ind2 == 2,]

summary(bodyfat$DEXfat)
summary(bodyfat.train$DEXfat)
summary(bodyfat.test$DEXfat)

library(rpart)
myFormula <- DEXfat ~ age + waistcirc + hipcirc + elbowbreadth + kneebreadth
bodyfat_rpart <- rpart(myFormula, data = bodyfat.train, control = rpart.control(minsplit = 10))

## Result rpart & Pruning
print(bodyfat_rpart)

plot(bodyfat_rpart)
text(bodyfat_rpart, use.n = T)

opt <- which.min(bodyfat_rpart$cptable[, 'xerror'])
cp <- bodyfat_rpart$cptable[opt, 'CP']

bodyfat_prune <- prune(bodyfat_rpart, cp = cp)

plot(bodyfat_prune)
text(bodyfat_prune, use.n = T)

## rpart Model Evaluation
DEXfat_pred <- predict(bodyfat_prune, newdata = bodyfat.test)
xlim <- range(bodyfat$DEXfat)
plot(DEXfat_pred ~ DEXfat, data = bodyfat.test, 
     xlab = 'Observed', ylab = 'prediction', ylim = xlim, xlim = xlim)
abline(a = 0, b = 1, col = "red")

## Train a Random Forest
library(randomForest)
rf <- randomForest(Species ~ ., data = train.data, ntree = 100, proximity = T)

(trTable.rf <- table(predict(rf), train.data$Species))
(trAccuracy.rf <- sum(diag(trTable.rf)) / sum(trTable.rf))

print(rf)

# Error Rate
plot(rf, matin = '')

# Vairable Importance
importance(rf)

varImpPlot(rf)

testPred.rf <- predict(rf, newdata = test.data)

(teTable.rf <- table(testPred.rf, test.data$Species))
(teAccuracy.rf <- sum(diag(teTable.rf)) / sum(teTable.rf))















## 연습문제 
data("mammoexp", package = "TH.data")
mammoct <- ctree(ME ~ ., data = mammoexp) 
plot(mammoct)

# 클래스 가능성 예측
treeresponse(mammoct, newdata = mammoexp[1:10,])
mammoexp[1:10, "ME"]
