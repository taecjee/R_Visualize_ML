## Australian CPI (Consumer Price Index) data
year <- rep(2008:2010, each = 4)
quarter <- rep(1:4, 3)
cpi <- c(162.2,164.6,166.5,166,166.2,167,168.6,169.5,171,172.1,173.3,174)
plot(cpi, xaxt = "n", ylab = "CPI", xlab = "")
axis(1, labels = paste(year, quarter, sep = "Q"), at = 1:12, las = 3)

## Train by Linear Regression
cor(year, cpi)
cor(quarter, cpi)
fit <- lm(cpi ~ year + quarter)

attributes(fit)
fit$coefficients
residuals(fit)
summary(fit)

cpi2011 <- fit$coefficients[[1]]+fit$coefficients[[2]]*2011 + fit$coefficients[[3]]*(1:4)
cpi2011

## Prediction of CPIs in 2011
data2011 <- data.frame(year = 2011, quarter = 1:4)
cpi2011 <- predict(fit, newdata = data2011)
style <- c(rep(1,12), rep(2,4))

plot(c(cpi, cpi2011), xaxt = 'n', ylab = 'CPI', xlab = '', pch = style, col = style)
axis(1, at = 1:16, las = 3, 
     labels = c(paste(year, quarter, sep='Q'), '2011Q1', '2011Q2', '2011Q3','2011Q4'))

## Generalized Linear Model (GLM)
install.packages("TH.data")
data('bodyfat', package='TH.data')
myFormula <- DEXfat ~ age + waistcirc + hipcirc + elbowbreadth + kneebreadth
bodyfat.glm <- glm(myFormula, family = gaussian('log'), data = bodyfat)
summary(bodyfat.glm)

pred <- predict(bodyfat.glm, type = 'response')
plot(bodyfat$DEXfat, pred, xlab = 'Observed', ylab = 'Prediction')
abline(a = 0, b = 1, col = 'red', lwd = 2)

## Build Logistic Regression Model
# 독립 변수의 가능성이 있는 변수 확인
library(MASS)
data("bacteria")
str(bacteria)

# 독립 변수 탐색
table(bacteria$y)
bacteria2 <- bacteria
bacteria2$y <- ifelse(bacteria2$y == 'n', 0, 1)

# 박테리아 모델 개발 
bacteria_model <- glm(y ~ ap + hilo + week + trt, 
                      data = bacteria2, family = "binomial")

# 모델 결과 요약
summary(bacteria_model)

# 박테리아 가능성 계산 
bacteria2$bacteria_prob <- predict(bacteria_model, type = "response")

# 박테리아의 평균으로 박테리아의 가능성 확인
mean(bacteria2$y)

# 평균보다 크면 박테리아 가능으로 예측
bacteria2$bacteria_pred <- ifelse(bacteria2$bacteria_prob > mean(bacteria2$y), 1, 0)

# 모델의 정확도 계산
mean(bacteria2$y == bacteria2$bacteria_pred)







## 연습 문제
# Australian CPI 예측 값을 ggplot으로 그리기

# 날짜와 cpi 데이터로 데이터 프레임 생성
newCPI <- data.frame(date = paste(year, quarter, sep='Q'), CPI = cpi)
# 예측 날짜와 예측 cpi 데이터 추가
newCPI <- rbind(newCPI, data.frame(date = c('2011Q1', '2011Q2', '2011Q3','2011Q4'), CPI = cpi2011))
# 예측 여부를 나타내는 컬럼 추가
newCPI <- cbind(newCPI, Predict = (c(rep('Y',12), rep('N',4))))

library(ggplot2)
# 산점도 그리기
ggplot(newCPI, aes(date, CPI)) + geom_point(aes(shape = Predict))


# 콘크리트 데이터 셋
"https://archive.ics.uci.edu/ml/machine-learning-databases/concrete/compressive/Concrete_Data.xls"

# 데이터 읽기
library(readxl)
concrete <- read_xls('../data/Concrete_Data.xls', sheet = 1)

# 컬럼명 변환하기
View(concrete)
colnames(concrete) <- c("Cement", "Blast", "FlyAsh", "Water", "Superplasticizer", "CoarseAggregate", "FineAggregate", "Age", "Strength")

# LM 모델 만들기
fit <- lm(Strength ~ ., data = concrete)
fit

# 예측하기
StrengthPred <- predict(fit)

# 그림으로 나타내기
plot(concrete$Strength, StrengthPred, xlab = 'Observed', ylab = 'Prediction')
abline(a = 0, b = 1, col = 'red', lwd = 2)


## 국내 인구 증가율 lm 모델 만들기
# 데이터 읽기
load("data/popRateData.RData")
View(popRateData)

# 그림으로 나타내기
p <- ggplot(as.data.frame(popRateData), aes(year, popRate)) + 
  geom_point()
p

# 연도를 2060년 까지 나타내기
p2 <- p + xlim(1960, 2060) + ylim(-3, 3)
p2

# lm으로 예측하기
pop.lm <- lm(popRate ~ year, as.data.frame(popRateData))
p3 <- p2 + geom_abline(intercept = pop.lm$coefficients[1], slope = pop.lm$coefficients[2], size = 1, color = "red")
p3

p2 + stat_smooth(method = "lm", se = FALSE)

## 국내 인구증가율 예측하기
# 2060년까지 예측하기
year2 <- data.frame(year = rep(2019:2060))
predRate <- predict(pop.lm, year2)
predPopRate <- data.frame(year = year2, popRate = predRate)

# 구분자를 넣어서 관측치와 예측치를 하나로 나타내기
popRateData2 <- cbind(as.data.frame(popRateData), type = "org")
predPopRate2 <- cbind(as.data.frame(predPopRate), type = "pred")
totalPopRate.lm <- rbind(popRateData2, predPopRate2)

# 그림으로 나타내기
p.lm <- ggplot(totalPopRate.lm, aes(year, popRate, col = type))
p.lm + geom_point()

## 통계청 예측치와 비교하기
# 데이터 읽기 (popDataKostat)
load("data/popData.RData")
View(popDataKostat)

# 예측치와 통계데이터를 하나로 합치기
popDataKostat2 <- cbind(popDataKostat[-1, c("year", "popRate")], type = "kostat")
totalPopRate.lm2 <- cbind(totalPopRate.lm[, c("year", "popRate")], type = "pred")
totalPopRate.lmNew <- rbind(popDataKostat2, totalPopRate.lm2)

# 그림으로 나타내기
p.lm2 <- ggplot(totalPopRate.lmNew, aes(year, popRate, col = type))
p.lm2 + geom_point()
