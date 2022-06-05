## ggplot2

#install.packages("ggplot2")

library(ggplot2)

## 산점도 (Scatter Plot)
ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_smooth()

p <- ggplot(iris, aes(Sepal.Length, Sepal.Width, color=Species)) +
  geom_point(size=4)
print(p)

p <- ggplot(iris, aes(Sepal.Length, Sepal.Width, color=Species)) +
  geom_point(size=4) +
  geom_smooth(method="lm", se=FALSE)
print(p)

# 산점도 (Scatter Plot)
p <- ggplot(USArrests, aes(Murder, Assault))
p + geom_point(aes(size = UrbanPop/10))

p + geom_point(shape = 1)

p + geom_point(aes(size = UrbanPop/10), shape = USArrests$UrbanPop/10)

p + geom_point(aes(size = UrbanPop/10), colour = "blue")

p + geom_point(aes(size = UrbanPop%/%10)) +
  geom_point(aes(colour = factor(UrbanPop%/%10)))

## 선도표 (Line Plot)
p <- ggplot(airquality)

p + geom_line(aes(Day, Ozone))

library(dplyr)
#install.packages("lubridate")
library(lubridate)
airquality2 <- airquality %>% 
  mutate(dd = ymd(paste("1999", Month, Day, sep = "-")))

p <- ggplot(airquality2, aes(x = dd))

p + geom_line(aes(y = Ozone))

p + geom_line(aes(y = Ozone)) +
  geom_line(aes(y = Solar.R))

p + geom_line(aes(y = Ozone)) +
  geom_line(aes(y = Solar.R), colour = "blue")

# 선도표 (Line Plot)
library(tidyr)
airquality3 <- airquality2 %>% 
  gather(key = key, value = value, Ozone, Solar.R)

p <- ggplot(airquality3, aes(dd, value))
p + geom_line()

p <- ggplot(airquality3, aes(dd, value, group = key, color = key))
p + geom_line()

p + geom_line() + facet_wrap(~key, ncol = 1)


## 막대 그래프 (Bar Plot)
p <- ggplot(chickwts, aes(feed))

p + geom_bar()

p + geom_bar(aes(weight = weight))

chickwts2 <- chickwts %>% 
  group_by(feed) %>% 
  summarise(weight = mean(weight))

p <- ggplot(chickwts2, aes(feed, weight))

p + geom_col()

# 막대 그래프 (Bar Plot)
iris2 <- iris %>% 
  group_by(Species) %>% 
  summarise(Sepal.Length = mean(Sepal.Length),
            Sepal.Width = mean(Sepal.Width),
            Petal.Length = mean(Petal.Length),
            Petal.Width = mean(Petal.Width)) %>% 
  gather(key = key, value = value, Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)

p <- ggplot(iris2, aes(key, value, fill = Species))
p + geom_col(position = "dodge")
p + geom_col(position = "dodge") + coord_flip()
p + geom_col(position = "dodge") + facet_wrap(~Species, ncol = 1)

## 히스토그램 (Histogram)
ggplot(iris, aes(x = Sepal.Width)) +
  geom_histogram(aes(fill = ..count..), binwidth = 0.2)

ggplot(iris, aes(x = Sepal.Width)) +
  geom_histogram(aes(fill = ..count..), binwidth = 0.1)

ggplot(iris, aes(x = Sepal.Width)) +
  geom_histogram(aes(fill = ..count..), binwidth = 0.2) +
  geom_density()

ggplot(iris, aes(x = Sepal.Width)) +
  geom_histogram(aes(y = ..density.., fill = ..density..), binwidth = 0.2) +
  geom_density()

## 파이차트 (Pie Chart)
(p <- ggplot(mpg, aes(class)) +
  geom_bar())

p + coord_polar()
p + coord_polar(theta = "y")

(p2 <- ggplot(mpg, aes(x = "", fill = factor(class))) +
    geom_bar(width = 1))
p2 + coord_polar(theta = "y", start = pi / 3)

## 박스 플롯 (Box Plot)
ggplot(iris, aes(Species, Sepal.Length, fill = Species)) +
  geom_boxplot()

p <- ggplot(mpg, aes(class, hwy))
p + geom_boxplot()
p + geom_boxplot() + coord_flip()

## 동적 데이터 시각화
#install.packages("plotly")
library(plotly)

ggplotly(ggplot(USArrests, aes(Murder, Assault)) +
          geom_point(aes(size = UrbanPop/10)))
ggplotly(ggplot(airquality3, aes(dd, value)) + 
          geom_line())
ggplotly(ggplot(chickwts2, aes(feed, weight)) + 
          geom_col())
ggplotly(ggplot(iris, aes(x = Sepal.Width)) +
           geom_histogram(aes(fill = ..count..), binwidth = 0.2))
ggplotly(p + geom_boxplot())

## 동적 데이터 시각화
#install.packages("shiny")
library(shiny)











## 연습 문제
# 인구 데이터 피라미드 차트
#install.packages("plotrix")
library(plotrix)

popDataRate2010 <- popData2010 %>% 
  spread(Key, Population) %>% 
  mutate(Men_Rate = (Men/max(popData2010$Population)) * 100,
         Women_Rate = (Women/max(popData2010$Population)) * 100) %>% 
  filter(Ages != '계') %>% 
  mutate(Ages = ifelse(Ages == '5 - 9세',  '05 - 09세', Ages)) %>% 
  arrange(Ages)

par(mar = pyramid.plot(popDataRate2010$Men_Rate, popDataRate2010$Women_Rate, labels = popDataRate2010$Ages, main = "Korean Polulation Pyramid 2010", lxcol = "blue", rxcol = "pink", gap = 1.2, show.values = T))
  
# 2015년 데이터에 2010년 데이터 덧 씌우기
load("data/popDataRate2015.RData")

par(mar = pyramid.plot(popDataRate2015$Men_Rate, popDataRate2015$Women_Rate, labels = popDataRate2015$Age, main = "Korean Polulation Pyramid 2015", lxcol = "blue", rxcol = "pink", gap = 0, show.values = F, xlim = c(6,6), space = 0))

polygon(c(0, rep(popDataRate2010$Women_Rate, each = 2), 0), c(rep(0.5:(length(popDataRate2010$Ages)+0.5), each=2)), border="red")

polygon(c(0, rep(-popDataRate2010$Men_Rate, each = 2), 0), c(rep(0.5:(length(popDataRate2010$Ages)+0.5), each=2)), border="green")
