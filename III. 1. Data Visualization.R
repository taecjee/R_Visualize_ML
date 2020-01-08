## ggplot2

install.packages("ggplot2")

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
