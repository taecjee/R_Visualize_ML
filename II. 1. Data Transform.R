#install.packages("dplyr")
library(dplyr)

# 조건에 따른 관찰값의 선택

filter(mtcars, mpg >= 30)
filter(mtcars, mpg >= 30 & wt < 1.8)

filter(mtcars, mpg <= 30 & (cyl == 6 | cyl == 8) & am == 1)
filter(mtcars, mpg <= 30 & cyl %in% c(6,8) & am == 1)
filter(mtcars, mpg <= 30, cyl %in% c(6,8), am == 1)

filter(mtcars, mpg >= median(mpg) & mpg <= quantile(mpg, probs = 0.75))
filter(mtcars, between(mpg, median(mpg), quantile(mpg, probs = 0.75)))

# 관찰값의 단순 임의 추출
sample_n(mtcars, size = 3)
sample_frac(mtcars, size = 0.1)

myIdx <- sample(1:nrow(mtcars), size = 3)
mtcars[myIdx,]

# 특정 변수의 값이 가장 큰(작은) 관찰값 선택
top_n(mtcars, n = 2, wt=mpg)
top_n(mtcars, n = -2, wt=mpg)

# 관찰값의 정렬
arrange(mtcars, mpg)
arrange(mtcars, desc(mpg))
arrange(mtcars, mpg, desc(wt))

# 변수의 선택
select(mtcars, mpg, cyl, disp)
select(mtcars, mpg:disp)
select(mtcars, 1:3)

select(mtcars, -mpg)
select(mtcars, -mpg, -cyl, -disp)
select(mtcars, -(mpg:disp))
select(mtcars, -(1:3))

select(mtcars, starts_with("d"))
select(mtcars, ends_with("t"))
select(mtcars, contains("a"))

#
select(mtcars, contains("ar", ignore.case = FALSE))

select(mtcars, -starts_with("d"))
select(mtcars, -contains("a"))

select(mtcars, vs, wt, everything())

# 변수 이름 수정
rename(mtcars, MPG = mpg)

# 새로운 변수의 추가
mutate(mtcars, 
       kml = mpg * 0.43, 
       gp_kml = if_else(kml >= 10, "good", "bad"))

transmute(mtcars, 
          kml = mpg * 0.43, 
          gp_kml = if_else(kml >= 10, "good", "bad"))

# 그룹 생성 및 그룹 별 자료 요약
summarise(mtcars, avg_mpg = mean(mpg))
summarise(mtcars, n = n(), n_mpg = n_distinct(mpg), avg_mpg = mean(mpg), sd_mpg = sd(mpg))

by_cyl <- group_by(mtcars, cyl)
by_cyl
summarise(by_cyl, n = n(), n_mpg = n_distinct(mpg), avg_mpg = mean(mpg), sd_mpg = sd(mpg))

# Pipe 기능
mtcars %>% 
  group_by(cyl) %>% 
  summarise(n = n(), n_mpg = n_distinct(mpg), avg_mpg = mean(mpg), sd_mpg = sd(mpg))

mtcars %>% 
  mutate(kml = mpg * 0.43,
         gp_kml = if_else(kml >= 10, "good", "bad")) %>% 
  select(mpg, kml, gp_kml, everything()) %>% 
  filter(gear >= 4) %>% 
  group_by(cyl, gp_kml) %>% 
  summarise(n = n(),
            avg_mpg = mean(mpg),
            avg_kml = mean(kml))






#######################
#### 연습 문제
#######################
# Girth 값이 평균 이상이고, Height 값이 평균 미만인 데이터를 선택하여, trees_sub1에 저장. 변수는 Girth, Height 만 선택.
trees_sub1 <- trees %>% 
  filter(Girth >= mean(Girth), 
         Height < mean(Height)) %>% 
  select(Girth, Height)

# Girth 값이 평균 미만이고, Height 값이 평균 이상인 데이터를 선택하여, trees_sub2에 저장. 변수는 Girth, Height 만 선택.
trees_sub2 <- trees %>% 
  filter(Girth < mean(Girth), 
         Height >= mean(Height)) %>% 
  select(Girth, Height)

# trees_sub1과 trees_sub2의 Girth와 Height의 평균값 및 케이스 값 계산.
trees_sub1 %>%
  summarise(n = n(),
            avg_Girth = mean(Girth),
            avg_Height = mean(Height))
trees_sub2 %>%
  summarise(n = n(),
            avg_Girth = mean(Girth),
            avg_Height = mean(Height))

# mtcars 데이터에서 disp는 세제곱인치 단위의 배기량이므로 cc 단위의 배기량으로 변환 (1세제곱인치 = 16.44cc)하여 disp_cc 생성, cyl에 따라 구분되는 자동차 대수, mpg, disp_cc, hp, wt의 평균값 출력.
mtcars %>% 
  mutate(disp_cc = disp * 16.44) %>% 
  group_by(cyl) %>% 
  summarise(n = n(),
            avg_mpg = mean(mpg),
            avg_disp_cc = mean(disp_cc),
            avg_hp = mean(hp),
            avg_wt = mean(wt))

## 국내 총인구 데이터
# 열 (Column) 이름 변경
colnames(popData) <- c("NOT", "Ages", paste(rep(c("1944", "1949", "1955", "1960", "1966", "1970", "1975", "1980", "1985", "1990", "1995", "2000", "2005", "2010"), each = 3), rep(c("Total", "Men", "Women"), 14), sep="_"))
# 첫째 행 제거, 첫째 열 제거
popData <- popData[-1,]
popData <- popData[,-1]
