install.packages("tidyr")
library(tidyr)

# gather로 tidy 데이터 만들기
table1

table4a

table4a %>% 
  gather(key = year, value = cases, `1999`, `2000`)

table4a %>% 
  gather(key = year, value = cases, `1999`, `2000`) %>% 
  arrange(country)

# spread로 tidy 데이터 만들기
table2

table2 %>% 
  spread(key = type, value = count)

# separate로 tidy 데이터 만들기
table3

table3 %>% 
  separate(col = rate, into = c("cases", "population"))

table3 %>% 
  separate(col = rate, into = c("cases", "population"),
           sep = 4)

table3 %>% 
  separate(col = rate, into = c("cases", "population"),
           convert = TRUE)

# unite로 데이터 다루기
table5

table5 %>% 
  unite(col = year, century, year)

table5 %>% 
  unite(col = year, century, year, sep="") %>% 
  separate(col = rate, into = c("cases", "population"),
           convert = TRUE) %>% 
  mutate(year = as.integer(year))

# Mutating joins
band_members
band_instruments

band_members %>% 
  inner_join(band_instruments)

band_members %>% 
  left_join(band_instruments, by = "name")

band_members %>% 
  right_join(band_instruments, by = "name")

band_members %>% 
  full_join(band_instruments, by = "name")

band_instruments2
band_members %>% 
  full_join(band_instruments2, by = c("name" = "artist"))

# Filtering joins
band_members
band_instruments

band_members %>% 
  inner_join(band_instruments)

band_members %>% 
  semi_join(band_instruments)

band_members %>% 
  anti_join(band_instruments, by = "name")

# 단순 수평 및 수직 결합
df_x <- tibble(x1 = letters[1:3],
               x2 = 1:3)
df_y <- tibble(y1 = LETTERS[4:6],
               y2 = 4:6)

bind_cols(df_x, df_y)

df_z <- tibble(x1 = LETTERS[4:6],
               x2 = 4:6)

bind_rows(df_x, df_z)






#######################
#### 연습 문제
#######################
# 다음의 두 데이터 프레임 생성
part_df <- tibble(num = c(155, 501, 244, 796),
                  tool = c("screwdrive", "pliers", "wrench", "hammer"))
part_df

order_df <- tibble(num = c(155, 796, 155, 244, 244, 796, 244),
                   name = c("Par", "Fox", "Smith", "White", "Crus", "White", "Lee"))
order_df

# 다음의 결과가 되도록 데이터 프레임 결함
part_df %>% left_join(order_df, by = "num")
part_df %>% inner_join(order_df, by = "num")

## 다음의 두 데이터 프레임 생성
jan <- tibble(reg = c("NE", "SW", "W"),
              tempjan = c(34, 60, 55))
jan

sep <- tibble(reg = c("NE", "SW", "W"),
              tempsep = c(65, 88, 72))
sep

# 다음의 결과가 나오도록 결합
jan %>% left_join(sep, by = "reg")

# 다음의 결과가 나오도록 결합 및 변형
jan %>% left_join(sep, by = "reg") %>% 
  rename(jan = tempjan, sep = tempsep) %>% 
  gather(key = Month, value = Temp, jan, sep)

## 국내 총인구 데이터
popData2010 <- popData %>% 
  select("Ages", "2010_Total", "2010_Men", "2010_Women") %>% 
  gather(key = Key, value = Population, `2010_Total`, `2010_Men`, `2010_Women`) %>% 
  separate(col = Key, into = c("Year", "Key"), sep = "_") %>% 
  mutate(Population = as.numeric(Population)) %>% 
  arrange(Ages)

popData2000 <- popData %>% 
  select("Ages", "2000_Total", "2000_Men", "2000_Women") %>% 
  gather(key = Key, value = Population, `2000_Total`, `2000_Men`, `2000_Women`) %>% 
  separate(col = Key, into = c("Year", "Key"), sep = "_") %>% 
  mutate(Population = as.numeric(Population)) %>% 
  arrange(Ages)

popDataTotal <- popData %>% 
  filter(Ages == "계") %>% 
  select(ends_with("Total")) %>% 
  gather(key = Year, value = Population) %>% 
  separate(col = Year, into = c("Year", "Key"), sep = "_")%>% 
  mutate(Population = as.numeric(Population))
