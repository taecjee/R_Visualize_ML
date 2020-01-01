# 1차원 데이터
vec1 <- scan("data/data1.txt")
vec1

# 2차원 데이터
df1 <- read.table("data/data2.txt")
df1
class(df1$V2)

df1 <- read.table("data/data2.txt", stringsAsFactors = FALSE)
df1
class(df1$V2)

df2 <- read.table("data/data3.txt", header = TRUE, stringsAsFactors = FALSE)
df2

df3 <- read.table("data/data4.txt", stringsAsFactors = FALSE)
df3

df4 <- read.table("data/data5.txt", header = TRUE, na.strings=".", stringsAsFactors = FALSE)
df4

df5 <- read.table("data/data6.txt", header = TRUE, sep = ",", stringsAsFactors = FALSE)
df5

df6 <- read.csv("data/data6.txt", stringsAsFactors = FALSE)
df6

file1 <- file.choose()
file1

# 텍스트 파일 저장
head(trees)

write.table(trees, "data/out1.txt")
write.table(trees, "data/out2.txt", quote = FALSE)
write.table(trees, "data/out3.txt", quote = FALSE, row.names = FALSE)
write.table(trees, "data/out4.txt", quote = FALSE, row.names = FALSE, sep = ",")
write.csv(trees, "data/out1.csv")
write.csv(trees, "data/out2.csv", quote = FALSE, row.names = FALSE)

# readr 패키지 활용
install.packages("readr")
library(readr)

read_csv("data/data6.txt")

# Excel 파일 불러오기
install.packages("xlsx")
install.packages("readxl")

library(xlsx)
library(readxl)

xls_file <- system.file("tests", "test_import.xlsx", package = "xlsx")
xls1 <- read.xlsx(xls_file, sheetIndex = 1)
head(xls1)

xls2 <- read.xlsx(xls_file, sheetIndex = 1, rowIndex = 1:5, colIndex = 1:2)
xls2

xls3 <- read_excel(xls_file)
head(xls3)

# SAS 파일 불러오기
install.packages("haven")
library(haven)

sas_file <- system.file("examples", "iris.sas7bdat", package = "haven")
sas1 <- read_sas(sas_file)
head(sas1)

# 웹에서 데이터 불러오기
iris.url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"

iris.web <- read_csv(iris.url)
iris.web

iris.web2 <- read_csv(iris.url, col_names = c("SepalLength", "SepalWidth", "PetalLength", "PetalWedth", "Species"))
iris.web2

# HTML 테이블
install.packages("rvest")
library(rvest)

URL <- "https://en.wikipedia.org/wiki/World_population"
web <- read_html(URL)

tbl <- html_nodes(web, "table")
tbl

tbl1 <- html_table(tbl[6])
tbl1[[1]]






#######################
#### 연습 문제
#######################
# 국내 총인구 데이터 다운로드 및 설정 불러오기
popFile <- "data/Population_20200101155551.xlsx"
popData <- read_excel(popFile)
View(popData)
